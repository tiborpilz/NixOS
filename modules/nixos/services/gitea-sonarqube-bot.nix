{ config, inputs, pkgs, lib, ... }:

# gitea-sonarqube-bot — receives SonarQube + Forgejo webhooks, posts
# quality-gate status as PR comments + status checks on Forgejo PRs.
#
# This is the maintained replacement for SonarQube's missing native
# Gitea/Forgejo PR-decoration support. See
# https://codeberg.org/justusbunsi/gitea-sonarqube-bot.
#
# One-time bootstrap (do AFTER both Forgejo and SonarQube are up):
#
#  1. Create a bot user in Forgejo (e.g. "sonarqube-bot"). Generate a PAT
#     with `read:repository` and `write:issue` scopes (issues = PR
#     comments). Add to sops as `forgejo_sonarqube_bot_token`.
#
#  2. Create a bot user in SonarQube (Administration → Security → Users).
#     Grant "Browse on Project" on every project the bot should report
#     on. Generate a user token. Add to sops as
#     `sonarqube_bot_api_token`.
#
#  3. Generate two random webhook secrets (`openssl rand -hex 24`):
#       forgejo_sonarqube_bot_webhook_secret
#       sonarqube_bot_webhook_secret
#     The bot validates incoming webhooks against these. The same values
#     go into the webhook config on the Forgejo and SonarQube sides.
#
#  4. In SonarQube: Administration → Configuration → Webhooks → Create.
#     URL: https://sonarbot.tiborpilz.xyz/hooks/sonarqube
#     Secret: <sonarqube_bot_webhook_secret value>
#
#  5. In Forgejo (per repo or org): Settings → Webhooks → Add → Gitea.
#     URL: https://sonarbot.tiborpilz.xyz/hooks/gitea
#     Secret: <forgejo_sonarqube_bot_webhook_secret value>
#     Trigger on: Pull Request events.
#
#  6. Add the project mapping to `modules.services.gitea-sonarqube-bot.projects`
#     in the host config (key on SQ side, owner/name on Forgejo side).
#     Rebuild.

with lib;
let
  cfg = config.modules.services.gitea-sonarqube-bot;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  hostname = config.modules.services.reverseProxy.hostname;
  forgejoUrl = "https://forgejo.${hostname}";
  sonarUrl = "https://sonarqube.${hostname}";

  projectMappingYaml = lib.concatMapStringsSep "\n" (p: ''
    - sonarqube:
        key: ${p.sonarKey}
      gitea:
        owner: ${p.giteaOwner}
        name: ${p.giteaRepo}
  '') cfg.projects;

  configContent = ''
    gitea:
      url: ${forgejoUrl}
      token:
        value: ${config.sops.placeholder.forgejo_sonarqube_bot_token}
      webhook:
        secret: ${config.sops.placeholder.forgejo_sonarqube_bot_webhook_secret}
      statusCheck:
        name: "sonarqube"

    sonarqube:
      url: ${sonarUrl}
      token:
        value: ${config.sops.placeholder.sonarqube_bot_api_token}
      webhook:
        secret: ${config.sops.placeholder.sonarqube_bot_webhook_secret}

    projects:
    ${projectMappingYaml}

    namingPattern:
      regex: "^PR-(\\d+)$"
      template: "PR-%d"
  '';
in
with mylib;
{
  options.modules.services.gitea-sonarqube-bot = {
    enable = mkBoolOpt false;

    publicPort = mkOption {
      type = types.int;
      default = 3009;
      description = "Host port the bot's webhook listener is published on.";
    };

    image = mkOption {
      type = types.str;
      default = "docker.io/justusbunsi/gitea-sonarqube-bot:v0.4.0";
    };

    projects = mkOption {
      default = [ ];
      description = ''
        SonarQube ↔ Forgejo project mappings. The bot ignores webhooks
        for projects not listed here. Each entry needs the SonarQube
        projectKey and the Forgejo owner/name.
      '';
      example = [
        {
          sonarKey = "my-go-project";
          giteaOwner = "tibor";
          giteaRepo = "my-go-project";
        }
      ];
      type = types.listOf (types.submodule {
        options = {
          sonarKey = mkOption {
            type = types.str;
            description = "SonarQube projectKey value.";
          };
          giteaOwner = mkOption {
            type = types.str;
            description = "Forgejo repository owner (user or org).";
          };
          giteaRepo = mkOption {
            type = types.str;
            description = "Forgejo repository name (no owner prefix).";
          };
        };
      });
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [{
      assertion = cfg.projects != [ ];
      message = ''
        modules.services.gitea-sonarqube-bot.enable is true but
        .projects is empty. The bot panics at startup without at least
        one mapping. Either set .enable = false until a project is
        ready, or add at least one entry like:
          projects = [{
            sonarKey = "my-project";
            giteaOwner = "tibor";
            giteaRepo = "my-project";
          }];
      '';
    }];

    sops.secrets = {
      forgejo_sonarqube_bot_token = { };
      forgejo_sonarqube_bot_webhook_secret = { };
      sonarqube_bot_api_token = { };
      sonarqube_bot_webhook_secret = { };
    };

    sops.templates."gitea-sonarqube-bot.yaml" = {
      content = configContent;
      mode = "0444";
    };

    virtualisation.quadlet.containers.gitea-sonarqube-bot.containerConfig = {
      image = cfg.image;
      volumes = [
        "${config.sops.templates."gitea-sonarqube-bot.yaml".path}:/home/bot/config/config.yaml:ro"
      ];
      environments = {
        GITEA_SQ_BOT_PORT = "3000";
      };
      publishPorts = [ "${toString cfg.publicPort}:3000" ];
    };

    modules.services.reverseProxy.proxies.sonarbot = {
      publicPort = cfg.publicPort;
      auth = false;  # webhooks are HMAC-validated by the bot itself
    };
  };
}
