{ config, lib, pkgs, ... }:

with lib;
with lib.my;

let
  cfg = config.modules.services.voice;
in
{
  # Fully-local Home Assistant voice pipeline compute. Home Assistant itself runs
  # elsewhere (192.168.1.175) and reaches these over the LAN via the Wyoming
  # protocol / Ollama HTTP API. The GTX 1080 accelerates the LLM; Whisper stays
  # on CPU so the 8 GB of VRAM is spent entirely on the conversation agent.
  options.modules.services.voice = {
    enable = mkBoolOpt false;

    whisperModel = mkOpt' types.str "Systran/faster-distil-whisper-small.en" ''
      faster-whisper model for speech-to-text. Runs on CPU; the distil-small.en
      model is a large accuracy upgrade over the tiny-int8 default while staying
      fast enough for short voice commands.
    '';

    piperVoice = mkOpt' types.str "en_US-lessac-medium" ''
      Piper text-to-speech voice. Voices are downloaded automatically on first use.
    '';

    llmModel = mkOpt' types.str "qwen2.5:7b" ''
      Ollama model used as the conversation agent. A q4 7-8B model (~5 GB) fits the
      1080's 8 GB VRAM. Alternatives: "llama3.1:8b", or "llama3.2:3b" for lower latency.
    '';

    wakeword = mkBoolOpt true;

    # Optional cloud gateway: a LiteLLM proxy that fronts a cheap hosted model
    # (DeepSeek) as the primary conversation agent and falls back to the local
    # Ollama model when the cloud is unreachable or the budget is exhausted.
    gateway = {
      enable = mkBoolOpt false;

      port = mkOpt' types.port 4000 "Port for the LiteLLM OpenAI-compatible gateway.";

      deepseekModel = mkOpt' types.str "deepseek-v4-flash" ''
        DeepSeek model id served as the primary agent. "deepseek-v4-flash" is the
        cheap non-thinking model (the old "deepseek-chat" alias is deprecated).
      '';

      monthlyBudget = mkOpt' types.int 15 ''
        Global spend cap in USD per 30 days. Enforced in-process by LiteLLM;
        DeepSeek's prepaid balance is the ultimate hard cap. When DeepSeek is over
        budget / out of balance it errors, which triggers the local Ollama fallback.
      '';

      environmentFile = mkOpt' (types.nullOr types.path) null ''
        Env file providing DEEPSEEK_API_KEY and LITELLM_MASTER_KEY (e.g. a sops
        secret path). Home Assistant authenticates to the gateway with the master key.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = !cfg.gateway.enable || cfg.gateway.environmentFile != null;
      message = ''
        modules.services.voice.gateway.enable requires gateway.environmentFile to be
        set (an env file with DEEPSEEK_API_KEY and LITELLM_MASTER_KEY).
      '';
    }];

    # Speech-to-text (Wyoming). CPU by design — see module comment above.
    services.wyoming.faster-whisper.servers.ha = {
      enable = true;
      model = cfg.whisperModel;
      language = "en";
      device = "cpu";
      uri = "tcp://0.0.0.0:10300";
    };

    # Text-to-speech (Wyoming).
    services.wyoming.piper.servers.ha = {
      enable = true;
      voice = cfg.piperVoice;
      uri = "tcp://0.0.0.0:10200";
    };

    # Central wake-word detection (Wyoming). Useful for mic-only satellites. The
    # active wake word (e.g. "ok_nabu") is chosen per-pipeline in Home Assistant;
    # since wyoming-openwakeword 2.0 the models are no longer preloaded here.
    services.wyoming.openwakeword = mkIf cfg.wakeword {
      enable = true;
      uri = "tcp://0.0.0.0:10400";
    };

    # Local LLM conversation agent, GPU-accelerated on the 1080.
    services.ollama = {
      enable = true;
      package = pkgs.ollama-cuda;
      # Bind to all interfaces so Home Assistant (192.168.1.175) can reach it;
      # klaus runs with the firewall disabled so no port needs opening.
      host = "0.0.0.0";
      loadModels = [ cfg.llmModel ];
      # Keep the model resident — this box is a dedicated assistant, and reloading
      # a multi-GB model on every request adds seconds of latency.
      environmentVariables.OLLAMA_KEEP_ALIVE = "-1";
    };

    # Expose Ollama on the local domain only (ollama.<localDomain>), for browser
    # access / monitoring. Home Assistant still talks to it directly on :11434;
    # localOnly keeps it out of the public Cloudflare tunnel.
    modules.services.reverseProxy.proxies.ollama = {
      publicPort = 11434;
      localOnly = true;
      auth = false;
    };

    # LiteLLM gateway: one OpenAI-compatible endpoint fronting DeepSeek (primary)
    # and the local Ollama model (fallback), with a global monthly budget cap.
    # Home Assistant points its OpenAI-compatible conversation agent at
    # http://<klaus>:<port>/v1, model "assistant", api key = LITELLM_MASTER_KEY.
    services.litellm = mkIf cfg.gateway.enable {
      enable = true;
      host = "0.0.0.0";               # reachable by Home Assistant on the LAN (firewall off)
      port = cfg.gateway.port;
      environmentFile = cfg.gateway.environmentFile;
      settings = {
        model_list = [
          {
            model_name = "assistant";
            litellm_params = {
              model = "deepseek/${cfg.gateway.deepseekModel}";
              api_key = "os.environ/DEEPSEEK_API_KEY";
              # Pin costs so budget tracking is correct even if the model id is not
              # in LiteLLM's registry: $0.14/M in, $0.28/M out.
              input_cost_per_token = 0.00000014;
              output_cost_per_token = 0.00000028;
            };
          }
          {
            model_name = "assistant-local";
            litellm_params = {
              model = "ollama_chat/${cfg.llmModel}";
              api_base = "http://127.0.0.1:11434";
            };
          }
        ];
        router_settings = {
          # Cloud is primary; any error (outage, or DeepSeek refusing once the
          # prepaid balance is spent) fails over to the local model.
          fallbacks = [{ "assistant" = [ "assistant-local" ]; }];
        };
        litellm_settings = {
          drop_params = true;          # HA may send params a backend rejects
          max_budget = cfg.gateway.monthlyBudget;
          budget_duration = "30d";
        };
        general_settings = {
          master_key = "os.environ/LITELLM_MASTER_KEY";
        };
      };
    };

    modules.services.reverseProxy.proxies.litellm = mkIf cfg.gateway.enable {
      publicPort = cfg.gateway.port;
      localOnly = true;
      auth = false;
    };
  };
}
