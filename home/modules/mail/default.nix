{ inputs, lib, pkgs, config, ... }:

with lib;
let
  cfg = config.modules.mail;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  passwordCommand = entry:
    let
      slug = replaceStrings [ "/" "." ] [ "-" "-" ] entry;
      script = pkgs.writeShellScript "mail-password-${slug}" ''
        export PASSWORD_STORE_DIR=${config.programs.password-store.settings.PASSWORD_STORE_DIR}
        ${config.programs.password-store.package}/bin/pass show ${escapeShellArg entry} \
          | ${pkgs.coreutils}/bin/head -n1
      '';
    in
    [ "${script}" ];

  common = {
    realName = "Tibor Pilz";
    mbsync = {
      enable = true;
      create = "maildir";
      expunge = "both";
    };
    msmtp.enable = true;
    mu.enable = true;
    aerc.enable = true;
  };
in
{
  options.modules.mail = {
    enable = mylib.mkBoolOpt false;

    maildir = mylib.mkOpt types.str "${config.home.homeDirectory}/Mail";

    autoSync = mylib.mkBoolOpt false;
    syncInterval = mylib.mkOpt types.str "*:0/5";

    ionos.passEntry = mylib.mkOpt types.str "bitwarden/mail.ionos.de";
    gmail.passEntry = mylib.mkOpt types.str "bitwarden/gmail-app-password";
  };

  config = mkIf cfg.enable {
    accounts.email.maildirBasePath = cfg.maildir;

    accounts.email.accounts = {
      ionos = common // {
        primary = true;
        address = "tibor@pilz.berlin";
        userName = "tibor@pilz.berlin";
        passwordCommand = passwordCommand cfg.ionos.passEntry;
        maildir.path = "pilz.berlin";
        imap = {
          host = "imap.ionos.de";
          port = 993;
          tls.enable = true;
        };
        smtp = {
          host = "smtp.ionos.de";
          port = 465;
          tls.enable = true;
        };
      };

      gmail = common // {
        primary = false;
        flavor = "gmail.com";
        address = "tbrpilz@googlemail.com";
        userName = "tbrpilz@googlemail.com";
        passwordCommand = passwordCommand cfg.gmail.passEntry;
        maildir.path = "gmail";
        mbsync = common.mbsync // {
          patterns = [ "*" "![Gmail]/All Mail" "![Gmail]/Important" "![Gmail]/Starred" ];
        };
      };
    };

    programs.mbsync.enable = true;
    programs.msmtp.enable = true;
    programs.mu.enable = true;
    programs.aerc = {
      enable = true;
      extraConfig.general.unsafe-accounts-conf = true;
    };

    services.mbsync = mkIf (cfg.autoSync && pkgs.stdenv.isLinux) {
      enable = true;
      frequency = cfg.syncInterval;
    };
  };
}
