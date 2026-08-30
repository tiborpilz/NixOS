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

  imapnotifyFor = channel: {
    enable = true;
    boxes = [ "INBOX" ];
    onNotify = "${config.programs.mbsync.package}/bin/mbsync ${channel}";
    onNotifyPost = "${config.programs.mu.package}/bin/mu index";
  };

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
    pushSync = mylib.mkBoolOpt true;
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
        imapnotify = imapnotifyFor "ionos";
        # The mailbox is German-localised; the English folders exist but are
        # empty, so sending against them would hide mail from IONOS webmail.
        folders = {
          sent = "Gesendete Objekte";
          drafts = "Entwürfe";
          trash = "Papierkorb";
        };
        mbsync = common.mbsync // {
          # IONOS also advertises English Sent and Trash, but they are not
          # selectable - opening them is an error, so keep them out of the run.
          patterns = [ "*" "!Sent" "!Trash" ];
        };
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
        # Doom's mu4e +gmail flag decides per message whether Gmail semantics
        # apply by matching "gmail" against the root maildir, so the directory
        # name is load-bearing.
        maildir.path = "gmail";
        imapnotify = imapnotifyFor "gmail";
        # googlemail.com addresses get the localised "[Google Mail]" special
        # folder namespace rather than "[Gmail]".
        folders = {
          sent = "[Google Mail]/Sent Mail";
          drafts = "[Google Mail]/Drafts";
          trash = "[Google Mail]/Trash";
        };
        mbsync = common.mbsync // {
          # Every message also appears under All Mail once per label, which
          # otherwise lands in the maildir two or three times over.
          patterns = [
            "*"
            "![Google Mail]/All Mail"
            "![Google Mail]/Important"
            "![Google Mail]/Starred"
            "![Google Mail]/Spam"
          ];
        };
      };
    };

    services.imapnotify.enable = cfg.pushSync;

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
