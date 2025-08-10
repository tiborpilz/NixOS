{ inputs, lib, config, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.zk;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.zk = with types; {
    enable = mylib.mkBoolOpt false;
    zkDataPath = mylib.mkOpt str "${config.home.homeDirectory}/zk";
    zkConfigPath = mylib.mkOpt str "${config.xdg.configHome}/zk";
  };

  config = mkIf cfg.enable {
    # write file to $XDG_CONFIG_HOME/zk/template/daily.md
    home.file."${cfg.zkConfigPath}/templates/daily.md" = {
      text = ''
        # {{format-date now}} :daily:
        
        - [ ] 
      '';
    };

    home.file."${cfg.zkConfigPath}/templates/weekly.md" = {
      text = ''
      # Week of {{format-date (date "monday this week") "long"}}

      ## Daily notes in this week
      {{sh "zk list thisweek-daily --format='- {{link}}'"}}
      '';
    };

    programs.zk.enable = true;
    programs.zk.settings = {
      notebook = {
        dir = cfg.zkDataPath;
      };
      note = {
        language = "en";
        default-title = "Untitled";
        filename = "{{id}}-{{slug title}}";
        extension = "md";
        template = "default.md";
        id-charset = "alphanum";
        id-length = 4;
        id-case = "lower";
      };
      extra = {
        author = "Tibor Pilz";
      };
      group.daily = {
        paths = [ "journal/daily" ];
      };
      group.daily.note = {
        filename = "{{format-date now}}";
        extension = "md";
        template = "daily.md";
      };
      group.weekly = {
        paths = [ "journal/weekly" ];
      };
      group.weekly.note = {
        filename = "week-{{format-date (date 'monday this week') '%Y-%m-%d'}}";
        extension = "md";
        template = "weekly.md";
      };
      format.markdown = {
        hashtags = true;
        colonTags = true;
        linkStyle = "markdown";
      };
      tool = {
        editor = "nvim";
        shell = "zsh";
        pager = "less";
        searchEngine = "fzf";
        searchEngineArgs = "--no-multi --preview 'cat {}' --preview-window=up:30%:wrap";
      };
      filter = {
        recents = "--sort created- --created-after 'last two weeks'";
        thisweek-daily = "--sort created+ journal/daily --created-after 'monday this week' --created-before 'monday next week'";
      };
      alias = {
        daily = "zk new --no-input \"$ZK_NOTEBOOK_DIR/journal/daily\"";
        weekly = "zk new --no-input \"$ZK_NOTEBOOK_DIR/journal/weekly\"";
        refresh-weekly = ''
          file="$1"
          tmpfile=$(mktemp)
          awk '
          /<!-- daily-links:start -->/ { print; system("zk list thisweek-daily --working-dir journal/weekly --format \"- {{link}}\""); next }
          /<!-- daily-links:end -->/ { print; next }
          { print }
          ' "$file" > "$tmpfile" && mv "$tmpfile" "$file"
        '';
      };

      lsp = {
        diagnostics = {
          wikiTitle = "hint";
          deadLink = "error";
        };
      };
    };

    modules.shell.zsh.envInit = ''
      export ZK_NOTEBOOK_DIR=${cfg.zkDataPath}
    '';
  };
}

