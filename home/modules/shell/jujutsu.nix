{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.jujutsu;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  # The log views in home/config/jj are Deno scripts without remote imports.
  jjScript = name: perms: deps: pkgs.writeShellApplication {
    inherit name;
    runtimeInputs = [ pkgs.deno ] ++ deps;
    text = ''exec deno run --quiet --no-prompt ${lib.escapeShellArgs perms} ${../../config/jj + "/${name}.ts"} "$@"'';
  };

  # Diff args shared by the jjui previews; jjui substitutes $preview_width.
  difftPreview = ''merge-tools.difft.diff-args=["--color=always","--width=$preview_width","$left","$right"]'';
in
{
  options.modules.shell.jujutsu = with types; {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      unstable.jjui
      unstable.gg-jj # desktop GUI
      unstable.radicle-node
      radicle-explorer
      difftastic
      (jjScript "jj-pretty" [ "--allow-run" "--allow-env" "--allow-read" "--allow-write" ] [ less ])
      (jjScript "jj-dag" [ "--allow-run" "--allow-env" "--allow-write" ] [ graphviz ])
      (jjScript "jj-pulse" [ "--allow-run" ] [ ])
    ];

    programs.jujutsu = {
      enable = true;
      package = pkgs.unstable.jujutsu;
      # Log templates, node glyphs and colors; `jj lo` and `jj ll` come from here too.
      settings = mkMerge [
        (import ../../config/jj/log-templates.nix)
        {
          user = {
            name = "Tibor Pilz";
            email = "tibor@pilz.berlin";
          };

          # git signs every commit; signing on push instead of on every snapshot avoids
          # an ssh-keygen call per working-copy change.
          signing = {
            behavior = "drop";
            backend = "ssh";
            key = "~/.ssh/id.pub";
            backends.ssh.allowed-signers = "~/.ssh/allowed_signers";
          };
          git.sign-on-push = true;

          ui = {
            diff-editor = [ "nvim" "-c" "DiffEditor $left $right $output" ];
            # hunk.nvim would list JJ-INSTRUCTIONS as a changed file
            diff-instructions = false;
            # Syntax-aware diffs for show/diff/log -p; `--git` still gives a plain patch.
            diff-formatter = "difft";
            # less shows Private Use Area characters, like the Nerd Font glyphs in the log
            # templates, as <U+XXXX> unless they are marked printable.
            pager = {
              command = [ "less" "-FRXK" ];
              env = {
                LESSCHARSET = "utf-8";
                LESSUTFCHARDEF = "E000-F8FF:p,F0000-FFFFD:p";
              };
            };
          };

          "--scope" = [
            {
              "--when".repositories = [ "~/Code/liqid" ];
              user.email = "tibor.pilz@liqid.de";
              signing.key = "~/.ssh/id_liqid.pub";
            }
            {
              # jj.nvim finds commits in its log buffer by the builtin node glyphs.
              # Neovim sets $NVIM for everything it spawns.
              "--when".environments = [ "NVIM" ];
              templates.log_node = "builtin_log_node";
            }
          ];

          revset-aliases = {
            "closest_bookmark(to)" = "heads(::to & bookmarks())";
            # Nearest commit that can be pushed: described, and not the empty working copy.
            "closest_pushable(to)" = ''heads(::to & mutable() & ~description(exact:"") & (~empty() | merges()))'';
            # The git branch the current work sits on.
            "base" = "closest_bookmark(@)";
          };

          aliases = {
            base = [ "log" "-r" "base" ];
            stack = [ "log" "-r" "base::@" ];
            # The whole branch since trunk, its fork point, and trunk itself.
            l = [ "log" "-r" "(trunk()..@):: | (trunk()..@)- | trunk()" ];
            wip = [ "log" "-r" "mine() & mutable()" ];
            retrunk = [ "rebase" "-d" "trunk()" "--skip-emptied" ];
            sync = [ "util" "exec" "--" "bash" "-c" "jj git fetch && jj retrunk" "" ];
            push = [ "git" "push" ];
            pr = [
              "util"
              "exec"
              "--"
              "bash"
              "-c"
              ''
                [ -n "$1" ] || { echo "usage: jj pr BRANCH" >&2; exit 2; }
                jj git push --named "$1=closest_pushable(@)"
              ''
              ""
            ];
            # Registers a secondary workspace as a git worktree of the backing repo, so
            # git-based tools (codediff, gitsigns) work in it. jj then treats it as
            # colocated and keeps its HEAD at @-, like in the main workspace.
            colocate = [
              "util"
              "exec"
              "--"
              "bash"
              "-c"
              ''
                set -eu
                root=$(jj root)
                gitdir=$(jj git root)
                wt="$gitdir/worktrees/$(basename "$root")"
                if [ -e "$root/.git" ] || [ -e "$wt" ]; then
                  echo "$root is already colocated, or $wt exists" >&2
                  exit 1
                fi
                cur=$(jj log --no-graph -r @ -T change_id)
                mkdir -p "$wt"
                echo "$root/.git" > "$wt/gitdir"
                echo ../.. > "$wt/commondir"
                # A HEAD jj hasn't recorded gets imported, which moves @ onto a fresh
                # commit. Start unborn, then step off @ and back so jj exports HEAD
                # and the index itself.
                echo "ref: refs/heads/jj-unborn" > "$wt/HEAD"
                echo "gitdir: $wt" > "$root/.git"
                echo '/*' > "$root/.jj/.gitignore"
                jj new --quiet
                jj edit --quiet "$cur"
              ''
              ""
            ];
            # Trunk is excluded so a feature bookmark sharing a commit with main doesn't drag main along.
            tug = [ "bookmark" "move" "--from" "closest_bookmark(@)" "--to" "closest_pushable(@)" ''~regex:"^(main|master)$"'' ];
            # Script views from home/config/jj
            pretty = [ "util" "exec" "--" "jj-pretty" ];
            cards = [ "util" "exec" "--" "jj-pretty" "stack" ];
            dag = [ "util" "exec" "--" "jj-dag" "--show" ];
            pulse = [ "util" "exec" "--" "jj-pulse" ];
          };
        }
      ];
    };

    programs.jjui = {
      enable = true;
      package = pkgs.unstable.jjui;
      settings = {
        ui = {
          theme = { dark = "pretty"; light = "default"; };
          set_window_title = "base";
        };
        revisions.revset = "(trunk()..@):: | (trunk()..@)- | trunk() | @";
        preview = {
          show_at_start = true;
          width_percentage = 55.0;
          revision_command = [ "show" "--color" "always" "-r" "$change_id" "--tool" "difft" "--config" difftPreview ];
          file_command = [ "diff" "--color" "always" "-r" "$change_id" "--tool" "difft" "--config" difftPreview "$file" ];
        };
        actions = [{
          name = "open on github";
          # git_web_url() turns the origin remote into its web URL
          lua = ''
            local id = context.commit_id()
            exec_shell("xdg-open \"$(jj log --no-graph -r " .. id .. " -T 'git_web_url()')/commit/" .. id .. "\"")
          '';
        }];
        bindings = [{ action = "open on github"; key = "O"; scope = "revisions"; desc = "open commit on forge"; }];
      };
    };

    home.file."${config.programs.jjui.configDir}/themes/pretty.toml".source = (pkgs.formats.toml { }).generate "pretty.toml" {
      colors = {
        "status title" = { fg = "black"; bg = "blue"; bold = true; };
        title = { fg = "blue"; bold = true; };
        shortcut = "blue";
      };
      dark = {
        background_blend = 0.3;
        colors = {
          ":selected" = { bg = "#3b4252"; };
          "revisions details:selected" = { bg = "#3b4252"; bold = true; };
        };
      };
    };
  };
}
