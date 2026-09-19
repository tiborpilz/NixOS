{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.jujutsu;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.jujutsu = with types; {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      unstable.jjui
      unstable.radicle-node
      radicle-explorer
    ];

    programs.jujutsu = {
      enable = true;
      package = pkgs.unstable.jujutsu;
      settings = {
        # Mirrors home/config/git/config, including the liqid override below.
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
        };

        "--scope" = [
          {
            "--when".repositories = [ "~/Code/liqid" ];
            user.email = "tibor.pilz@liqid.de";
            signing.key = "~/.ssh/id_liqid.pub";
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
          pr = [ "util" "exec" "--" "bash" "-c" ''
            [ -n "$1" ] || { echo "usage: jj pr BRANCH" >&2; exit 2; }
            jj git push --named "$1=closest_pushable(@)"
          '' "" ];
          # Trunk is excluded so a feature bookmark sharing a commit with main doesn't drag main along.
          tug = [ "bookmark" "move" "--from" "closest_bookmark(@)" "--to" "closest_pushable(@)" ''~regex:"^(main|master)$"'' ];
        };
      };
    };
  };
}
