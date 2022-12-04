{ pkgs, ... }:
pkgs.writeShellScriptBin "repl" ''
  confnix=$(mktemp)
  echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
  trap "rm $confnix" EXIT
  nix repl $confnix
''
