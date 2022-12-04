{ inputs, pkgs, lib, ... }:
let taskFileSkeleton = lib.replaceStrings ["\"" "\n"] ["\\\"" ""] ''
    {
      "version": "3",
      "tasks": {
        "default": {
          "cmd": [{ "task": "${pkgs.nix}/bin/nix flake check" }]
        }
      }
    }
'';
in pkgs.writeShellScriptBin "generateTaskFile" ''
  base='{ "version": "3" }'
  defaultTask='{ "default": { "cmds": ["${pkgs.nix}/bin/nix flake check"] } }'
  nixosMachines=$(${pkgs.nix}/bin/nix flake show --json | ${pkgs.jq}/bin/jq -r '.nixosConfigurations | keys')
  nixosTasks=$(echo $nixosMachines | jq -c 'map({ key: ("build-" + .), value: { cmds: [("nix build .#nixosConfigurations." + . + ".config.system.build.toplevel") ] } }) | from_entries')
  tasks=$(echo "[$defaultTask, $nixosTasks]" | jq '.[0] * .[1]')
  taskJSON=$(echo "[$base, { \"tasks\": $tasks }]" | jq '.[0] * .[1]')
  echo $taskJSON | ${pkgs.yq}/bin/yq -y > Taskfile.yml
''
