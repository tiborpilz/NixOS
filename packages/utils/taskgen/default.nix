{ pkgs, ... }:
pkgs.deno2nix.mkExecutable {
  pname = "taskgen";
  version = "0.0.1";

  src = ./.;
  lockfile = ./lock.json;
  importMap = null;

  entrypoint = "mod.ts";
  additionalDenoFlags = "--allow-run --import-map=import_map.json";
}
