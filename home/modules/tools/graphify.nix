# graphify turns a folder of code, docs and images into a queryable knowledge
# graph. nixpkgs builds it with buildPythonApplication, so the library is only
# importable from an interpreter that has it on sys.path. Agent tooling reads
# the CLI's shebang to find that interpreter, which a makeWrapper shell wrapper
# does not expose, hence the hand-written script below.
{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.tools.graphify;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  # mcp is a lazy import inside graphify.serve, so it is not part of the
  # package's own dependency closure.
  pythonEnv = pkgs.unstable.python3.withPackages (ps: [
    ps.mcp
    (ps.toPythonModule pkgs.unstable.graphify)
  ]);

  graphify = pkgs.writeScriptBin "graphify" ''
    #!${pythonEnv}/bin/python3
    import sys
    from graphify.__main__ import main

    sys.exit(main())
  '';

  graphify-python = pkgs.writeShellScriptBin "graphify-python" ''
    exec ${pythonEnv}/bin/python3 "$@"
  '';

  # MCP stdio server over an existing graph.json, for `claude mcp add`.
  graphify-mcp = pkgs.writeShellScriptBin "graphify-mcp" ''
    exec ${pythonEnv}/bin/python3 -m graphify.serve "''${1:-graphify-out/graph.json}"
  '';
in
{
  options.modules.tools.graphify = {
    enable = mylib.mkBoolOpt false;
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ graphify graphify-python graphify-mcp ];
  };
}
