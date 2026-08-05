# mermaid-cli (mmdc) to render mermaid diagrams to svg/pdf/png.
{ lib, pkgs, inputs, config, ... }:
with lib;
let
  cfg = config.modules.tools.mermaid;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  wrapped = pkgs.runCommand "mermaid-cli-wrapped"
    { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
    mkdir -p $out/bin
    makeWrapper ${pkgs.mermaid-cli}/bin/mmdc $out/bin/mmdc \
      --set-default PUPPETEER_EXECUTABLE_PATH ${escapeShellArg cfg.chromePath}
  '';
in
{
  options.modules.tools.mermaid = {
    enable = mylib.mkBoolOpt false;
    chromePath = mylib.mkOpt types.str
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
  };

  config = mkIf cfg.enable {
    home.packages = [
      (if pkgs.stdenv.isDarwin then wrapped else pkgs.mermaid-cli)
    ];
  };
}
