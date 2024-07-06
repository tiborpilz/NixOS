{ lib, pkgs, inputs, config, ... }:
with lib;
let
  cfg = config.modules.editors.lsp;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.editors.lsp = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      # Markdown langauge server
      marksman
    ];
    xdg.configFile."marksman/config.toml".text = ''
      [core]
      markdown.file_extensions = ["md", "markdown"]
      text_sync = "full"

      [code_action]
      toc.enable = true
      create_missing_file.enable = true

      [completion]
      wiki.style = "title-slug"
    '';
  };
}
