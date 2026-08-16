{ lib, ... }:
with lib;
with lib.my;

{
  options.modules.services.media.music = {
    libraryDir = mkOption {
      type = types.str;
      default = "/data/media/music";
      description = "The music library.";
    };

    downloadsDir = mkOption {
      type = types.str;
      default = "/data/downloads";
      description = "Parent of the per-client download trees.";
    };
  };
}
