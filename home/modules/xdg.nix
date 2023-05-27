{ config, ... }:
{
  xdg.enable = true;

  home.sessionVariables = {
    XDG_CONFIG_HOME = "${config.xdg.configHome}";
    XDG_CACHE_HOME = "${config.xdg.cacheHome}";
    XDG_DATA_HOME = "${config.xdg.dataHome}";
    # XDG_BIN_HOME = "${config.xdg.home}/.local/bin";
  };
}
