{ ... }:
{
  openhab_cfg = {
    services = {
      addons = {
        package = "standard";
        binding = ["deconz"];
        ui = ["basic" "habpanel"];
      };
      runtime = {
        "org.openhab.i18n:language" = "en";
      };
    };
  };
}
