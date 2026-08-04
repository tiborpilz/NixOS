# Amnesiac -- a Tails-like amnesic Tor live system.
#
# Self-contained: the image carries its own system definition instead of
# installing one of ./hosts, so nothing from ./modules or ./hosts is merged in.
#
#   nix build .#isos.amnesiac
{
  kind = "live";
  modules = [ ./system.nix ];
}
