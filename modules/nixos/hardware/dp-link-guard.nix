# Keeps a flapping DisplayPort output from stalling the whole desktop.
{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.hardware.dpLinkGuard;

  guard = pkgs.writeShellApplication {
    name = "dp-link-guard";
    runtimeInputs = with pkgs; [ systemd gnugrep coreutils ];
    text = ''
      output=${escapeShellArg cfg.output}
      settle=${toString cfg.settleSeconds}

      # Card index is not stable across boots, so resolve the connector now.
      connector=""
      for c in /sys/class/drm/card*-"$output"; do
        [ -e "$c/status" ] && connector="$c" && break
      done
      if [ -z "$connector" ]; then
        echo "no drm connector for $output; nothing to guard"
        exit 0
      fi
      echo "guarding $connector (settle ''${settle}s)"

      parked=0

      park() {
        # Forcing the connector off makes it disappear for every compositor,
        # greeter included -- unlike a kscreen-doctor disable, which only ever
        # applies inside one user session.
        echo off > "$connector/status"
        parked=1
        echo "link failing: parked $output"
      }

      unpark() {
        # `detect` clears the force and re-probes. If the hub is back this
        # restores the real mode list; if not, the retries resume and the next
        # error parks it again.
        echo detect > "$connector/status"
        parked=0
        echo "quiet for ''${settle}s: released $output"
      }

      # Only the *first* error of a burst matters; the rest just keep the timer
      # alive. `read -t` returning non-zero is the quiet signal.
      while true; do
        if read -r -t "$settle" _line; then
          [ "$parked" -eq 0 ] && park
        else
          [ "$parked" -eq 1 ] && unpark
        fi
      done < <(journalctl -k -f -o cat --since now \
                 | grep --line-buffered -E ${escapeShellArg cfg.errorPattern})
    '';
  };
in
{
  options.modules.hardware.dpLinkGuard = {
    enable = mkBoolOpt false;

    output = mkOpt types.str "DP-1";

    # Long enough to ride out a full hub re-enumeration (the worst measured
    # outage was 119s, but the retries stop as soon as the link comes back).
    settleSeconds = mkOpt types.int 20;

    errorPattern = mkOpt types.str "enabling link [0-9]+ failed|core_link_write_dpcd";
  };

  config = mkIf cfg.enable {
    systemd.services.dp-link-guard = {
      description = "Park ${cfg.output} while its DP link is failing";
      wantedBy = [ "multi-user.target" ];
      # The connector has to exist before we can resolve it.
      after = [ "systemd-udev-settle.service" ];
      serviceConfig = {
        ExecStart = getExe guard;
        Restart = "always";
        RestartSec = 5;
      };
    };
  };
}
