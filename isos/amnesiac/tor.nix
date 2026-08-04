# Tor kill switch: every packet either goes through Tor or does not go.
#
# A nat/output chain redirects application traffic into Tor's TransPort and DNS
# into its DNSPort; a filter/output chain with a drop policy catches whatever
# the nat chain did not rewrite, so protocols Tor cannot carry (raw UDP, ICMP,
# QUIC) fail closed rather than leaking in the clear.
{ config, ... }:
let
  transPort = 9040;
  dnsPort = 9053;
  socksPort = 9050;
  # Numeric, not `meta skuid "tor"`: the build-time `nft -c` check runs in a
  # sandbox with no such user and would reject the name.
  torUid = toString config.users.users.tor.uid;
in
{
  services.tor = {
    enable = true;
    client.enable = true;
    torsocks.enable = true;
    settings = {
      TransPort = [{ addr = "127.0.0.1"; port = transPort; }];
      DNSPort = [{ addr = "127.0.0.1"; port = dnsPort; }];
      # Virtual IPs for .onion names, which have no routable address of their
      # own for the redirected TCP to reach.
      VirtualAddrNetworkIPv4 = "10.192.0.0/10";
      AutomapHostsOnResolve = true;
      AutomapHostsSuffixes = [ ".onion" ".exit" ];
      ClientOnly = true;
      AvoidDiskWrites = 1; # keep Tor's state in RAM
    };
  };

  # Resolution goes to 127.0.0.1:53, which the nat chain rewrites to Tor's
  # DNSPort. Nothing actually listens on :53.
  networking.nameservers = [ "127.0.0.1" ];

  # The stock firewall would add a second set of hooks alongside these; one
  # explicit ruleset beats two interacting ones.
  networking.firewall.enable = false;
  networking.nftables = {
    enable = true;
    ruleset = ''
      table ip tor-nat {
        chain output {
          type nat hook output priority dstnat; policy accept;

          # Tor's own traffic must leave untouched or it would loop.
          meta skuid ${torUid} return

          # DNS first: this has to win over the loopback exemption below,
          # since the resolver address is 127.0.0.1.
          meta l4proto { tcp, udp } th dport 53 redirect to :${toString dnsPort}

          ip daddr 127.0.0.0/8 return

          meta l4proto tcp redirect to :${toString transPort}
        }
      }

      table inet tor-filter {
        chain input {
          type filter hook input priority filter; policy drop;
          iif lo accept
          ct state established,related accept
          udp sport { 67, 68 } accept
        }

        chain forward {
          type filter hook forward priority filter; policy drop;
        }

        chain output {
          type filter hook output priority filter; policy drop;

          # Redirected packets are now addressed to 127.0.0.1 and leave via lo.
          oif lo accept
          meta skuid ${torUid} accept
          ct state established,related accept

          # DHCP, so NetworkManager can get a lease at all.
          udp dport { 67, 68 } accept
        }
      }
    '';
  };

  # Point Tor Browser at the system daemon so its bundled copy is not a second,
  # unfiltered circuit.
  environment.sessionVariables = {
    TOR_SKIP_LAUNCH = "1";
    TOR_SKIP_CONTROLPORT = "1";
    TOR_SOCKS_HOST = "127.0.0.1";
    TOR_SOCKS_PORT = toString socksPort;
    # Anything else that honours the convention (curl, git via socks, ...).
    ALL_PROXY = "socks5h://127.0.0.1:${toString socksPort}";
  };
}
