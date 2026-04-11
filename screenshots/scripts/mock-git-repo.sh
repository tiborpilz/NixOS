#!/usr/bin/env bash
set -euo pipefail

MOCK_DIR="/tmp/showcase"

# Clean up previous runs
rm -rf "$MOCK_DIR"
mkdir -p "$MOCK_DIR"
cd "$MOCK_DIR"

git init
git checkout -b main

# --- Sample files ---

cat > config.nix << 'NIXEOF'
{ config, pkgs, lib, ... }:

let
  # Nord color palette for terminal applications
  colors = {
    nord0  = "#2E3440"; nord1  = "#3B4252";
    nord2  = "#434C5E"; nord3  = "#4C566A";
    nord4  = "#D8DEE9"; nord5  = "#E5E9F0";
    nord6  = "#ECEFF4"; nord7  = "#8FBCBB";
    nord8  = "#88C0D0"; nord9  = "#81A1C1";
    nord10 = "#5E81AC"; nord11 = "#BF616A";
    nord12 = "#D08770"; nord13 = "#EBCB8B";
    nord14 = "#A3BE8C"; nord15 = "#B48EAD";
  };

  mkService = name: port: {
    enable = true;
    description = "${name} web service";
    listenPort = port;
    environment = {
      RUST_LOG = "info";
      DATABASE_URL = "postgres://localhost/${name}";
    };
  };
in
{
  imports = [
    ./hardware.nix
    ./networking.nix
  ];

  # System packages available globally
  environment.systemPackages = with pkgs; [
    neovim
    git
    ripgrep
    fd
    bat
    fzf
    tmux
    kitty
  ];

  # Enable services
  services.api = mkService "api" 8080;
  services.web = mkService "web" 3000;

  # Firewall configuration
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 80 443 8080 3000 ];
  };
}
NIXEOF

cat > server.lua << 'LUAEOF'
local M = {}

--- Configure routes for the HTTP server.
-- @param app The application instance
-- @return table Route configuration
function M.setup(app)
  local routes = {}
  local middleware = require("middleware")

  -- Health check endpoint
  routes["/health"] = function(req, res)
    res:json({ status = "ok", uptime = os.clock() })
  end

  -- User endpoints with authentication
  routes["/api/users"] = middleware.auth(function(req, res)
    local db = require("db")
    local users = db.query("SELECT id, name, email FROM users LIMIT 50")

    local result = {}
    for _, user in ipairs(users) do
      table.insert(result, {
        id = user.id,
        name = user.name,
        avatar = string.format("/avatars/%s.png", user.id),
      })
    end

    res:json({ data = result, total = #result })
  end)

  -- WebSocket handler for live updates
  routes["/ws"] = function(req, res)
    local ws = res:upgrade()
    ws:on("message", function(msg)
      local payload = vim.json.decode(msg)
      if payload.type == "subscribe" then
        app.pubsub:subscribe(payload.channel, ws)
      end
    end)
  end

  return routes
end

return M
LUAEOF

cat > notes.org << 'ORGEOF'
#+title: Project Notes
#+author: Tibor Pilz
#+startup: overview

* TODO Migrate authentication to OAuth2          :security:backend:
DEADLINE: <2026-04-15 Wed>
:PROPERTIES:
:EFFORT: 3d
:ASSIGNEE: tibor
:END:

The current session-based auth needs to be replaced with OAuth2.
Key considerations:
- Token refresh strategy
- Backward compatibility with existing sessions

** DONE Research OAuth2 providers
CLOSED: [2026-04-01 Wed 14:30]
- [X] Auth0
- [X] Keycloak
- [X] Custom implementation

** IN-PROGRESS Implement token service
:LOGBOOK:
- State "IN-PROGRESS" from "TODO" [2026-04-05 Sat 09:00]
:END:

#+begin_src rust
pub struct TokenService {
    signing_key: Ed25519Key,
    expiry: Duration,
}

impl TokenService {
    pub fn issue(&self, claims: Claims) -> Result<Token> {
        let header = Header::new(Algorithm::EdDSA);
        encode(&header, &claims, &self.signing_key)
    }
}
#+end_src

* DONE Set up monitoring dashboard                :ops:monitoring:
CLOSED: [2026-03-28 Sat 11:00]

Grafana dashboards for:
| Metric          | Source     | Alert Threshold |
|-----------------+------------+-----------------|
| Request latency | Prometheus | > 200ms p99     |
| Error rate      | Loki       | > 1% 5min       |
| CPU usage       | Node exp.  | > 80% 10min     |

* Resources
- [[https://nixos.org/manual][NixOS Manual]]
- [[https://nix.dev][Nix Developer Guide]]
ORGEOF

cat > deploy.sh << 'SHEOF'
#!/usr/bin/env bash
set -euo pipefail

# Deployment script for NixOS configurations
ENVIRONMENT="${1:-staging}"
FLAKE_REF="${2:-.}"
LOG_FILE="/var/log/deploy-$(date +%Y%m%d-%H%M%S).log"

declare -A SERVERS=(
  [staging]="10.0.1.10"
  [production]="10.0.1.20"
  [monitoring]="10.0.1.30"
)

log() { echo "[$(date '+%H:%M:%S')] $*" | tee -a "$LOG_FILE"; }

deploy_host() {
  local host="$1" addr="$2"
  log "Deploying to ${host} (${addr})..."

  if nixos-rebuild switch \
    --flake "${FLAKE_REF}#${host}" \
    --target-host "root@${addr}" \
    --build-host "root@${addr}" \
    --fast 2>&1 | tee -a "$LOG_FILE"; then
    log "SUCCESS: ${host} deployed"
  else
    log "FAILED: ${host} deployment"
    return 1
  fi
}

# Pre-flight checks
if [[ ! -f "flake.nix" ]]; then
  log "ERROR: No flake.nix found in current directory"
  exit 1
fi

log "Starting deployment to ${ENVIRONMENT}"
for host in "${!SERVERS[@]}"; do
  if [[ "$host" == "$ENVIRONMENT"* ]]; then
    deploy_host "$host" "${SERVERS[$host]}"
  fi
done

log "Deployment complete"
SHEOF

cat > README.md << 'MDEOF'
# NixOS Configuration

Personal NixOS and home-manager configurations with Nord theme.

## Structure

- `hosts/` - Machine-specific configurations
- `home/` - Home-manager modules
- `overlays/` - Nixpkgs overlays
- `lib/` - Shared library functions
MDEOF

cat > flake.nix << 'FLAKEEOF'
{
  description = "NixOS configuration";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }: {
    nixosConfigurations.default = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./config.nix ];
    };
  };
}
FLAKEEOF

mkdir -p hosts lib overlays
echo '{ }' > hosts/default.nix
echo '{ lib, ... }: { }' > lib/default.nix
echo '[ ]' > overlays/default.nix

# --- Git history ---

git add -A
git commit -m "Initial commit: NixOS configuration scaffold"

git checkout -b feat/nord-theme
echo '# Nord theme integration notes' > THEME.md
git add THEME.md
git commit -m "feat: add Nord theme integration notes"

# Add a second commit on this branch
cat >> config.nix << 'EOF'

# Nord terminal colors
programs.kitty.themeFile = "Nord";
EOF
git add config.nix
git commit -m "feat: configure kitty with Nord theme"

git checkout -b fix/prompt-colors main
sed -i.bak 's/nord11 = "#BF616A"/nord11 = "#BF616A"  # Aurora red/' config.nix
rm -f config.nix.bak
git add config.nix
git commit -m "fix: annotate aurora red color value"

git checkout main

# Make the working tree dirty for realistic prompt display
echo "# WIP: adding new host configuration" >> hosts/default.nix
echo '{ enable = true; }' > hosts/laptop.nix

echo "Mock git repo created at $MOCK_DIR"
