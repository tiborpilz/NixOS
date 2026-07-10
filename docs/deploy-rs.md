# Deploying with deploy-rs

Klaus is deployed with [deploy-rs](https://github.com/serokell/deploy-rs). The
deploy is plain SSH under the hood; `sshd` on Klaus authenticates the same
deploy key regardless of how the runner gets there. There are two transports,
picked with the **transport** input on the "Deploy" workflow:

- **`cloudflare`** (default) — reach Klaus's `sshd` through the existing
  **Cloudflare Tunnel**, gated by Cloudflare Access.
- **`tailscale`** — the runner joins the tailnet as an ephemeral node and SSHes
  to Klaus directly. Independent of the tunnel, so a deploy that restarts
  cloudflared can't drop the in-flight session (see Notes & caveats).

Both work from anywhere (CI or a laptop) without being on the LAN or the
Netmaker VPN.

```
Cloudflare transport:
GitHub Actions runner
  └─ ssh root@ssh.tiborpilz.xyz
       └─ ProxyCommand: cloudflared access ssh   ── Cloudflare Access (service token)
            └─ Cloudflare Tunnel  ──▶  Klaus  ssh://localhost:22
                 └─ deploy-rs builds on Klaus (remoteBuild) and activates

Tailscale transport:
GitHub Actions runner  ── tailscale up (OAuth, tag:ci, ephemeral) ──┐
  └─ ssh root@<klaus tailnet IP>  ─────── tailnet ──────────────────┘  Klaus  ssh://100.x.y.z:22
                 └─ deploy-rs builds on Klaus (remoteBuild) and activates
```

## What lives in this repo

| Piece | File |
| --- | --- |
| deploy-rs input + `deploy.nodes.klaus` + deploy checks | `flake.nix` |
| SSH ingress on the Cloudflare Tunnel (`ssh.tiborpilz.xyz` → `ssh://localhost:22`) | `modules/nixos/services/reverseProxy.nix` (`ssh.enable`) |
| `ssh.enable = true` + root's deploy key | `hosts/nixos/klaus/default.nix`, `hosts/nixos/klaus/deploy.pub` |
| Tailscale on Klaus + `tailscale_auth_key` sops secret | `hosts/nixos/klaus/default.nix` |
| Manual "Deploy" GitHub Action (transport: cloudflare \| tailscale) | `.github/workflows/deploy.yml` |
| Local convenience recipe | `Justfile` (`just deploy-rs`) |

The SSH ingress is written in cloudflared submodule form (`{ service = ...; }`)
while the web catch-all stays a plain string, because the nixpkgs cloudflared
module emits attrset-valued ingress rules *before* string-valued ones — and
cloudflared matches ingress top-down, first match wins. That ordering is what
keeps `ssh.tiborpilz.xyz` from being swallowed by the `*.tiborpilz.xyz` HTTP
catch-all.

## One-time setup

### 1. Deploy keypair

Generate a dedicated keypair (not a personal key):

```sh
ssh-keygen -t ed25519 -C "github-actions-deploy@klaus" -f deploy_key -N ""
```

- Put `deploy_key.pub` into `hosts/nixos/klaus/deploy.pub` (replace the placeholder).
- Put the private `deploy_key` into the GitHub repo secret `DEPLOY_SSH_KEY`.
- Delete the local copies afterwards.

### 2. Cloudflare Access (Zero Trust dashboard)

The tunnel already routes `*.tiborpilz.xyz`, so `ssh.tiborpilz.xyz` resolves via
the existing wildcard DNS — no new DNS record is normally needed. Then:

1. **Access → Service Auth → Create a service token.** Note the Client ID and
   Client Secret.
2. **Access → Applications → Add → Self-hosted**, application domain
   `ssh.tiborpilz.xyz`.
3. Add a policy with **Action: Service Auth**, Include → *Service Token* → the
   token from step 1. (Add a second, normal Allow policy for your own email if
   you also want interactive browser SSH.)

### 3. Tailscale (only for the `tailscale` transport)

Skip this if you only use the Cloudflare transport.

**Klaus's auth key.** In the Tailscale admin console → **Settings → Keys →
Generate auth key**: make it **reusable** and **pre-approved** (and non-ephemeral
— Klaus is a server, it should stay in the tailnet when offline). Optionally
attach a tag such as `tag:server`. Then add it to Klaus's sops secrets under the
key `tailscale_auth_key`:

```sh
sops hosts/nixos/klaus/secrets/secrets.yaml
# add a top-level line:
#   tailscale_auth_key: tskey-auth-xxxxxxxxxxxx
```

`services.tailscale.authKeyFile` (in `hosts/nixos/klaus/default.nix`) reads it on
the next switch and runs `tailscale up` once. Klaus then appears in the tailnet
with MagicDNS name `klaus`. After it joins, open Klaus in the admin console →
**⋯ → Disable key expiry** so the server doesn't get logged out periodically.

**Runner OAuth client.** The workflow brings the runner onto the tailnet as an
ephemeral, tagged node using an OAuth client (not a personal auth key):

1. **Access controls (ACL):** make sure `tag:ci` exists under `tagOwners`, e.g.
   `"tagOwners": { "tag:ci": ["autogroup:admin"] }`. If your ACL restricts
   traffic, also allow `tag:ci` to reach Klaus on tcp/22.
2. **Settings → OAuth clients → Generate OAuth client** with the
   **Devices → Core → write** scope and the `tag:ci` tag. Note the Client ID and
   Client Secret.

### 4. GitHub repo secrets

| Secret | Value | Needed for |
| --- | --- | --- |
| `DEPLOY_SSH_KEY` | private half of the deploy keypair | both |
| `CF_ACCESS_CLIENT_ID` | Cloudflare Access service token Client ID | cloudflare |
| `CF_ACCESS_CLIENT_SECRET` | Cloudflare Access service token Client Secret | cloudflare |
| `TS_OAUTH_CLIENT_ID` | Tailscale OAuth client ID | tailscale |
| `TS_OAUTH_SECRET` | Tailscale OAuth client secret | tailscale |
| `CACHIX_AUTH_TOKEN` | already present (used by the build workflow) | both |

### 5. Bootstrap once over the LAN/VPN

Neither transport works until this config is actually on Klaus — the tunnel
doesn't carry SSH and root doesn't trust the deploy key, and Klaus isn't on the
tailnet yet. Do the first switch through the existing channel:

```sh
just deploy klaus
```

After that, Klaus trusts the deploy key, exposes `ssh.tiborpilz.xyz`, and joins
the tailnet, so CI deploys work over either transport.

## Deploying

### From GitHub (manual)

Actions tab → **Deploy** → **Run workflow** → pick `node` (`klaus`), `mode`
(`switch` or `dry-activate`), and `transport` (`cloudflare` or `tailscale`).
The `tailscale` transport overrides the node hostname to the `klaus` ssh alias
(resolved to Klaus's tailnet IP), so it skips the tunnel entirely.

### Locally

Add a `~/.ssh/config` block so SSH tunnels through Cloudflare (browser auth, no
service token needed):

```
Host ssh.tiborpilz.xyz
  User root
  ProxyCommand cloudflared access ssh --hostname %h
```

Then:

```sh
just deploy-rs klaus                 # activate
just deploy-rs klaus --dry-activate  # build + preview only
```

Over Tailscale (once you're on the tailnet) it's just deploy-rs against Klaus's
tailnet address, no ssh config needed:

```sh
just deploy-rs klaus --hostname klaus
```

## Notes & caveats

- **`magicRollback` is on.** If the new config leaves Klaus unreachable,
  deploy-rs automatically rolls back — the main safety win over the old
  `nixos-rebuild --target-host` path.
- **remoteBuild.** The build runs on Klaus (mirrors the old `--build-host =
  klaus`), so the CI runner only evaluates and copies derivations.
- **Deploys that change the tunnel/reverse-proxy config restart cloudflared**
  and can drop the in-flight SSH session on the `cloudflare` transport. That's
  rare; if a deploy dies mid-way for that reason, re-run it, use the `tailscale`
  transport (which the cloudflared restart can't affect), or apply those
  specific changes over the LAN/Netmaker with `just deploy klaus`.
- **Tailscale transport is a separate network path.** It doesn't ride the
  tunnel, so reverse-proxy/cloudflared changes don't interrupt it — but a deploy
  that breaks Klaus's own networking (or Tailscale itself) can still cut it off.
  `magicRollback` still covers that case.
- The old `just deploy <host>` (`nixos-rebuild --target-host`) path is
  untouched and still works over the LAN/VPN.
