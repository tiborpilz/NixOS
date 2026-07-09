# Deploying with deploy-rs over Cloudflare

Klaus is deployed with [deploy-rs](https://github.com/serokell/deploy-rs). The
deploy is plain SSH under the hood; we reach Klaus's `sshd` through the existing
**Cloudflare Tunnel** so a deploy works from anywhere (CI or a laptop) without
being on the LAN or the Netmaker VPN.

```
GitHub Actions runner
  └─ ssh root@ssh.tiborpilz.xyz
       └─ ProxyCommand: cloudflared access ssh   ── Cloudflare Access (service token)
            └─ Cloudflare Tunnel  ──▶  Klaus  ssh://localhost:22
                 └─ deploy-rs builds on Klaus (remoteBuild) and activates
```

## What lives in this repo

| Piece | File |
| --- | --- |
| deploy-rs input + `deploy.nodes.klaus` + deploy checks | `flake.nix` |
| SSH ingress on the Cloudflare Tunnel (`ssh.tiborpilz.xyz` → `ssh://localhost:22`) | `modules/nixos/services/reverseProxy.nix` (`ssh.enable`) |
| `ssh.enable = true` + root's deploy key | `hosts/nixos/klaus/default.nix`, `hosts/nixos/klaus/deploy.pub` |
| Manual "Deploy" GitHub Action | `.github/workflows/deploy.yml` |
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

### 3. GitHub repo secrets

| Secret | Value |
| --- | --- |
| `DEPLOY_SSH_KEY` | private half of the deploy keypair |
| `CF_ACCESS_CLIENT_ID` | Cloudflare Access service token Client ID |
| `CF_ACCESS_CLIENT_SECRET` | Cloudflare Access service token Client Secret |
| `CACHIX_AUTH_TOKEN` | already present (used by the build workflow) |

### 4. Bootstrap once over the LAN/VPN

The tunnel doesn't carry SSH, and root doesn't trust the deploy key, until this
config is actually on Klaus — a chicken-and-egg. Do the first switch through the
existing channel:

```sh
just deploy klaus
```

After that, Klaus trusts the deploy key and exposes `ssh.tiborpilz.xyz`, so CI
deploys work.

## Deploying

### From GitHub (manual)

Actions tab → **Deploy** → **Run workflow** → pick `node` (`klaus`) and `mode`
(`switch` or `dry-activate`).

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

## Notes & caveats

- **`magicRollback` is on.** If the new config leaves Klaus unreachable,
  deploy-rs automatically rolls back — the main safety win over the old
  `nixos-rebuild --target-host` path.
- **remoteBuild.** The build runs on Klaus (mirrors the old `--build-host =
  klaus`), so the CI runner only evaluates and copies derivations.
- **Deploys that change the tunnel/reverse-proxy config restart cloudflared**
  and can drop the in-flight SSH session. That's rare; if a deploy dies mid-way
  for that reason, re-run it, or apply those specific changes over the
  LAN/Netmaker with `just deploy klaus`.
- The old `just deploy <host>` (`nixos-rebuild --target-host`) path is
  untouched and still works over the LAN/VPN.
