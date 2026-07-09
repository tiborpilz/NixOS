# Deploying with deploy-rs over Netmaker

Klaus is deployed with [deploy-rs](https://github.com/serokell/deploy-rs) over
plain SSH. Because Klaus sits on the home LAN behind NAT, CI runners reach it by
joining the existing **Netmaker** mesh for the duration of the job — no bastion,
no Cloudflare, no port-forwarding.

```
GitHub Actions runner
  └─ netclient join  ──▶  Netmaker mesh (WireGuard)  ──▶  Klaus
       └─ ssh root@klaus  (ssh.config -> KLAUS_NETMAKER_IP)
            └─ deploy-rs builds on Klaus (remoteBuild) and activates
```

## What lives in this repo

| Piece | File |
| --- | --- |
| deploy-rs input + `deploy.nodes.klaus` | `flake.nix` |
| root trusts the deploy key | `hosts/nixos/klaus/default.nix`, `hosts/nixos/klaus/deploy.pub` |
| "Deploy" GitHub Action: push to main + manual (joins the mesh, deploys, leaves) | `.github/workflows/deploy.yml` |
| Local convenience recipe | `Justfile` (`just deploy-rs`) |

The deploy node's `hostname` is just `klaus`; SSH resolves it to Klaus's mesh IP
via `~/.ssh/config` (CI writes that from the `KLAUS_NETMAKER_IP` variable).

## One-time setup

### 1. Deploy keypair

Generate a dedicated keypair (not a personal key):

```sh
ssh-keygen -t ed25519 -C "github-actions-deploy@klaus" -f deploy_key -N ""
```

- Put `deploy_key.pub` into `hosts/nixos/klaus/deploy.pub` (replace the placeholder).
- Put the private `deploy_key` into the GitHub repo secret `DEPLOY_SSH_KEY`.
- Delete the local copies afterwards.

### 2. Netmaker enrollment key

In the Netmaker dashboard (`dashboard.tiborpilz.xyz`) create an **enrollment
key** for the network Klaus is on — reusable, or with a use cap — and copy the
`netclient join -t <token>` token.

Also confirm:
- **ACLs** let a new node reach Klaus on `:22` (default networks allow all).
- A **relay/egress** exists so the deploy still connects when direct NAT
  hole-punching fails (see caveats).

### 3. GitHub repo secrets & variables

| Kind | Name | Value |
| --- | --- | --- |
| secret | `DEPLOY_SSH_KEY` | private half of the deploy keypair |
| secret | `NETMAKER_ENROLLMENT_TOKEN` | the `netclient join -t` token |
| secret | `CACHIX_AUTH_TOKEN` | already present |
| variable | `KLAUS_NETMAKER_IP` | Klaus's address on the mesh (not secret) |

### 4. Bootstrap once over the LAN/VPN

root doesn't trust the deploy key until this config is on Klaus. Do the first
switch through the existing channel:

```sh
just deploy klaus
```

After that, CI deploys work.

## Deploying

### From GitHub

Every **push to main** deploys Klaus (mode `switch`). Concurrent deploys of the
same node queue rather than overlap; stacked-up pushes collapse to the newest.

For manual runs (including `dry-activate`): Actions tab → **Deploy** → **Run
workflow** → pick `node` and `mode`. Either way the job joins the mesh,
deploys, and leaves.

### Locally

Point `klaus` at the mesh IP (or LAN IP) in `~/.ssh/config`:

```
Host klaus
  HostName 10.x.x.x   # Klaus's Netmaker (or LAN) address
  User root
```

Then:

```sh
just deploy-rs klaus                 # activate
just deploy-rs klaus --dry-activate  # build + preview only
```

## Notes & caveats

- **`magicRollback` is on.** If the new config leaves Klaus unreachable,
  deploy-rs automatically rolls back.
- **remoteBuild.** The build runs on Klaus (mirrors the old `--build-host =
  klaus`), so the runner only evaluates and copies derivations.
- **netclient version must match the server** (currently `v0.26.0`, set in
  `.github/workflows/deploy.yml` as `NETCLIENT_VERSION`). Bump both together.
- **NAT traversal.** Runner and Klaus are both behind NAT. Netmaker hole-punches
  directly when it can; when it can't it falls back to a relay — which in this
  setup runs on `edge`. So the "no edge" path holds only for direct connections.
- **No ephemeral cleanup.** Unlike some meshes, Netmaker doesn't auto-expire
  nodes. The workflow's `leave`/`uninstall` step deregisters the runner, but a
  hard-killed run can orphan a node — prune stale nodes in the dashboard.
- The old `just deploy <host>` (`nixos-rebuild --target-host`) path is untouched
  and still works over the LAN/VPN.
