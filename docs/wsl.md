# Running your NixOS config in WSL

Your flake ships a `wsl` host — a full NixOS system that boots under Windows
Subsystem for Linux. Build a rootfs on any Nix machine, import it into Windows,
and manage it exactly like your other hosts.

| | |
|---|---|
| **host** | `wsl` |
| **flake output** | `.#wsl` (`nixosConfigurations.wsl`) |
| **base** | [NixOS-WSL](https://github.com/nix-community/NixOS-WSL) |
| **arch** | `x86_64-linux` |

## What you get

A real NixOS system — not a shell layered on Ubuntu. The kernel comes from WSL;
everything above it is your config.

- **Full NixOS** — systemd, declarative, rollback-able
- **Default user** `tibor` — passwordless sudo (via the nixos-wsl module)
- **Home Manager** — enabled, with `graphical = false` (no desktop pulled in)
- **Caches** — Cachix + garnix already trusted, so rebuilds pull prebuilt paths
- **Not included** — no bootloader, no X11 (WSL boots it, Windows draws the GUI)

The host is intentionally minimal — extend it in `hosts/nixos/wsl/default.nix`
(enable a service, add packages) exactly like any other host.

## Before you start

You need three things:

1. **Windows with WSL2.** Run `wsl --update` in an admin PowerShell to get a
   recent WSL build (the `--from-file` import flag wants WSL 2.4.4+).
2. **A machine with Nix + flakes** to build the rootfs — any NixOS box, macOS
   with Nix, or an existing NixOS-WSL instance. This is your "build host."
3. **This repo checked out** on that build host, on the branch that contains
   the `wsl` host.

## Skip the build: grab the CI image

The [`Build WSL image`](../.github/workflows/wsl.yml) GitHub Actions workflow
builds the tarball on every relevant push to `main` (and on manual dispatch) and
publishes it to a rolling **`wsl-latest`** pre-release. If you just want to run
it on Windows, download `nixos.wsl` from the release and jump to step 2:

```powershell
# stable URL, always the newest build off main
curl.exe -L -o nixos.wsl `
  https://github.com/tiborpilz/NixOS/releases/download/wsl-latest/nixos.wsl
wsl --install --from-file nixos.wsl --name NixOS
```

Every run also uploads the image as a workflow artifact, so older builds stay
retrievable from the Actions run page. Building it yourself (below) is only
needed when you want a change that isn't on `main` yet.

## Install (first time)

A genuine three-step sequence: build → import → launch.

### 1. Build the rootfs tarball

On the build host, from the repo root. The builder needs root because it
assembles a full filesystem image.

```sh
# evaluate + build the WSL rootfs builder
nix build .#nixosConfigurations.wsl.config.system.build.tarballBuilder
sudo ./result/bin/nixos-wsl-tarball-builder   # → ./nixos.wsl
```

Copy the resulting `nixos.wsl` over to the Windows machine.

### 2. Import it into Windows

In PowerShell. This registers a distribution named `NixOS`.

```powershell
wsl --install --from-file nixos.wsl --name NixOS

# older WSL builds without --from-file:
wsl --import NixOS C:\WSL\NixOS nixos.wsl
```

### 3. Launch it

You land in a shell as `tibor`. From here on you never touch the tarball again
— you rebuild in place.

```powershell
wsl -d NixOS
```

## Everyday use

Once you're inside, clone the repo (or use your Windows checkout under
`/mnt/c/…`) and drive it like any NixOS machine.

```sh
# apply config changes
sudo nixos-rebuild switch --flake .#wsl

# pull newer package versions (bumps flake.lock)
nix flake update

# reclaim disk after a while
sudo nix-collect-garbage -d
```

Because the flake already trusts your Cachix and garnix caches, most rebuilds
download prebuilt paths instead of compiling.

## What's in the `wsl` host

`hosts/nixos/wsl/default.nix` is small on purpose — it leans on the NixOS-WSL
module and your shared modules.

```nix
imports = [ inputs.nixos-wsl.nixosModules.default ];

wsl.enable      = true;
wsl.defaultUser = "tibor";

home.enable     = true;
home.graphical  = false;

system.stateVersion = "26.05";
```

Every service module under `modules/nixos/` is still imported but stays off
until you opt in — so the WSL host starts minimal and grows only where you ask.

## Make it yours

**Turn on a service.** The same option-gated modules your servers use are
available here. Enable one in `hosts/nixos/wsl/default.nix` and rebuild. No new
wiring needed.

**GUI apps.** WSLg forwards Linux GUI windows and audio to Windows
automatically, so a graphical app installed via Nix just works. The heavy
desktop environments (Plasma, Hyprland) stay disabled here.

**Keep the tarball reproducible.** Anyone with the flake can rebuild the exact
same image from `.#nixosConfigurations.wsl`. Treat the tarball as disposable
output, not something to back up.

## Two things that will bite you

> **⚠ Flakes only see git-tracked files.** A brand-new file that isn't staged is
> invisible to `nix build` — you'll get *"flake does not provide attribute
> nixosConfigurations.wsl."* If you add a host or module and it won't show up,
> this is almost always why.
>
> Fix: `git add hosts/nixos/wsl/` then rebuild.

> **⚠ The nixos-wsl input needs locking.** If `flake.lock` doesn't yet pin
> `nixos-wsl`, the first build adds it. On a dirty tree Nix may refuse with
> *"cannot write modified lock file."*
>
> Fix: `nix flake lock`, then commit the updated lockfile.

## Alternative: Home-Manager only

Don't want a second full OS? Install Nix on an existing Ubuntu/Debian WSL distro
and layer just your user environment on top:

```sh
nix run home-manager -- switch --flake .#tibor
```

One caveat: `homeConfigurations.tibor` defaults `graphical = true`, which pulls
in Plasma, Hyprland and Firefox — pointless in WSL. A `graphical = false`
variant makes this path lean. The full NixOS-WSL host above avoids the issue
entirely.

## Command reference

| Command | What it does |
|---|---|
| `nix build .#nixosConfigurations.wsl.config.system.build.tarballBuilder` | Build the rootfs builder |
| `sudo ./result/bin/nixos-wsl-tarball-builder` | Produce `nixos.wsl` |
| `wsl --install --from-file nixos.wsl --name NixOS` | Import into Windows |
| `wsl -d NixOS` | Start the distro |
| `sudo nixos-rebuild switch --flake .#wsl` | Apply config changes |
| `nix flake update` | Bump all inputs |
| `nix flake lock` | Pin a newly-added input |
| `wsl --unregister NixOS` | Wipe & start over (Windows) |
