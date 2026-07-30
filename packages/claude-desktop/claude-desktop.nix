{ lib
, stdenv
, fetchurl
, dpkg
, autoPatchelfHook
, addDriverRunpath
, makeWrapper
, wrapGAppsHook3
, writeShellApplication
, curl
, gawk
, gnused
, nix
, alsa-lib
, at-spi2-atk
, at-spi2-core
, atk
, cairo
, cups
, dbus
, expat
, glib
, gsettings-desktop-schemas
, gtk3
, libcap_ng
, libGL
, libdrm
, libgbm
, libglvnd
, libseccomp
, libnotify
, libpulseaudio
, libsecret
, libuuid
, libxkbcommon
, nspr
, nss
, pango
, pipewire
, systemd
, libx11
, libxcb
, libxcomposite
, libxdamage
, libxext
, libxfixes
, libxrandr
, libxscrnsaver
, libxtst
  # Runtime helpers put on the app's PATH. Claude Code runs inside the desktop
  # app and shells out to these; MCP servers commonly need npx/uvx/docker.
, bash
, coreutils
, docker
, git
, nodejs
, qemu
, ripgrep
, uv
, xdg-utils
}:

let
  pname = "claude-desktop";
  version = "1.24012.9";

  # Anthropic's apt repo. The pool URL is stable and every published version
  # keeps its own file, so this is a proper immutable source. The hashes below
  # are the SHA256 values from the repository's own Packages index:
  #   https://downloads.claude.ai/claude-desktop/apt/stable/dists/stable/main/binary-<arch>/Packages
  # `passthru.updateScript` below re-reads that index to bump version + hashes.
  sources = {
    x86_64-linux = {
      debArch = "amd64";
      hash = "sha256-MC5tII3YyOnlIGfaoo7zsRcaFhNYb9DhC+3GQiJbbuE=";
    };
    aarch64-linux = {
      debArch = "arm64";
      hash = "sha256-Gpvhd7BjNluS5SL+3RnIfa9uvJKp9xEhvf9ynifeQIw=";
    };
  };

  source = sources.${stdenv.hostPlatform.system}
    or (throw "${pname}: unsupported system ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  inherit pname version;

  src = fetchurl {
    url = "https://downloads.claude.ai/claude-desktop/apt/stable/pool/main/c/claude-desktop/claude-desktop_${version}_${source.debArch}.deb";
    inherit (source) hash;
  };

  nativeBuildInputs = [ dpkg autoPatchelfHook makeWrapper wrapGAppsHook3 ];

  # The .deb bundles its own Electron (42.x) rather than linking a system one,
  # so we keep that and only patch its ELF interpreter/rpaths. Swapping in
  # nixpkgs' electron is what makes the community repack brittle across
  # updates - the app ships native helpers built against its exact Electron.
  buildInputs = [
    alsa-lib
    at-spi2-atk
    at-spi2-core
    atk
    cairo
    cups
    dbus
    expat
    glib
    gsettings-desktop-schemas # otherwise GSettings lookups abort at startup
    gtk3
    libcap_ng # resources/virtiofsd (Cowork VM)
    libseccomp # resources/virtiofsd (Cowork VM)
    libdrm
    libgbm
    libnotify
    libsecret
    libuuid
    libxkbcommon
    nspr
    nss
    pango
    stdenv.cc.cc.lib
    libx11
    libxcb
    libxcomposite
    libxdamage
    libxext
    libxfixes
    libxrandr
    libxscrnsaver
    libxtst
  ];

  # dlopen()ed at runtime rather than DT_NEEDED, so autoPatchelf has to be told
  # about them explicitly.
  runtimeDependencies = [
    libGL
    libglvnd
    libpulseaudio
    pipewire
    (lib.getLib systemd)
  ];

  unpackCmd = "dpkg-deb -x $curSrc source";
  sourceRoot = "source";

  dontConfigure = true;
  dontBuild = true;

  # `dontWrapGApps` + manual `${gappsWrapperArgs[@]}`: wrapGAppsHook3 would
  # otherwise wrap the raw Electron binary in usr/lib, and we want a single
  # wrapper that also carries our PATH additions.
  dontWrapGApps = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib $out/share
    cp -r usr/lib/claude-desktop $out/lib/claude-desktop
    cp -r usr/share/applications $out/share/applications
    cp -r usr/share/icons $out/share/icons

    # The bundled SUID sandbox helper cannot be setuid in the store. Electron
    # only consults it when unprivileged user namespaces are unavailable, and
    # NixOS enables those, so the namespace sandbox is used instead. Drop the
    # helper so Electron doesn't find a non-setuid one and abort.
    rm -f $out/lib/claude-desktop/chrome-sandbox

    runHook postInstall
  '';

  # In preFixup, not installPhase: wrapGAppsHook3 fills gappsWrapperArgs from a
  # preFixup hook, so during installPhase the array is still empty and the
  # GSettings schema path never makes it into the wrapper.
  preFixup = ''
    # ANGLE dlopen()s the *native* libEGL.so.1 by soname, which no RPATH we can
    # set on the bundled libs covers. Without this the GPU process dies with
    # "Could not dlopen native EGL" and falls back to software rendering.
    # libglvnd supplies the dispatch library; driverLink (/run/opengl-driver/lib)
    # supplies the actual vendor driver it dispatches to.
    makeWrapper $out/lib/claude-desktop/claude-desktop $out/bin/claude-desktop \
      "''${gappsWrapperArgs[@]}" \
      --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [ libglvnd ]}:${addDriverRunpath.driverLink}/lib \
      --suffix PATH : ${lib.makeBinPath [ bash coreutils git nodejs ripgrep uv docker qemu xdg-utils ]}
  '';

  # `nix run .#claude-desktop.updateScript` from the repo root bumps version and
  # both hashes in place. Everything it needs is published in the apt index, so
  # no downloading of 160 MB debs just to learn their hashes.
  passthru.updateScript = writeShellApplication {
    name = "update-claude-desktop";
    runtimeInputs = [ curl gnused gawk nix ];
    text = ''
      nix_file="''${1:-packages/claude-desktop/claude-desktop.nix}"
      base=https://downloads.claude.ai/claude-desktop/apt/stable

      if [ ! -w "$nix_file" ]; then
        echo "not writable: $nix_file (run from the repo root, or pass the path)" >&2
        exit 1
      fi

      index() { curl -fsSL "$base/dists/stable/main/binary-$1/Packages"; }

      amd64_index=$(index amd64)
      arm64_index=$(index arm64)

      latest=$(printf '%s' "$amd64_index" | sed -n 's/^Version: //p' | sort -V | tail -n1)
      # Read the current version out of the file being edited rather than using
      # the version baked into this script, so it stays correct if the script is
      # older than the checkout (and so it can be pointed at a copy).
      current=$(sed -n 's|^  version = "\(.*\)";|\1|p' "$nix_file")
      echo "current: $current  latest: $latest"
      if [ "$latest" = "$current" ]; then
        echo "already up to date"
        exit 0
      fi

      for arch in amd64 arm64; do
        if [ "$arch" = amd64 ]; then idx=$amd64_index; else idx=$arm64_index; fi
        # Stanzas are blank-line separated; pull the SHA256 out of the one whose
        # Version matches. arm64 may briefly lag amd64, so fail loudly if absent.
        sha=$(printf '%s' "$idx" | awk -v v="$latest" '
          /^Version: / { cur = $2 }
          /^SHA256: /  { if (cur == v) print $2 }' | tail -n1)
        if [ -z "$sha" ]; then
          echo "no $arch package published for $latest yet; not bumping" >&2
          exit 1
        fi
        sri=$(nix hash convert --hash-algo sha256 --to sri "$sha")
        echo "  $arch  $sri"
        # The hash always sits on the line after its arch's debArch line.
        sed -i "/debArch = \"$arch\";/{n;s|hash = \".*\";|hash = \"$sri\";|;}" "$nix_file"
      done

      sed -i "s|^  version = \".*\";|  version = \"$latest\";|" "$nix_file"
      echo "bumped to $latest"
    '';
  };

  meta = {
    description = "Official Claude desktop app for Linux (beta)";
    homepage = "https://code.claude.com/docs/en/desktop-linux";
    downloadPage = "https://claude.com/download";
    license = lib.licenses.unfree;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    platforms = lib.attrNames sources;
    mainProgram = "claude-desktop";
  };
}
