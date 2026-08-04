{ pkgs, lib, ... }:

let
  pnpm = pkgs.pnpm_10;
in
pkgs.stdenv.mkDerivation (finalAttrs: {
  pname = "diffx";
  version = "0.16.0";

  src = pkgs.fetchFromGitHub {
    owner = "wong2";
    repo = "diffx";
    tag = "v${finalAttrs.version}";
    hash = "sha256-tNmc4eTnLp6BMn4800y1iokvqIAQ7lkkuhpl0jnv6w8=";
  };

  pnpmDeps = pkgs.fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    inherit pnpm;
    fetcherVersion = 3;
    hash = "sha256-+R5ZDL16iLG7iodrCICwnGSP6QJ+6g/2yBK7h0rhG6U=";
  };

  nativeBuildInputs = [
    pkgs.nodejs
    pkgs.pnpmConfigHook
    pnpm
  ];

  buildInputs = [ pkgs.nodejs ];

  buildPhase = ''
    runHook preBuild

    pnpm run build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    # `open` and `get-port` are deliberately not bundled, so keep runtime deps around
    rm -rf node_modules
    pnpm install --force --offline --production --ignore-scripts

    mkdir -p $out/lib/node_modules/diffx $out/bin
    mv ./dist ./node_modules ./package.json $out/lib/node_modules/diffx

    ln -s $out/lib/node_modules/diffx/dist/cli.mjs $out/bin/diffx
    chmod +x $out/lib/node_modules/diffx/dist/cli.mjs
    patchShebangs $out/lib/node_modules/diffx/dist/cli.mjs

    runHook postInstall
  '';

  meta = with lib; {
    description = "Browser-based diff review UI built around handing review comments to a coding agent";
    homepage = "https://github.com/wong2/diffx";
    changelog = "https://github.com/wong2/diffx/releases/tag/v${finalAttrs.version}";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "diffx";
  };
})
