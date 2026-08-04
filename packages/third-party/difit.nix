{ pkgs, lib, ... }:

let
  # Lockfile is v9, and pnpm 11 gets SIGKILLed on darwin at the end of the fetch
  pnpm = pkgs.pnpm_10;
in
pkgs.stdenv.mkDerivation (finalAttrs: {
  pname = "difit";
  version = "5.0.8";

  src = pkgs.fetchFromGitHub {
    owner = "yoshiko-pg";
    repo = "difit";
    tag = "v${finalAttrs.version}";
    hash = "sha256-AT2dUT14+yfMLxcJdJC/CI28RfyElsoa97vxUIMjUo0=";
  };

  # The repo is a workspace; packages/vscode is a separate extension we don't want.
  pnpmWorkspaces = [ "difit" ];

  pnpmDeps = pkgs.fetchPnpmDeps {
    inherit (finalAttrs)
      pname
      version
      src
      pnpmWorkspaces
      ;
    inherit pnpm;
    fetcherVersion = 3;
    hash = "sha256-b+1Q64ftMhTqaq9iveYSpPwmTakKXhaKJW69kPkcueo=";
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

    # Re-resolve node_modules without the build-only deps
    rm -rf node_modules
    pnpm install --force --offline --production --ignore-scripts --filter=difit

    mkdir -p $out/lib/node_modules/difit $out/bin
    mv ./dist ./node_modules ./package.json $out/lib/node_modules/difit

    ln -s $out/lib/node_modules/difit/dist/cli/index.js $out/bin/difit
    chmod +x $out/lib/node_modules/difit/dist/cli/index.js
    patchShebangs $out/lib/node_modules/difit/dist/cli/index.js

    runHook postInstall
  '';

  meta = with lib; {
    description = "Review local git diffs in a GitHub-style browser UI, with comments exportable to agents";
    homepage = "https://github.com/yoshiko-pg/difit";
    changelog = "https://github.com/yoshiko-pg/difit/releases/tag/v${finalAttrs.version}";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "difit";
  };
})
