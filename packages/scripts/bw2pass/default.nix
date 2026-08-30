{ pkgs, ... }:

pkgs.stdenvNoCC.mkDerivation {
  pname = "bw2pass";
  version = "2.0.0";
  dontUnpack = true;

  nativeBuildInputs = [ pkgs.makeWrapper ];
  nativeCheckInputs = [
    pkgs.bash
    pkgs.coreutils
    pkgs.findutils
    pkgs.gnugrep
    pkgs.jq
  ];
  doCheck = true;

  checkPhase = ''
    runHook preCheck
    ${pkgs.bash}/bin/bash ${./test.sh} ${./bw2pass.sh}
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall

    install -Dm755 ${./bw2pass.sh} $out/bin/bw2pass
    wrapProgram $out/bin/bw2pass \
      --prefix PATH : ${pkgs.lib.makeBinPath [
        pkgs.bash
        pkgs.bitwarden-cli
        pkgs.coreutils
        pkgs.jq
        pkgs.pass
      ]}

    runHook postInstall
  '';
}
