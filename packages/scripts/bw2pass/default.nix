{ pkgs, ... }:
with pkgs;
stdenv.mkDerivation rec {
  name = "bw2pass";
  phases = "installPhase";
  src = ./bw2pass.sh;
  buildInputs = [ bash coreutils jq bitwarden-cli ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/bw2pass
      wrapProgram $out/bin/bw2pass \
        --prefix PATH : ${lib.makeBinPath [ bash jq ]}
    '';
}
