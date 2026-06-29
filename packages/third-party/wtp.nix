{ pkgs, lib, ... }:

pkgs.buildGoModule rec {
  pname = "wtp";
  version = "2.10.3";

  src = pkgs.fetchFromGitHub {
    owner = "satococoa";
    repo = "wtp";
    rev = "v${version}";
    hash = "sha256-KgayKjH4iHi7LgWwk2Laba33bMVZdbiMQgSmqBSTfZ0=";
  };

  vendorHash = "sha256-zsSNo1MQgpvH3ZSd3kmvdIpOCVJgSu1/pYLltx/9dZg=";

  subPackages = [ "cmd/wtp" ];

  # Integration tests require a real git repository and user config.
  doCheck = false;

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  meta = with lib; {
    description = "A powerful Git worktree CLI tool with automated setup, branch tracking, and smart navigation";
    homepage = "https://github.com/satococoa/wtp";
    changelog = "https://github.com/satococoa/wtp/releases/tag/v${version}";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "wtp";
  };
}
