{ pkgs ? import <nixpkgs> {
    inherit system;
  }
, inputs
, system ? builtins.currentSystem
, ...
}:
let
  currentSystem = if pkgs.stdenv.isDarwin then "x86_64-darwin" else "x86_64-linux";
in
{
  "llm-workflow-engine" = import ./llm-workflow-engine {
    inherit pkgs;
    inherit inputs;
    inherit system;
  };
  # "chatgpt-wrapper" = inputs.mach-nix.lib.x86_64-linux.buildPythonPackage {
  #   pname = "chatgpt-wrapper";
  #   version = "0.1.0";
  #   src = builtins.fetchGit {
  #     url = "https://github.com/mmabrouk/chatgpt-wrapper";
  #     ref = "main";
  #     rev = inputs.chatgpt-wrapper.rev;
  #   };
  #   requirements = builtins.readFile ./chatgpt-wrapper-requirements.txt;
  # };
  # "chatgpt-wrapper" = buildPythonApplication rec {
  #   pname = "chatgpt-wrapper";
  #   version = "0.1.0";
  #   src = builtins.fetchGit {
  #     url = "https://github.com/mmabrouk/chatgpt-wrapper";
  #     ref = "main";
  #     rev = inputs.chatgpt-wrapper.rev;
  #   };

  #   dontUseSetuptoolscheck = true;
  # }
}
