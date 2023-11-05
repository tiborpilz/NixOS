{ pkgs ? import <nixpkgs> {
    inherit system;
  }
, system ? builtins.currentSystem
, ...
}:
pkgs.python3Packages.buildPythonPackage rec {
    pname = "llm-workflow-engine";
    version = "v0.18.2";
    buildInputs = [
      pkgs.git
    ];
    src = pkgs.fetchFromGitHub {
      owner = "llm-workflow-engine";
      repo = pname;
      rev = version;
      sha256 = "foG3g63Yx5QtNcBP5aOnkmqOWsj0tX3EOHq3Il5WE+M=";
    };
    meta = {
      homepage = "https://github.com/llm-workflow-engine/llm-workflow-engine";
      description = "LLM Workflow Engine";
      license = pkgs.lib.licenses.mit;
    };
  }
