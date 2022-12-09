{ pkgs
, inputs
, ...
}:
with pkgs;
with pkgs.emacsPackages;

{
  copilot = melpaBuild {
    pname = "copilot";
    ename = "copilot";
    version = inputs.copilot-el.lastModifiedDate;
    commit = inputs.copilot-el.shortRev;
    packageRequires = [ s ];

    src = fetchFromGitHub {
      rev = inputs.copilot-el.shortRev;
      owner = "zerolfx";
      repo = "copilot.el";
      sha256 = inputs.copilot-el.narHash;
    };

    recipe = writeText "recipe" ''
      (copilot
        :repo "zerolfx/copilot.el"
        :fetcher github
        :files ("*.el" "dist"))
    '';
  };

  lsp-bridge = melpaBuild rec {
    pname = "lsp-bridge";
    ename = "lsp-bridge";
    version = inputs.lsp-bridge.lastModifiedDate;
    commit = inputs.lsp-bridge.shortRev;
    packageRequires = with python310Packages; [
      # Emacs requirements
      posframe
      markdown-mode
      yasnippet
      # Python requirements
      python310
      epc
      orjson
      six
    ];

    src = fetchFromGitHub {
      rev = inputs.lsp-bridge.shortRev;
      owner = "manateelazycat";
      repo = "lsp-bridge";
      sha256 = inputs.lsp-bridge.narHash;
    };

    recipe = writeText "recipe" ''
      (lsp-bridge
        :repo "manateelazycat/lsp-bridge"
        :fetcher github
        :files ("*"))
    '';
  };
}
