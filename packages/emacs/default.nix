{ pkgs
, inputs
, ...
}:
with pkgs;
with pkgs.emacsPackages;
{
  "copilot" = let 
  rev = inputs.copilot-el.shortRev;
  in melpaBuild {
    pname = "copilot";
    ename = "copilot";
    version = inputs.copilot-el.lastModifiedDate;
    commit = rev;
    packageRequires = [ s ];

    src = fetchFromGitHub {
      inherit rev;
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
  "lsp-mode" = let
    rev = inputs.lsp-mode.shortRev;
  in  melpaBuild rec {
    ename = "lsp-mode";
    pname = "lsp-mode";
    fetcher = "github";
    repo = "emacs-lsp/lsp-mode";
    version = inputs.lsp-mode.lastModifiedDate;
    packageRequires = [
      dash
      f
      ht
      lv
      markdown-mode
      spinner
    ];
    commit = rev;
    recipe = writeText "recipe" ''
    (lsp-mode
      :repo "${repo}"
      :fetcher github
      :files (:defaults "clients/*.el"))
    '';
    src = fetchFromGitHub {
      inherit rev;
      owner = "emacs-lsp";
      repo = "lsp-mode";
      sha256 = inputs.lsp-mode.narHash;
    };
  };
  "emacs" = emacs.overrideAttrs (old: {
    # patches = (old.patches or [ ]) ++ [
    #   (pkgs.fetchpatch {
    #     url = "https://github.com/emacs-mirror/emacs/commit/8b52d9f5f177ce76b9ebecadd70c6dbbf07a20c6.patch";
    #     hash = "sha256-/W9yateE9UZ9a8CUjavQw0X7TgxigYzBuOvtAXdEsSA=";
    #   })
    # ];
    postFixup = (old.postFixup or "") + "wrapProgram $out/bin/emacs --set LSP_USE_PLISTS 'true'";
    # nativeComp = true;
  });
}
