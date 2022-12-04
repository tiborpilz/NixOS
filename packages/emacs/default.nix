{
  pkgs,
  inputs,
  ...
}:
with pkgs;
with pkgs.emacsPackages;
let
  rev = inputs.copilot-el.shortRev;
in
  {
    "copilot" = melpaBuild {
      pname = "copilot";
      ename = "copilot";
      version = inputs.copilot-el.lastModifiedDate;
      commit = rev;
      packageRequires = [s];

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
  }
