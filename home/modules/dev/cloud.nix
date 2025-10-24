{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.cloud;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.cloud = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # it's pronounced kubernetes not kubernetes
      kubectl
      kubernetes-helm
      kustomize
      # backup management
      velero
      # plugins
      krew

      # Rancher Kubernetes Engine
      rke

      # Google
      google-cloud-sdk
      google-cloud-sql-proxy

      # iac n stuff
      terraform
      opentofu
      terraform
      unstable.terragrunt
    ];
    
    modules.shell.zsh.aliases.k = "kubectl";
    modules.shell.zsh.aliases.tf = "terraform";

    modules.shell.zsh.fpathDirs = ''
      ${pkgs.google-cloud-sdk}/share/zsh/site-functions
      ${pkgs.terraform}/share/zsh/site-functions
      ${pkgs.kubectl}/share/zsh/site-functions
    '';
  };
}
