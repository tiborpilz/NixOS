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

      # iac n stuff
      terraform
      opentofu
      # Terraform
      terraform
      unstable.terragrunt
    ];
  };
}
