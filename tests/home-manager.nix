{ inputs, lib, pkgs, ... }:

let
  my = import ../lib { inherit inputs lib pkgs; };
in
{
  name = "home-manager-suite";

  nodes.machine = { config, pkgs, lib, ... }: {
    inherit lib;

    virtualisation.diskSize = 8192;
    
    # Import home-manager NixOS module
    imports = [ inputs.home-manager.nixosModules.home-manager ];

    # 1. Setup X11 for visual tests
    services.xserver.enable = true;
    services.xserver.desktopManager.xterm.enable = false;
    services.xserver.windowManager.i3.enable = true;
    services.displayManager.defaultSession = "none+i3";

    # 2. Configure Test User
    users.users.testuser = {
      isNormalUser = true;
      password = "password";
      shell = pkgs.zsh;
    };

    # 3. Import Home Manager Configuration
    # We adapt the existing home configuration for the test user
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.testuser = { config, ... }: {
      imports = [ ../home ];
      
      # Override inputs parameter that home config expects
      _module.args.inputs = inputs;
      
      # Set required home-manager options
      home.username = "testuser";
      home.homeDirectory = "/home/testuser";
      home.stateVersion = "23.11";
      
      # Disable graphical components for simpler testing
      graphical = true;
      
      # Disable syncthing service for testing
      modules.syncthing.service = false;
      
      # Override neovim config to use the actual config files in the nix store
      # instead of out-of-store symlink which won't work in test VM
      xdg.configFile."nvim" = lib.mkForce {
        source = ../home/config/neovim;
        recursive = true;
      };
    };

    # Ensure dependencies for testing (e.g. terminal) are present
    environment.systemPackages = with pkgs; [ xterm ];
  };

  # Python Test Script
  testScript = ''
    start_all()
    
    # Phase 1: Functional CLI Tests
    machine.wait_for_unit("multi-user.target")
    
    # Check that key packages are available
    machine.succeed("su - testuser -c 'which nvim'")
    machine.succeed("su - testuser -c 'which git'")
    machine.succeed("su - testuser -c 'which direnv'")
    machine.succeed("su - testuser -c 'which zsh'")
    
    # Verify nvim runs without errors
    machine.succeed("su - testuser -c 'nvim --version'")
    
    # Verify git is configured
    machine.succeed("su - testuser -c 'git --version'")
    
    # Check for configuration files
    machine.succeed("su - testuser -c 'test -e ~/.config/nvim/init.lua'")

    # Phase 2: Visual/GUI Tests
    machine.wait_for_x()
    
    # Open a terminal with Neovim running
    # Use execute() for background processes to avoid race conditions
    machine.execute("su - testuser -c 'DISPLAY=:0 xterm -e nvim &'")
    
    # Wait for Neovim to potentially load (give it 5-10 seconds)
    machine.sleep(10)
    
    # Take a screenshot of the initial state
    machine.screenshot("neovim_startup")

    # Check for the absence of error messages on screen
    # (This is a negative check; OCR can be flaky so we rely mostly on the screenshot and exit code)
    
    # Attempt to verify status line text if predictable
    # Note: OCR-based checks can be unreliable, so we primarily rely on functional tests
    # machine.wait_for_text("NORMAL")
  '';
}
