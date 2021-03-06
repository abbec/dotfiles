# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # have insanely fancy graphics right from the start
  boot.initrd.kernelModules = [ "i915" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos-laptop"; # Define your hostname.
  networking.networkmanager.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  networking.firewall.enable = false;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    inputMethod.enabled = "ibus";
  };

  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true;
  };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    pavucontrol
    python2
    python3
    latte-dock
    libsForQt5.qtstyleplugin-kvantum
  ];

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    forwardX11 = true;
    extraConfig = "StreamLocalBindUnlink yes";
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "se";
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
  };

  # Enable touchpad support.
  services.xserver.libinput = {
    enable = true;
    scrollMethod = "twofinger";
    naturalScrolling = false;
  };

  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  # this is a laptop
  services.upower.enable = true;

  fonts.fontconfig.defaultFonts = {
    emoji = pkgs.lib.mkForce [];
    monospace = pkgs.lib.mkForce [];
    sansSerif = pkgs.lib.mkForce [];
    serif = pkgs.lib.mkForce [];
  };

  users.users = {
    abbe = {
      isNormalUser = true;
      extraGroups = [ "wheel" "audio" "video" "networkmanager" ];
      description = "Albert Cervin";
      home = "/home/abbe";
      shell = pkgs.zsh;
    };
  };

  nix.trustedUsers = [ "abbe" ];

  # yubikey things
  services.udev.packages = [ pkgs.yubikey-personalization pkgs.libu2f-host ];
  services.pcscd.enable = true;

  security.pam.u2f = {
    enable = true;
    cue = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  # intel video
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver # only available starting nixos-19.03 or the current nixos-unstable
    ];
  };
}

