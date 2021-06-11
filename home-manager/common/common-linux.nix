{ config, pkgs, lib, ... }:
{
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    discord
    powertop
    xsel
    yubikey-manager
    yubikey-personalization-gui
    ttf_bitstream_vera
    twitter-color-emoji
    fixedsys-excelsior
    albert
    spleen
  ];

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  programs.firefox.enable = true;

  # font stuff
  fonts.fontconfig.enable = true;
  home.file = {
    "default-fonts" = {
      source = ../../fonts.conf;
      target = ".config/fontconfig/conf.d/99-default-fonts.conf";
      onChange = ''
        fc-cache -f -v
      '';
    };
  };
}
