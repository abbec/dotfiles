{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    discord
    powertop
    xclip
    yubikey-manager
    yubikey-personalization-gui
    ttf_bitstream_vera
    twitter-color-emoji
    fixedsys-excelsior
  ];

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  programs.firefox.enable = true;

  gtk = {
    enable = true;
    font = {
      package = pkgs.ttf_bitstream_vera;
      name = "Bitstream Vera Sans 8";
    };
  };

  programs.termite = {
    enable = true;
    browser = "${pkgs.firefox}/bin/firefox";
    clickableUrl = true;
    fullscreen = true;
    iconName = "terminal";
    font = "monospace 10";
  };

  programs.rofi = {
    enable = true;
    theme = "gruvbox-light-soft";
    font = "Fira Code Regular 8";
  };

  # font stuff
  fonts.fontconfig.enable = true;
  home.file = {
    "default-fonts" = {
      source = ./fonts.conf;
      target = ".config/fontconfig/conf.d/99-default-fonts.conf";
      onChange = ''
        fc-cache -f -v
      '';
    };
  };

}
