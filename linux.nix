{ config, pkgs, lib, ... }:

let
  pkgsUnstable = import <nixpkgs-unstable> {};

  base16-konsole = pkgs.fetchFromGitHub {
    owner = "cskeeters";
    repo = "base16-konsole";
    rev = "b30e0b26e766cf8a3d12afb18ac2773f53af5a87";
    sha256 = "0a8pjah1nkimnj3b59mvfj53zmjd4252s7zb7drvv45ik4n4cjj9";
  };
in
{
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

    pkgsUnstable.obs-studio
    pkgsUnstable.zulip-term
  ];

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  # This is needed, otherwise the forwarded
  # SSH_AUTH_SOCK is overwritten
  home.sessionVariables = lib.mkAfter {
    SSH_AUTH_SOCK = "\${SSH_AUTH_SOCK:-$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)}";
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

    "konsole-base16" = {
      source = "${base16-konsole}/colorscheme-vim";
      target = ".local/share/konsole";
      recursive = true;
    };

    "konsole-profile" = {
      source = ./konsole.profile;
      target = ".local/share/konsole/abbe.profile";
    };

  };

}
