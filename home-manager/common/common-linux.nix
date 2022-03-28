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
    spotify-tui
    wally-cli
  ];

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  services.emacs = {
    enable = true;
    client = {
      enable = true;
    };
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

  services.spotifyd = {
    enable = true;
    package = (pkgs.spotifyd.override { withKeyring = true; withPulseAudio = true; });
    settings = {
      global = {
        username = "abbe_c";
        use_keyring = true;
        device_name = "ultramagnus";
        bitrate = 320;
      };
    };
  };

  services.dropbox = {
    enable = true;
  };
}
