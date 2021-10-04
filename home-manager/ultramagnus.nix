{ config, pkgs, ... }:
{
  imports = [ ./common/common-linux.nix ];

  home.packages = with pkgs; [
    obs-studio
    zulip-term
  ];

  home.sessionVariables = {
    GIT_SSH = "/usr/bin/ssh"; # https://github.com/NixOS/nixpkgs/issues/58132
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };

  # do this to get gnome to discover nix-installed apps
  targets.genericLinux.enable = true;
  programs.bash.enable = true;

  services.spotifyd.settings.global.device_name = "ultramagnus";
}
