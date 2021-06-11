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
  imports = [ ./commmon/common-linux.nix ];

  home.packages = [
    pkgsUnstable.obs-studio
    pkgsUnstable.zulip-term
  ];

  home.file = {
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

  # This is needed, otherwise the forwarded
  # SSH_AUTH_SOCK is overwritten
  home.sessionVariables = lib.mkAfter {
   SSH_AUTH_SOCK = "\${SSH_AUTH_SOCK:-$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)}";
  };
}
