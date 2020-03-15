{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    htop
    discord
    powertop
    ripgrep
    xclip
    fira-code
    twitter-color-emoji
    ttf_bitstream_vera
    yubikey-manager
    yubikey-personalization-gui
  ];

  fonts.fontconfig.enable = true;
  programs.firefox.enable = true;

  home.keyboard = {
    layout = "se";
    options = "terminate:ctrl_alt_bksp, caps:escape, nodeadkeys";
  };

  gtk = {
    enable = true;
    font = {
      package = pkgs.ttf_bitstream_vera;
      name = "Bitstream Vera Sans 8";
    };
  };

  programs.zsh = {
    enable = true;
    initExtra = "source ~/code/dotfiles/zshrc.zsh";
  };

  programs.termite = {
    enable = true;
    browser = "${pkgs.firefox}/bin/firefox";
    clickableUrl = true;
    fullscreen = true;
    iconName = "terminal";
  };

  programs.rofi = {
    enable = true;
    theme = "gruvbox-light-soft";
    font = "Fira Code Regular 8";
  };

  programs.git = {
    enable = true;
    userName = "Albert Cervin";
    userEmail = "albert@acervin.com";
    includes = [
      { path = "~/code/dotfiles/gitconfig"; }
    ];
    signing = {
      key = "8EC09E34A35E8D55";
      signByDefault = true;
    };
  };

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  programs.tmux = {
    enable = true;
    extraConfig = ''
      ${builtins.readFile ./tmux.conf}
      ${builtins.readFile ./tmux.unix.conf}
    '';
  };

  programs.vim = {
    enable = true;
    extraConfig = ''
      set runtimepath=~/code/dotfiles/vim,~/code/dotfiles/vim/after,$VIMRUNTIME
      source ~/code/dotfiles/vim/vimrc
    '';
  };

  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal = {
          family = "monospace";
        };
        bold = {
          family = "monospace";
        };
        italic = {
          family = "monospace";
        };
        size = 7;
      };
    };
  };

  home.file = {
    "default-fonts" = {
      source = ./fonts.conf;
      target = ".config/fontconfig/conf.d/99-default-fonts.conf";
      onChange = ''
        fc-cache -f -v
      '';
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
