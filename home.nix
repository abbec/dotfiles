{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    htop
    discord
    powertop
    ripgrep
    xclip
    ibus
  ];

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
      ${builtins.readFile ~/code/dotfiles/tmux.conf}
      ${builtins.readFile ~/code/dotfiles/tmux.unix.conf}
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
          family = "Fira Code";
        };
        size = 7;
      };
    };
  };

  fonts.fontconfig.enable = true;

  home.file = {
    "default-fonts" = {
      source = ~/code/dotfiles/fonts.conf;
      target = ".config/fontconfig/conf.d/20-defaults.conf";
      onChange = ''
        fc-cache -f -v
      '';
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
