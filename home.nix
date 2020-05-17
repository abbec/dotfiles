{ config, pkgs, ... }:
{

  imports = if builtins.currentSystem == "x86_64-linux"
  then [ ./linux.nix ]
  else if builtins.currentSystem == "x86_64-darwin"
  then [./macos.nix]
  else [];

  home.packages = with pkgs; [
    fira-code
    htop
    ripgrep
    nodejs # required by coc.nvim
    fontconfig
    mpv
  ];

  home.keyboard = {
    layout = "se";
    options = "terminate:ctrl_alt_bksp, caps:escape, nodeadkeys";
  };

  programs.zsh = {
    enable = true;
    initExtra = builtins.readFile ./zshrc.zsh;
    enableAutosuggestions = true;
    enableCompletion = true;
    plugins = [
      {
        name = "pure";
        src = pkgs.fetchFromGitHub {
          owner = "sindresorhus";
          repo = "pure";
          rev = "v1.11.0";
          sha256 = "0nzvb5iqyn3fv9z5xba850mxphxmnsiq3wxm1rclzffislm8ml1j";
        };
      }
      {
        name = "base16-shell";
        src = pkgs.fetchFromGitHub {
          owner = "chriskempson";
          repo = "base16-shell";
          rev = "master";
          sha256 = "1yj36k64zz65lxh28bb5rb5skwlinixxz6qwkwaf845ajvm45j1q";
        };
      }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "0.7.1";
          sha256 = "03r6hpb5fy4yaakqm3lbf4xcvd408r44jgpv4lnzl9asp4sb9qc0";
        };
      }
    ];

    history = {
      extended = true;
      ignoreDups = true;
    };

    shellAliases = {
      gp = "gpg2";
      l = "ls -lah";
    };

  };

  programs.git = {
    enable = true;
    userName = "Albert Cervin";
    userEmail = "albert@acervin.com";
    signing = {
      key = "8EC09E34A35E8D55";
      signByDefault = true;
    };

    aliases = {
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      co = "checkout"; # checkout a branch
      cob = "checkout -b"; # checkout a new not yet existing branch
      f = "fetch -p"; # fetch from a repository and prune any remote-tracking references that no longer exist on the remote.
      c = "commit"; # commit your changes
      p = "push"; # push you changes to a remote
      ba = "branch -a"; # list both remote-tracking branches and local branches.
      bd = "branch -d"; # delete a branch only if it has been merged
      bD = "branch -D"; # force delete a branch
      dc = "diff --cached"; # display the staged changes
      bdm = "!git branch --merged | grep -v '*' | xargs -n 1 git branch -d"; # delete merged branches
      st = "status -sb"; # fancier status command
      can = "commit --amend --no-edit"; # amend to last commit
      please = "push --force-with-lease"; # force push with lease
      apa = "add -p -A"; # add in patches

      serve = "!git daemon --base-path=. --export-all --reuseaddr --informative-errors --verbose";
      hub = "!git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --informative-errors --verbose";
    };
  };

  programs.fzf = rec {
    enable = true;
    enableZshIntegration = true;
    defaultCommand = "${pkgs.ripgrep}/bin/rg --files";
    fileWidgetCommand = defaultCommand;
  };

  # editors
  programs.neovim = {
    enable = true;
    configure = {
      customRC = ''
        " main config
        ${builtins.readFile ./neovim.vim}

        " coc config
        ${builtins.readFile ./coc-config.vim}
      '';
      packages.nvimPlugins = with pkgs.vimPlugins; {
        start = [
          fugitive
          fzf-vim
          base16-vim
          vim-nix
          vim-protobuf
          coc-nvim
        ];

        # can be loaded manually with `:packadd <plugin-name>`
        opt = [];
      };
    };
  };

  home.file = {
    "coc-config" = {
      source = ./coc-settings.json;
      target = ".config/nvim/coc-settings.json";
    };
  };

  programs.gpg = {
    enable = true;
  };

  programs.tmux = {
    enable = true;
    extraConfig = ''
      ${builtins.readFile ./tmux.conf}
      ${if builtins.currentSystem == "x86_64-darwin" then
      builtins.readFile ./tmux.unix.conf
      else
      builtins.readFile ./tmux.macos.conf}
    '';

    secureSocket = builtins.currentSystem != "x86_64-darwin";
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

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
