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
    bat

    gitAndTools.delta

    perlPackages.NetSMTPSSL # needed for git-send-email

    zulip-term

    (import <toolbelt> {})
  ];

  home.keyboard = {
    layout = "se";
    options = "terminate:ctrl_alt_bksp, caps:escape, nodeadkeys";
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.vterm
    ];
  };

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
  };

  programs.zsh = {
    enable = true;
    initExtra = builtins.readFile ./zshrc.zsh;
    enableAutosuggestions = true;
    enableCompletion = true;
    plugins = [
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

    delta = {
      enable = true;
      options = {
        line-numbers = true;
        theme = "base16";
      };
    };

    aliases = {
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      co = "checkout"; # checkout a branch
      cob = "checkout -b"; # checkout a new not yet existing branch
      f = "fetch -p"; # fetch from a repository and prune any remote-tracking references that no longer exist on the remote.
      c = "commit"; # commit your changes
      p = "push"; # push your changes to a remote
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

    extraConfig = {
      sendemail = {
        from = "Albert Cervin <albert@acervin.com>";
        smtpserver = "smtp.gmail.com";
        smtpuser = "albert@acervin.com";
        smtpencryption = "tls";
        smtpserverport = "587";
      };
      pull = {
        rebase = true;
      };
    };
  };

  programs.fzf = rec {
    enable = true;
    enableZshIntegration = true;
    defaultCommand = "rg --files";
    fileWidgetCommand = defaultCommand;
  };

  # editors
  programs.neovim = {
    enable = true;
    extraConfig = ''
      " main config
      ${builtins.readFile ./neovim.vim}
      " coc config
      ${builtins.readFile ./coc-config.vim}
    '';

    plugins = with pkgs.vimPlugins;
      let
        customPlugins = {
          rainbow = pkgs.vimUtils.buildVimPlugin {
            name = "rainbow";
            src = pkgs.fetchFromGitHub {
              owner = "junegunn";
              repo = "rainbow_parentheses.vim";
              rev = "27e7cd73fec9d1162169180399ff8ea9fa28b003";
              sha256 = "0izbjq6qbia013vmd84rdwjmwagln948jh9labhly0asnhqyrkb8";
            };
          };
        };
      in
        [
          fugitive
          fzf-vim
          base16-vim
          coc-nvim
          customPlugins.rainbow

          # language support
          vim-nix
          vim-protobuf
          vim-toml
          vim-jinja
        ];
  };

  home.file = {
    "coc-config" = {
      source = ./coc-settings.json;
      target = ".config/nvim/coc-settings.json";
    };

    ".emacs.d" = {
      source = ./emacs.d;
      recursive = true;
    };
  };

  programs.gpg = {
    enable = true;
  };

  programs.tmux = {
    enable = true;
    terminal = "xterm-256color";
    keyMode = "vi";
    extraConfig = ''
      ${builtins.readFile ./tmux.conf}
      ${if builtins.currentSystem == "x86_64-darwin" then
      builtins.readFile ./tmux.macos.conf
      else
      builtins.readFile ./tmux.unix.conf}
    '';

    secureSocket = builtins.currentSystem != "x86_64-darwin";
  };

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
