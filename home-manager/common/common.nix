{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    fira-code
    htop
    ripgrep
    nodejs # required by coc.nvim
    fontconfig
    mpv
    bat
    gitAndTools.delta
    zulip
    glances
    dropbox-cli
    perlPackages.NetSMTPSSL # needed for git-send-email
    rnix-lsp
    (import <toolbelt> { })
  ];

  home.keyboard = {
    layout = "se";
    options = "terminate:ctrl_alt_bksp, caps:escape, nodeadkeys";
  };

  home.stateVersion = "22.05";

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: [
      epkgs.vterm
    ];
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.zsh = {
    enable = true;
    initExtra = builtins.readFile ../../zshrc.zsh;
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

  accounts.email.accounts = {
    private = {
      realName = "Albert Cervin";
      address = "albert@acervin.com";
      flavor = "gmail.com";
      primary = true;
      msmtp.enable = true;
      mbsync = {
        enable = true;
        create = "both";
        subFolders = "Maildir++";
        extraConfig.account = {
          PipelineDepth = 1;
        };
      };
      passwordCommand = "secret-tool lookup service smtp user albert@acervin.com";
    };

    work = {
      realName = "Albert Cervin";
      address = "albert.cervin@goodbyekansas.com";
      flavor = "gmail.com";
      msmtp.enable = true;
    };
  };

  programs.msmtp.enable = true;
  services.mbsync.enable = true;
  programs.mbsync.enable = true;

  programs.git = {
    enable = true;
    userName = "Albert Cervin";
    userEmail = "albert@acervin.com";
    package = pkgs.gitAndTools.gitFull;
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

        # let msmtp use the from address to determine account
        envelopesender = "auto";
      };
      pull = {
        rebase = true;
      };

      github.user = "abbec";
    };

    includes = [
      {
        condition = "gitdir:~/code/gbk/";
        contents = {
          user = {
            name = "Albert Cervin";
            email = "albert.cervin@goodbyekansas.com";
          };

          sendemail = {
            from = "Albert Cervin <albert.cervin@goodbyekansas.com>";
          };
        };
      }
    ];
  };

  programs.fzf = rec {
    enable = true;
    enableZshIntegration = true;
    defaultCommand = "rg --files";
    fileWidgetCommand = defaultCommand;
  };

  home.file = {
    "coc-config" = {
      source = ../../coc-settings.json;
      target = ".config/nvim/coc-settings.json";
    };

    ".emacs.d" = {
      source = ../../emacs.d;
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
      ${builtins.readFile ../../tmux.conf}
      ${if builtins.currentSystem == "x86_64-darwin" then
      builtins.readFile ../../tmux.macos.conf
      else
      builtins.readFile ../../tmux.unix.conf}
    '';

    secureSocket = builtins.currentSystem != "x86_64-darwin";
  };

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
