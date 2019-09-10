# allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

source ~/.zplug/init.zsh

zplug "akoenig/npm-run.plugin.zsh"
zplug "chriskempson/base16-shell", use:"base16-shell.plugin.zsh"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure", use:"pure.zsh", as:"theme"
zplug "arzzen/calc.plugin.zsh", defer:2
zplug "chisui/zsh-nix-shell"
zplug "spwhitt/nix-zsh-completions"

# mark completion
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# have "global" history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.history
setopt INC_APPEND_HISTORY

# use ssh agent from gpg
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"

# local bin
export PATH="$HOME/bin:$PATH"

# local man pages
export MANPATH=$HOME/.local/share/man:$MANPATH

# vim
export EDITOR=vim

# backspace and ^h working even after
# returning from command mode
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char

# normal home, end and delete
typeset -g -A key
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line
bindkey "${terminfo[kdch1]}" delete-char

bindkey "^A" beginning-of-line
bindkey "^E" end-of-line

# arrow to search
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search
bindkey '^[OA' up-line-or-beginning-search
bindkey '^[OB' down-line-or-beginning-search

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# use rg for fzf
export FZF_DEFAULT_COMMAND='rg --files'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# aliases
alias gp=gpg2
alias l='ls -lah'

function bwd() {
	echo ${$(pwd):gs/\//🥖/}
}

zplug load
