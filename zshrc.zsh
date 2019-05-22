autoload -U compinit
compinit

# allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

source ~/.zplug/init.zsh

zplug "arzzen/calc.plugin.zsh"
zplug "akoenig/npm-run.plugin.zsh"
zplug "chriskempson/base16-shell", use:"base16-shell.plugin.zsh"
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure", use:"pure.zsh", as:"theme"

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
export PATH="~/bin:$PATH"

# vim
export EDITOR=vim

# But still use emacs-style zsh bindings
bindkey -e
bindkey "^[[3~" delete-char

# arrow to search
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search

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
