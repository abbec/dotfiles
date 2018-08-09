autoload -U compinit
compinit

# allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD
setopt COMPLETE_ALIASES

source ~/.zplug/init.zsh

zplug "arzzen/calc.plugin.zsh"
zplug "abbec/emoji-cli"
EMOJI_CLI_FILTER=fzf
EMOJI_CLI_KEYBIND=^X^E
zplug "akoenig/npm-run.plugin.zsh"
zplug "dracula/zsh", as:theme

# mark completion
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# have "globaL" history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.history
setopt APPEND_HISTORY

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
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# use rg for fzf
export FZF_DEFAULT_COMMAND='rg --files'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

zplug load
