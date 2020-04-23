# allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

# mark completion
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# use ssh agent from gpg
if [[ -z $XDG_RUNTIME_DIR ]]; then
        export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
else
        export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"
fi

# local bin
export PATH="$HOME/bin:$PATH"

# local man pages
export MANPATH=$HOME/.local/share/man:$MANPATH

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

function bwd() {
	echo ${$(pwd):gs/\//🥖/}
}
