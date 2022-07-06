if [[ -n $SSH_CONNECTION ]]; then
  export PS1='%m:%3~$(git_info_for_prompt)%# '
else
  export PS1='%3~$(git_info_for_prompt)%# '
fi

export LSCOLORS="exfxcxdxbxegedabagacad"
export CLICOLOR=true

fpath=($ZSH/functions $fpath)

autoload -U $ZSH/functions/*(:t)

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt NO_BG_NICE # don't nice background tasks
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS # allow functions to have local options
setopt LOCAL_TRAPS # allow functions to have local traps
setopt HIST_VERIFY
setopt SHARE_HISTORY # share history between sessions ???
setopt EXTENDED_HISTORY # add timestamps to history
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD

setopt APPEND_HISTORY # adds history
# incrementally add history and share it across sessions
setopt INC_APPEND_HISTORY SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS  # don't record dupes in history
setopt HIST_REDUCE_BLANKS

# don't expand aliases _before_ completion has finished
#   like: git comm-[tab]
setopt complete_aliases

#zle -N newtab
#
#bindkey '^[^[[D' backward-word
#bindkey '^[^[[C' forward-word
#bindkey '^[[5D' beginning-of-line
#bindkey '^[[5C' end-of-line
#bindkey '^[[3~' delete-char
#bindkey '^[^N' newtab
#bindkey '^?' backward-delete-char

# my old stuff
bindkey -e # Revert back to emacs mode
WORDCHARS='' # Use emacs-style word matching

autoload -U select-word-style
select-word-style normal
zstyle ':zle:*' word-style subword

autoload -U down-line-or-beginning-search up-line-or-beginning-search
zle -N down-line-or-history down-line-or-beginning-search
zle -N up-line-or-history up-line-or-beginning-search

# Make ^W work like it does in bash (while leaving other bindings alone)
zle -N backward-kill-word-bash backward-kill-word-match
zstyle ':zle:backward-kill-word-bash' word-style whitespace

bindkey '^Q' quoted-insert \
        '^U' vi-kill-line \
        '^W' backward-kill-word-bash

if [[ "$TERM" = xterm* ]]
then
    bindkey '\e[1;5A' up-line-or-history \
            '\e[1;3A' up-line-or-history \
            '\e[1;5B' down-line-or-history \
            '\e[1;3B' down-line-or-history \
            '\e[1;5D' backward-word \
            '\e[1;3D' backward-word \
            '\e[1;5C' forward-word \
            '\e[1;3C' forward-word
fi

autoload -U zkbd
[[ ! -d "$ZDOTDIR/.zkbd" ]] && mkdir "$ZDOTDIR/.zkbd"
[[ ! -f "$ZDOTDIR/.zkbd/$TERM-$VENDOR-$OSTYPE" ]] && zkbd
source "$ZDOTDIR/.zkbd/$TERM-$VENDOR-$OSTYPE"

[[ -n "${key[Backspace]}" ]] && bindkey "${key[Backspace]}" \
                                        backward-delete-char
[[ -n "${key[Insert]}" ]] && bindkey "${key[Insert]}" beep
[[ -n "${key[Home]}" ]] && bindkey "${key[Home]}" beginning-of-line
[[ -n "${key[PageUp]}" ]] && bindkey "${key[PageUp]}" beep
[[ -n "${key[Delete]}" ]] && bindkey "${key[Delete]}" delete-char
[[ -n "${key[End]}" ]] && bindkey "${key[End]}" end-of-line
[[ -n "${key[PageDown]}" ]] && bindkey "${key[PageDown]}" beep
[[ -n "${key[Up]}" ]] && bindkey "${key[Up]}" up-line-or-history && \
                         bindkey "\e${key[Up]}" up-line-or-history
[[ -n "${key[Left]}" ]] && bindkey "${key[Left]}" backward-char && \
                           bindkey "\e${key[Left]}" backward-word
[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-history && \
                           bindkey "\e${key[Down]}" down-line-or-history
[[ -n "${key[Right]}" ]] && bindkey "${key[Right]}" forward-char && \
                            bindkey "\e${key[Right]}" forward-word
[[ -n "${key[Control-Left]}" ]] && bindkey "${key[Control-Left]}" backward-word
[[ -n "${key[Control-Right]}" ]] && bindkey "${key[Control-Right]}" \
                                            forward-word
# local zsh help
unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

# zsh syntax highlighting:
# https://github.com/zsh-users/zsh-syntax-highlighting
[[ -d "$HOME/zsh-syntax-highlighting" ]] && source "$HOME/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
