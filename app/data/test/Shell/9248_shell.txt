# /etc/bash.bashrc
# source: https://github.com/olegstepura/config-bashrc

# If not running interactively, don't do anything!
[ -z "$PS1" ] && return

# fortune is a simple program that displays a pseudorandom message
# from a database of quotations at logon and/or logout.
# Type: "pacman -S fortune-mod" to install it, then uncomment the
# following line:

# [[ "$PS1" ]] && /usr/bin/fortune

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS. Try to use the external file
# first to take advantage of user additions. Use internal bash
# globbing instead of external grep binary.

# sanitize TERM:

function __prompt_command_generator() {
        PS1=""
        PS1+='$(__LAST_EXIT="$?";'  # This needs to be first

        local TXTRED='\[\e[0;31m\]' # red
        local TXTGRN='\[\e[0;32m\]' # green
        local TXTYLW='\[\e[0;33m\]' # yellow
        local TXTBLU='\[\e[0;34m\]' # blue
        local TXTPUR='\[\e[0;35m\]' # purple
        local TXTCYN='\[\e[0;36m\]' # cyan
        local TXTWHT='\[\e[0;37m\]' # white
        local BLDRED='\[\e[1;31m\]' # red    - Bold
        local BLDGRN='\[\e[1;32m\]' # green
        local BLDYLW='\[\e[1;33m\]' # yellow
        local BLDBLU='\[\e[1;34m\]' # blue
        local BLDPUR='\[\e[1;35m\]' # purple
        local BLDCYN='\[\e[1;36m\]' # cyan
        local BLDWHT='\[\e[1;37m\]' # white
        local TXTRST='\[\e[0m\]'    # Text reset

        #PS1+='echo -n "'$TXTCYN'\D{%H:%M:%S} ";'
        PS1+='echo -n "'$TXTCYN'\D{%H:%M:%S} ";'

        #mark root as red
        PS1+='[[ $UID -eq 0 ]] && echo -n "'$BLDRED'" || echo -n "'$TXTGRN'";'
        PS1+='echo -n "\u'$TXTYLW'@'$BLDCYN'\H";'

        #append current path
        PS1+='echo -n " '$BLDBLU'\w";'

        #append git branch
        if type -p git>/dev/null; then
                PS1+='__GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null);'
                PS1+='__GIT_CHANGES=$(git status  --untracked-files=all --porcelain 2> /dev/null | wc -l);'
                PS1+='__GIT_CHANGES=${__GIT_CHANGES//[[:blank:]]/};'
                PS1+='__GIT_CHANGES=$([[ $__GIT_CHANGES -eq 0 ]] && echo -n "" ||  echo -n " +${__GIT_CHANGES}");'
                PS1+='[[ -n "$__GIT_BRANCH" ]] && echo -n " '$TXTYLW'(${__GIT_BRANCH}${__GIT_CHANGES})";'
                PS1+='unset __GIT_BRANCH;'
                PS1+='unset __GIT_CHANGES;'
        fi

        #show exit code
        PS1+='[[ $__LAST_EXIT -eq 0 ]] && echo -n "'$BLDBLU'" || echo -n "'$BLDRED'";';
        PS1+='unset __LAST_EXIT)'

        PS1+=" \\\$ $TXTRST"
}

#do not use this in e.g. scp
__prompt_command_generator
unset __prompt_command_generator

PS2="> "
PS3="> "
PS4="+ "

case ${TERM} in
        xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
                PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
                ;;
        screen)
                PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
                ;;
esac

# update histfile after every command
export PROMPT_COMMAND="history -a"

# Enable colors for ls, etc. Prefer ~/.dir_colors
if type -P dircolors >/dev/null ; then
        if [[ -f ~/.dir_colors ]] ; then
                eval $(dircolors -b ~/.dir_colors)
        elif [[ -f /etc/DIR_COLORS ]] ; then
                eval $(dircolors -b /etc/DIR_COLORS)
        else
                eval $(dircolors -b)
        fi
fi

alias ls="ls --color=auto"
alias dir="dir --color=auto"
alias grep="grep --colour=auto"

# Try to enable the auto-completion (type: "pacman -S bash-completion" to install it).
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion
[ -r /usr/share/bash-completion/completions/fzf ] && . /usr/share/bash-completion/completions/fzf


alias lsh="ls -lah"

export EDITOR="vim"

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# automatically prepend cd when entering just a path in the shell
shopt -s autocd

export HISTFILESIZE=5000
export HISTCONTROL=ignoreboth:erasedups   # no duplicate entries

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

# Enable history appending instead of overwriting.
shopt -s histappend

# Append user home bin directory to allow using custom scripts easily
export PATH="$PATH::~/bin"

# Display local todo list
TODO=$(cat ~/.todo 2>/dev/null)
if [ "$TODO" ]; then
        echo "Todo list:"
        echo "$TODO"
fi

