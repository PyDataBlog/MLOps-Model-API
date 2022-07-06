if [[ "$OSTYPE" != darwin* ]]; then return 1; fi

unalias run-help 2> /dev/null
autoload run-help
export HELPDIR="$(brew --prefix)/share/zsh/helpfiles"
