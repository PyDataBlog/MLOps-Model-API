PROMPT_COMMAND='history -a >(tee -a ~/.bash_history | logger -t "$USER[$$] $SSH_CONNECTION")'
typeset -r PROMPT_COMMAND

alias top='glances'
