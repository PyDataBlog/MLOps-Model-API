# vim: filetype=zsh sw=2 ts=2 sts=2 et tw=80 foldlevel=0 nospell

function prompt_powerline_shell_precmd() {
  PS1="$(powerline-shell.py --cwd-mode fancy --colorize-hostname --shell zsh --mode patched $? 2> /dev/null)"
}

autoload -Uz add-zsh-hook
add-zsh-hook -Uz precmd prompt_powerline_shell_precmd
