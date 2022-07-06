export PATH=~/.composer/vendor/bin:$PATH
export PATH="$PATH:`yarn global bin`"

# Load custom commands (executables)
export PATH="$DOTFILES/bin:$PATH"

# Local bin directories before anything else
export PATH=/bin:/usr/bin:/usr/sbin:/usr/local/bin:$PATH
