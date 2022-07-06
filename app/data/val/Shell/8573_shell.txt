#!/bin/bash
#
# Template for Bash script file to provide standalone "command";
# 1. File should be executed in a new shell or a subshell to prevent contamination
#	 of the calling shell;
# 2. Script should declare all dependencies in upfront global array which is then
#	sourced as part of the execution of the main script;
#

# Upfront declaration of dependencies
DEPENDS_ON=( \
	~/bin/dependency-1.sh \
        ~/bin/dependency-2.sh \
)

# Global variables
# Get path of currently executing script
CWD="${BASH_SOURCE%/*}"

# Define script entry point (main)
# Run in subshell so that we can use exit
__main () (
	# Do stuff ...
	__func_1

	# Report success status back to parent shell
	exit 0
)

# Forward declcaration of all functions
# Private functions, should begin __
__func_1 () {
	# Declare local variables
	# Note, local scopes to current and all called functions
	local variable_1="..."
}

# Load dependencies
for FILE in "${DEPENDS_ON[@]}"; do source "${CWD}/${FILE}"; done

# Call the entry point after all other functions have been forward declared
# 1. Pass through all positional parameters
# 2. Call to main must be last line of script such that exit code is returned to caller
__main "$@"
