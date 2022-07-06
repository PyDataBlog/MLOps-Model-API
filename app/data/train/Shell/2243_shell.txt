#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh" \
    && . "./utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   CLI Tools\n\n"


brew_install "ack" "ack"
brew_install "findutils" "findutils"
brew_install "sed" "gnu-sed --with-default-names"
brew install "mtr" "mtr"
brew_install "moreutils" "moreutils"
brew_install "tree" "tree"
brew_install "pidcat" "pidcat"
brew_install "pv" "pv"
brew_install "wget" "wget --with-irl"
