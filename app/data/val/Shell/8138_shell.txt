#!/bin/sh
#
# zsh
#
#
echo "installing zsh files..."
if [ ! -d  "$HOME"/.oh-my-zsh ]; then
	git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

