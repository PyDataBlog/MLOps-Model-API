#!/usr/bin/env bash

DOTFILES=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# For now the dotvim directory is empty and isn't tracked by git
mkdir $DOTFILES/vim/dotvim

# Create symlinks
ln -s $DOTFILES/vim/dotvim ~/.vim
ln -s $DOTFILES/vim/vimrc ~/.vimrc
ln -s $DOTFILES/screen/screenrc ~/.screenrc
ln -s $DOTFILES/tmux/tmux.conf ~/.tmux.conf
ln -s $DOTFILES/git/gitconfig ~/.gitconfig

# Install Vim Plugins
vim +PlugInstall +qall

# Install CoC Plugins
# Note: requires Node.js to be installed
vim "+CocInstall -sync coc-css" +qall
vim "+CocInstall -sync coc-go" +qall
vim "+CocInstall -sync coc-html" +qall
vim "+CocInstall -sync coc-json" +qall
vim "+CocInstall -sync coc-pyright" +qall
vim "+CocInstall -sync coc-sh" +qall
vim "+CocInstall -sync coc-tsserver" +qall
