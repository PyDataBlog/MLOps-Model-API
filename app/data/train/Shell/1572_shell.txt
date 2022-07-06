#!/bin/sh
#  
#  Unix/Linux Environment build script
#  Author: Sasuke
#

if [ -e ~/.vim ] ; then
  else
    mkdir ~/.vim
fi

if [ -e  ~/.vimrc ]; then
  read ANS

# Ordinary Pochi files
ln -s $HOME/Pochi/.tmux.conf $HOME/.tmux.conf
ln -s $HOME/Pochi/.vimrc $HOME/.vimrc
ln -s $HOME/Pochi/.editorconfig $HOME/.editorconfig

# I think it's useful for zsh to contain zsh configuration to Pochi
# but if I want to use prezto, It's difficult to deploy on Pochi.
# And then I decided not to deploy zsh files on Pochi.
# I can deploy zsh and prezto maually instead of this script.
# 
# 1. 
# git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
# 2. setopt
# setopt EXTENDED_GLOB
# for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
#    ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
# done


