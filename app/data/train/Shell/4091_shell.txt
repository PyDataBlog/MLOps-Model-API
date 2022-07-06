#!/usr/bin/env bash

BASE_DIR="$HOME/.vim"
PLUGIN_DIR="$BASE_DIR/bundle"
COLORS_DIR="$BASE_DIR/colors"
AUTOLOAD_DIR="$BASE_DIR/autoload"
PLUGINS=(
  "tpope/vim-pathogen"
  "kien/ctrlp.vim"
  "vim-airline/vim-airline"
  "vim-airline/vim-airline-themes"
  "scrooloose/nerdtree"
  "dracula/vim"
  "tomasiser/vim-code-dark"
)

mkdir -p $PLUGIN_DIR
mkdir -p $COLORS_DIR
mkdir -p $AUTOLOAD_DIR

#wget https://raw.githubusercontent.com/mpa80/dotfiles/master/vimrc -O $HOME/.vimrc

for PLUGIN in ${PLUGINS[@]}; do
   DIRNAME="$(basename $PLUGIN)"
   git clone https://github.com/$PLUGIN.git $PLUGIN_DIR/$DIRNAME
done

cp $PLUGIN_DIR/vim-pathogen/autoload/pathogen.vim $AUTOLOAD_DIR/pathogen.vim

echo "Vim setup done"
