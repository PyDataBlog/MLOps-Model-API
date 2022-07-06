#!/bin/sh

# Deprecated by gruvbox
# Install desert256
mkdir -p ~/.vim/colors
wget -O ~/.vim/colors/desert256.vim https://raw.githubusercontent.com/clarkli86/desert256.vim/master/colors/desert256.vim

# Deprecated by the usage of vim-lsp's LspReferences which is smarter
# Install cscope plugin
sudo apt install cscope -y
mkdir -p ~/.vim/plugin
wget -O ~/.vim/plugin/cscope_maps.vim http://cscope.sourceforge.net/cscope_maps.vim

# Install vim-plug
sudo apt install curl -y
curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Get vimrc.local
wget https://raw.githubusercontent.com/clarkli86/vimrc/master/vimrc.local
cp vimrc.local ~/.vimrc
cp vimrc.local ~/.gvimrc
mkdir -p ~/.config/nvim/
cp vimrc.local ~/.config/nvim/init.vim

# Configure git
git config --global core.editor nvim

# Install vim
sudo apt-get install vim vim-gtk

# Install tmux
sudo apt-get install tmux

# Install zsh
sudo apt-get install zsh

# Install on-my-zsh
wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh

# Change on-my-zsh theme to random
# sed 's/ZSH_THEME=.*/ZSH_THEME=random/g' ~/.zshrc > ~/.zshrc
echo 'command_not_found_handler() {/usr/lib/command-not-found "$1"}' >> ~/.zshrc

# Change to zsh
chsh -s /usr/bin/zsh
