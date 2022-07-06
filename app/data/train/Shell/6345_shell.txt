#!/usr/bin/env zsh
################################################################################
# Install and configure Go (the programming language) on a Mac with Homebrew
#

echo
echo "Installing the Go language using Homebrew..."
sudo brew install go

echo "Making a GOPATH home directory for Go..."
mkdir -p ~/go/bin

echo "Modifying ~.zshrc to add the go/bin and GOPATH..."
echo "export PATH=\$PATH:\$HOME/go/bin" >> ~/.zshrc
echo "export GOPATH=\$HOME/go" >> ~/.zshrc

echo "Sourcing the changes..."
source $HOME/.zshrc

echo "Installing vet and godoc..."
go get golang.org/x/tools/cmd/vet
go get golang.org/x/tools/cmd/godoc

echo "...done!"
echo
