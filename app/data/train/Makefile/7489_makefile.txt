install:
	sudo apt-get install haskell-platform libncurses5-dev -y
	cabal update
	cabal install Cabal cabal-install
	cabal install purescript
	ln -s ~/.cabal/bin/psc ~/bin/psc
	ln -s ~/.cabal/bin/psci ~/bin/psci
	npm install -g grunt grunt-cli grunt-init bower
	mkdir ~/.grunt-init
	git clone https://github.com/purescript-contrib/grunt-init-purescript.git ~/.grunt-init/purescript

setup:
	grunt-init purescript --force
	npm install
	bower update
