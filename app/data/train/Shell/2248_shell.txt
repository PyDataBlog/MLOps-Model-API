gconftool-2 -t string -s /desktop/gnome/url-handlers/digitalcoin/command "java -splash:doesnotexist.png -jar $INSTALL_PATH/d-lite-exe.jar %s"
gconftool-2 -s /desktop/gnome/url-handlers/digitalcoin/needs_terminal false -t bool
gconftool-2 -t bool -s /desktop/gnome/url-handlers/digitalcoin/enabled true