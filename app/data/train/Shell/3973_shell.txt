#!/bin/bash

cd "$(dirname "$BASH_SOURCE")" \
	&& source '../utils.sh'

# TODO
# - xcape to caps lock to esc/ctrl
# 	- http://www.economyofeffort.com/2014/08/11/beyond-ctrl-remap-make-that-caps-lock-key-useful/
# - adjust mouse scrolling speed
#	- https://github.com/pwr/Solaar/issues/158
#	- xinput set-prop "Logitech Performance MX" "Evdev Scrolling Distance" 1 10 10

main() {
	print_info '\n Install Ubuntu packages'
}

main
