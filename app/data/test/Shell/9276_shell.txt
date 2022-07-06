VERSION=0.0.1
ITERATION=1
wget -O dot.shell_integration.bash https://iterm2.com/misc/bash_startup.in

echo "-s file
-t deb
-f
-v $VERSION
--iteration $ITERATION
--license 'Mozilla Public License'
--vendor iTerm2
-m 'Jeff Hubbard <lord2800@gmail.com>'
--url 'https://iterm2.com/shell_integration.html'
--deb-user root
--deb-group root
" > .fpm

fpm \
	-n iterm2-shell-integration \
	--description 'iTerm2 Shell Integration.' \
	--after-install iterm.postinst \
	--before-remove iterm.postrm \
	-s dir \
	shell_integration.bash=/etc/skel/shell_integration.bash
