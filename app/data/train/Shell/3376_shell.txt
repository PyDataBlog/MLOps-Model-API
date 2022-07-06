#/bin/sh

tar --ignore-failed-read -cvf tux-$USER.tar $(find /home/git -type d -name "$USER" 2>/dev/null)
