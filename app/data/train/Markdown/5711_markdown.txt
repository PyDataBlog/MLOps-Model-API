# How to use dotmaccore

Pull the dofiles from [mathias bynens](https://github.com/mathiasbynens/dotfiles.git)
This is geared towards jscript/node, so make sure to reset ruby to home
cd dotfiles && source bootstrap.sh

Now clone dotmaccore
cd dotmaccore && bootstrap.sh

Run the git setup if it's not already set:

    git config --global user.name "Your Name"
    git config --global user.email you@example.com

See my current brew listing at the bottom

# Git config

See dottoolcore.git

# Link iCloud

    ln -s ~/Library/Mobile\ Documents/com\~apple\~CloudDocs ~/iCloud

# Mac Commands

Sizes of directories    `tree -du -L 1`  
and                     `du -d1 ~`  
System log cleanup      `sudo periodic daily weekly monthly`  

Search files via spotlight  `mdfind <filename>`  
Show file info              `mdshow <filename>`  

List git folders        `find . -name .git -type d -print`  


### Checksum for downloads on Mac
    - shasum <fn>
    - openssl sha1 <drop file>


###Install homebrew

ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"

from:

http://mxcl.github.io/homebrew/

Install fine. Comment to run:
brew doctor
brwe help
###Old Path

PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin

###New Path
PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin

###Permanent path on OS X

Create ~/.bash_profile with:
`export PATH=/usr/local/bin:$PATH`

# Trashed my work mashine's mbr!!!


fixboot c:
fixmbr

Bootrec.exe /FixBoot
Bootrec.exe /FixMBR

FixMbr

MbrFix /drive 0 fixmbr /win7



# Pull all git masters

### [update\_git\_repos.sh](https://gist.github.com/douglas/1287372)

#### Notes

Dangerous! Use with care

- This will pull master over the current checked out branch
- Submodules will go out of sync

#### Code

    #!/bin/bash

    # store the current dir
    CUR_DIR=$(pwd)

    # Let the person running the script know what's going on.
    echo -e "\n\033[1mPulling in latest changes for all repositories...\033[0m\n"

    # Find all git repositories and update it to the master latest revision
    for i in $(find . -name ".git" | cut -c 3-); do
        echo -e "";
        echo -e "\033[33m"+$i+"\033[0m";

        # We have to go to the .git parent directory to call the pull command
        cd "$i";
        cd ..;

        # finally pull
        git pull origin master;

        # lets get back to the CUR_DIR
        cd $CUR_DIR
    done

    echo -e "\n\033[32mComplete!\033[0m\n"


# Brew listing


ack
apple-gcc42
asciidoc
autoconf
automake
bash
bash-completion
bash-git-prompt
bfg
binutils
binwalk
boost
brew-cask
cairo
cifer
cloog
cmake
coreutils
cscope
ctags
curl
czmq
dex2jar
dns2tcp
docbook
docbook-xsl
faac
fcrackzip
ffmpeg
findutils
fontconfig
fontforge
foremost
freetype
gcc
gdbm
gettext
git
git-flow
glib
gmp
gnu-getopt
gnu-sed
gobject-introspection
grep
harfbuzz
hashpump
hydra
icu4c
imagemagick
irssi
isl
john
jpeg
knock
lame
libdnet
libevent
libffi
libgpg-error
libidn
libksba
libmagic
libmpc
libpng
libsodium
libtiff
libtool
libvo-aacenc
libxml2
libyaml
little-cms2
lua
lynx
macvim
mercurial
moreutils
mpfr
mutt
mysql
narwhal
ngrep
nmap
node
openjpeg
openssl
p7zip
pandoc
pango
pcre
pdf2htmlex
pigz
pixman
pkg-config
pngcheck
poppler
pstree
pv
pyside
python
python3
qt
readline
rename
rhino
ringojs
screen
shiboken
socat
sqlite
sqlmap
ssdeep
swig
tcpflow
tcpreplay
tcptrace
texi2html
tig
tmux
tokyo-cabinet
tree
ttfautohint
ucspi-tcp
vim
vimpager
webkit2png
wget
x264
xmlto
xvid
xz
yasm
youtube-dl
zeromq
zopfli
