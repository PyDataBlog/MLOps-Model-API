# clipmenu-im

`clipmenu-im(proved)` is a simple clipboard history managment script in `bash`.

This version allows you to manage (add, remove, purge) entries in your clipboard history.

## Features:

- **copy** an entry from history to clipboard
- **remove** entries from history on demand (dude, I missed this one)
- **autoremove empty** or blank entries (new!)
- **limit duplicates** creation (in progress)
- **purge** history on demand (new!)
- option to use **cli-based menu** or **gtk-based menu** instead of `dmenu` (new!)
- accept any `dmenu` options on commandline 

## Usage:

    clipmenu-im [-c] [-d|-D] [any dmenu options]

First start `clipmenud` daemon:

    clipmenud &

Then use `clipmenu-im` to **copy** any entry from history to clipboard:

    clipmenu-im

now you can paste it the usual way.

To **delete** any entry from the history:

    clipmenu-im -d

It will let you choose lines to be deleted multiple times, when you are done press `C-c`.

If you want to **purge** the history, use:

    clipmenu-im -D

If you would like to use `clipmenu-im` with **cli-based menu** instead of `dmenu` add `-c` switch (`-z` for `zenity` dialog) to any of the above commands, eg:

    clipmenu-im -c -d

will let you delete entries using text menu in terminal.

### Dependencies:

[dmenu](http://tools.suckless.org/dmenu) (optional, recommended), `xclip`, `xsel`, zenity (optional, only if you want gtk gui)

under Debian you can get them with:

    sudo apt-get install suckless-tools xclip xsel zenity

### TODO:

- use pure `bash`, remove all unnecessary calls to external tools (in progress, not urgent)
- some dmenu "skinning" depending on what we are doing (copy/delete) (in progress, not urgent)
- purging history by sending SIGHUP to `clipmenud` seems more 'standard' (in progress, not urgent)

## Credits:

The `clipmenu-im` is the extended and rewritten version of [clipmenu](https://github.com/cdown/clipmenu) by cdown.
The `clipmenud` is the original code by cdown. 

## Copyright:

Licensed under GNU General Public License v3 (see License file).
Copyright (C) 2016 Tomasz Winiarski
