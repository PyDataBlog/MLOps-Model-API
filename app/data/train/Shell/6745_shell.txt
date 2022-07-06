#!/bin/bash
#
#  Test qtshell on macOS
#

export QT_PLUGIN_PATH=$(pwd)
export QML_IMPORT_TRACE=1

cp ./qtshell.app/Contents/MacOS/qtshell macshell
./macshell
