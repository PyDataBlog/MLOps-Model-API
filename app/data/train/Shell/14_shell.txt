#!/bin/bash
cd "$(dirname "$BASH_SOURCE")" || {
    echo "Error getting script directory" >&2
    exit 1
}
./hallo.command
sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist