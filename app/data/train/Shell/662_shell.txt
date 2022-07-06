#!/bin/bash

ROOM="CHANGE_ME"
FROM="CHANGE_ME"
TOKEN="CHANGE_ME"
FORMAT="text"
MESSAGE="CHANGE_ME"
NOTIFY="1"

curl -sS -d "auth_token=$TOKEN&room_id=$ROOM&from=$FROM&message_format=$FORMAT&message=$MESSAGE&notify=$NOTIFY" https://api.hipchat.com/v1/rooms/message
