#!/bin/bash
###############################################################################
# Author: Travis Goldie
# Purpose: Send notifications for active Taskwarrior tasks
###############################################################################
# Based on Taskwarrior-notifications at https://github.com/flickerfly/taskwarrior-notifications

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "$CURRENT_DIR/helpers.sh"
trap cleanup SIGINT SIGTERM

# XXX Do not quote EOF, so that variable substitution happens
read -r -d '' USAGE << EOF
Send notification for active Taskwarrior tasks.

# Can easily be used in cron job
# Show notifcation every 30 mins (Monday - Friday)
*/30 * * * 1-5 DISPLAY=:0.0 "${CURRENT_DIR}/tasknotify.sh"

Optional Arguments:
-a    Show active tasks (this is the default behavior)
-l    List all tasks
-h    Print this help and exit
-n    Test run
-s    Run short list (task ls)

EOF

# Variables
TASK=/usr/local/bin/task
CMD="active"
HEADER=
MSG=

dryrun=
optstring=alhmns
while getopts $optstring opt ; do
    case $opt in
        a) CMD="active" ;;
        l) CMD="list" ;;
        h) echo "$USAGE" ; exit 255 ;;
        m) CMD="min" ;;
        n) dryrun=true ;;
        s) CMD="ls" ;;
    esac
done

get_active_header_msg()
{
    local count=$( $TASK active | wc -l )
    HEADER="Active Tasks: $count"

    if [[ count -gt "1" ]] ; then
        local active_tasks=$( $TASK active | tail -n +4 | head -n -1 )
        MSG="$active_tasks"
    else
        MSG="No active tasks at this time"
    fi
}

get_list_header_msg()
{
    local count=$( $TASK list | tail -n +4 | head -n -1 | wc -l )
    HEADER="Current Tasks: $count"

    if [[ count -gt "1" ]] ; then
        local list_tasks=$( $TASK list | tail -n +4 | head -n -1 )
        MSG="$list_tasks"
    else
        MSG="No tasks at this time"
    fi
}

get_ls_header_msg()
{
    local count=$( $TASK ls | tail -n +4 | head -n -1 | wc -l )
    HEADER="Current Tasks: $count"

    if [[ count -gt "1" ]] ; then
        local list_tasks=$( $TASK ls | tail -n +4 | head -n -1 )
        MSG="$list_tasks"
    else
        MSG="No tasks at this time"
    fi
}

get_min_header_msg()
{
    local count=$( $TASK minimal | tail -n +4 | head -n -1 | wc -l )
    HEADER="Current Tasks: $count"

    if [[ count -gt "1" ]] ; then
        local list_tasks=$( $TASK minimal | tail -n +4 | head -n -1 )
        MSG="$list_tasks"
    else
        MSG="No tasks at this time"
    fi
}

cut_columns()
{
    local msg=""
    for line in ${MSG[@]} ; do
        msg+=$(echo $line | cut -d' ' -f 7-)
    done

    MSG=$msg
}

main()
{
    $TASK sync > /dev/null 2>&1

    if [[ $CMD == "active" ]] ; then
        get_active_header_msg
    elif [[ $CMD == "list" ]] ; then
        get_list_header_msg
    elif [[ $CMD == "ls" ]] ; then
        get_list_header_msg
    elif [[ $CMD == "min" ]] ; then
        get_min_header_msg
    fi

    echo $MSG

	notify-send --hint=int:transient:100 -u normal -t 800  "${HEADER}" "${MSG}"
}
main
