#!/bin/bash

usage() {
	echo "Usage:"
	echo "	start.sh <-c conf_name> [-p prop_file]"
	echo "Description:"
	echo "	conf_name: the dir name in conf directory. eg: licai-cifdb"
	echo "	prop_file: the path to hsb.properties. eg: /home/work/soft/sh-key.properties"
	exit -1;
}

CONFIG=""
PROPERTIES=""
CURRENT_DIR="${PWD}"

while getopts "c:p:" arg
do
	case $arg in
		c) CONFIG=$OPTARG;;
		p) PROPERTIES=$OPTARG;;
		?) usage;;
	esac
done

if [ "$CONFIG"x = ""x ]; then
	usage
fi

if [ -f "$PROPERTIES" ]; then
	if [ -f "${CURRENT_DIR}/../conf/${CONFIG}/hsb.properties" ]; then
		echo -e "\033[33m Backup hsb.properties \033[0m"
		rm -f ${CURRENT_DIR}/../conf/${CONFIG}/hsb.properties.bak
		mv ${CURRENT_DIR}/../conf/${CONFIG}/hsb.properties ${CURRENT_DIR}/../conf/${CONFIG}/hsb.properties.bak
	fi	
	echo -e "\033[33m Link hsb.properties to $PROPERTIES \033[0m"
	ln -s $PROPERTIES ${CURRENT_DIR}/../conf/${CONFIG}/hsb.properties
fi

if [ -d "${CURRENT_DIR}/../conf/${CONFIG}" ]; then
	pid=`ps -ef | grep HeisenbergStartup | grep ${CONFIG} | grep -v grep | awk '{print $2}'`
	
	if [ ! -z $pid ]; then
		echo -e "\033[33m Killing old process ${pid} \033[0m"
		kill -9 $pid
		sleep 5s
	fi
	
	echo -e "\033[33m Starting Heisenberg using ${CURRENT_DIR}/../conf/${CONFIG} \033[0m"
	mkdir -p ~/var/heisenberg/${CONFIG}/logs
	sh ${CURRENT_DIR}/startup.sh -c ${CURRENT_DIR}/../conf/${CONFIG} -l ~/var/heisenberg/${CONFIG}
else
	echo -e "\033[31m Config ${CONFIG} NOT found!! \033[0m"
	exit -1
fi
