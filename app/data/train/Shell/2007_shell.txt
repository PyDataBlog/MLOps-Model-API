#!/bin/bash

# newrelic plugin
if [ ! -z "$NEWRELIC_LICENSE" ]; then
        j2 /etc/newrelic/newrelic-plugin-agent.tmpl | sponge /etc/newrelic/newrelic-plugin-agent.cfg
	echo "******** File newrelic-plugin-agent.cfg rendered START ***********"
        cat /etc/newrelic/newrelic-plugin-agent.cfg
	echo "******** File newrelic-plugin-agent.cfg rendered END ***********"
        sed -i s/autostart=false/autostart=true/ /etc/supervisor/conf.d/newrelic-plugin.conf
fi
