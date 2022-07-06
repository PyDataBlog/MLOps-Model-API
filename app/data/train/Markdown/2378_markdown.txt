supportutils-plugin-hae
========

Supportconfig plugin for SUSE Linux Enteprise High Availability
Extension. It is based on a customized version of "hahealth", a tool
to perform basic health check against the core components of a
Linux-HA clusters.

hahealth is, in turn, based on hb_report from where it takes part of
the information it shows up. By default, the plugin creates a hb_report based
on the last day from which it extracts the most relevant facts. The
hb_report created is eventually attached to the supportconfig for
further analysis. This behavior and other default options can be
modified with file /usr/lib/supportconfig/plugins/suseha.conf

Along with the hb_report of the last day, the plugin will create and
attach to the supportconfig also a file named
"basic-health-check-ha.txt" which contains an easy-to-read report of
the checks it performs.
