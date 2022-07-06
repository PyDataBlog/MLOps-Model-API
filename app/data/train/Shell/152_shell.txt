#!/bin/bash
exec puppet agent --no-daemonize --server puppet --logdest console --verbose
