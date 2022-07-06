# Makefile for upward-test

UGLIFIED_BUNDLE := $(BUNDLE:js=min.js)

include node_modules/upward/etc/upward.mk
