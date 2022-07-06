
export GYP_GENERATORS=ninja

.PHONY: sync
sync:
	@./depot_tools/gclient sync

.PHONY: gyp
gyp:
	@cd src && ./build/gyp_chromium content/content.gyp --no-circular-check

.PHONY: build
build:
	@cd src && ninja -C out/Debug nw -j6

.PHONY: run
run:
	open src/out/Debug/node-webkit.app
