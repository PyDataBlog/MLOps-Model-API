# Copyright 2015 YP LLC.
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file
BIN_ARGS?=
BIN=docker-events-hook-jenkins

$(BIN): $(BIN).go
	go build -i $(BIN).go

run: $(BIN)
	./$(BIN) $(BIN_ARGS)
