#!/usr/bin/env julia

cmd = `th train.lua -input_h5 test.h5 -input_json test.json`
println(cmd)
run(cmd)
