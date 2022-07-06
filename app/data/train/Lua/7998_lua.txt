minedexer = {}

local httpstatus = require("httpstatus")
local configor = require("configor")

local template = include("template.lua")

minedexer.index = function(req, res)
	template.generate(res, "Minedexer", {"Hello, world!"})
end

minedexer.update = function(req, res)
	local params = req:params()

	local addr = assert(params.addr)
	local desc = assert(params.desc)
	local port = assert(tonumber(params.port))
	local plys = assert(tonumber(params.plys))
	local maxp = assert(tonumber(params.maxp))

	print(string.format("%s:%d (%d/%d) - %s", addr, port, plys, maxp, desc))
	error("Not yet implimented")
end

reqs.AddPattern("*", "/minedexer/", minedexer.index)
reqs.AddPattern("*", "/minedexer/update", minedexer.update)

