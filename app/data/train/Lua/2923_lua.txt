--[[
 The systems loader will handle loading all of the systems critical to making this gamemode work.
]]--

--A table for holding loaded systems information.
local LoadedSystems = {};

--Table for the directories list.
local SystemDirs = {};
 
--Function for detecting directories..
--This will load all the system directories into the loader.
local function findDirectories()
	local _, dir = file.Find(SLRP.DIR .. "*", "LUA");
	SystemDirs = dir;
end

--Function to check if a particular system has already been loaded.
local function isSystemLoaded( systemFName )

	local flag = false;

	for _,v in pairs(LoadedSystems) do
		if v == systemFName then
			flag = true;
			break;
		end
	end

	return flag;
end

--Function for loading init file inside of listed directories.
local function loadSystemInits()
	if SystemDirs == nil then
		MsgN("System directories are empty!");
		return;
	else
		for _,v in pairs(SystemDirs) do
			if not isSystemLoaded(v) then
				include(SLRP.DIR .. v .. "/shared.lua");
				if SERVER then
					AddCSLuaFile(SLRP.DIR .. v .. "/shared.lua");
				end

				table.insert(LoadedSystems, v);
			end
		end
	end
end

--This function will require a system prior to its loading order.
--This is useful for including a system that is required by another.
function SLRP.Require( systemFName )
	if not isSystemLoaded( systemFName ) then
		include(SLRP.DIR .. systemFName .. "/shared.lua");
		if SERVER then
			AddCSLuaFile(SLRP.DIR .. systemFName .. "/shared.lua");
		end
		table.insert(LoadedSystems, systemFName);
	end
end

-------------------------------BASE SYSTEM---------------------------------

local system = {};

--Basic system information that the systems will inherit.
--These systems will inherit these properties. 
--FNAME needs to be equal to the folder name.
system.FNAME = "overrideme";
system.NAME = "overrideme";
system.DIR = SLRP.DIR .. "/" .. system.FNAME;

--This is the basic loading function for the systems.

--Here is the structure:
--[[
	* The folder structure:
		cl - Client files go here
		sv - Server files go here
		sh - Shared files go here
]]--

system.svLoadFunction = function() 
	MsgN("Server ["..system.NAME.."]: ");

	--Client
	for _,v in pairs(file.Find(system.DIR .. "cl/*.lua", "LUA")) do
		MsgN("	-Pushing to client - " .. v);
		AddCSLuaFile(system.DIR .. "cl/" ..v);
	end
	--Server
	for _,v in pairs(file.Find(system.DIR .. "sv/*.lua", "LUA")) do
		MsgN("	-Including to server - " .. v);
		include(system.DIR .. "sv/" .. v);
	end
	--Shared
	for _,v in pairs(file.Find(system.DIR .. "sh/*.lua", "LUA")) do
		MsgN("	-Including to server and pushing to client - " .. v);
		include(system.DIR .. "sh/" .. v);
		AddCSLuaFile(system.DIR .. "sh/" .. v);
	end
end

system.clLoadFunction = function() 
	MsgN("Client ["..system.NAME.."]: ");

	--Client
	for _,v in pairs(file.Find(system.DIR .. "cl/*.lua", "LUA")) do
		MsgN("	-Including to client - " .. v);
		include(system.DIR .. "cl/" ..v);
	end
	
	--Shared
	for _,v in pairs(file.Find(system.DIR .. "sh/*.lua", "LUA")) do
		MsgN("	-Including to client - " .. v);
		include(system.DIR .. "sh/" .. v);
	end
end

SLRP.SystemTemplate = system;
findDirectories();
loadSystemInits();
