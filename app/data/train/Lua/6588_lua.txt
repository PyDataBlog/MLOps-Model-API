local websiteURL = "https://bitbucket.org/"
local repoCommitsURL = websiteURL .. "user/project/commits/"

local command = "--reverse --pretty=format:\"%at@%cn@%h@%n%B~\""

local header = [[
<html>
	<head>
		<title>Update Summary</title>
		<link href="style.css" rel="stylesheet">
		<script src="formatter.js"></script>
	</head>
	<body>
		<div class="header">Commit History</div>
		<div class="revision">Revision: %i</div>
		<div class="revision">Version: %s</div>
		<div class="revision">Long version: %s</div>
		<div class="commit_history">]] 
local footer = [[
		</div>
	</body>
</html>]]

function string.trim(s)
	s = s:gsub("[\"]", "")
	s = s:gsub("^%s*", "")
	return (s:gsub("^%s*(.-)%s*$", "%1"))
end
function string.split( s, separator )
	local t = {}
	if string.find(s, separator) then
		for token in string.gmatch( s, "[^" .. separator .. "]+" ) do
			table.insert( t, string.trim(token) )
		end
	else
		return {s}
	end
	return t
end
function parseTags( tags )
	local t = {}
	
	local raw = string.split(tags, ";")
	
	for k,v in pairs(raw) do
		if string.find(v,"=") then
			local variable = string.split(v,"=")
			t[variable[1]] = variable[2]
		else
			t[v] = true
		end
	end
	return t
end
function PrintTable(t)
	for k,v in pairs(t) do
		print("\t[ "..k.." ] => "..tostring(v))
	end
end

local function sliceCommit(v)
	c = string.split(v, "@")
	print(c[4])
	body = string.split( c[4], "#")
	
	local commit = {}
	
	commit.timestamp = c[1]
	commit.username = c[2]
	commit.hash = c[3]
	commit.body = body[1]
	
	if body[2] then
		local tags = parseTags(body[2])
		commit.tags = tags
	end
	
	return commit
end

local function premakeDebug(s)
	if PREMAKE_DEBUG then
		print(s)
	end
end

function getCommits( branch, directory )
	local commits = {}
	directory = directory or "."
	branch = branch or "HEAD"
	
	local gitFind = "git"--[[@echo off&&for %% in (git.exe) do if not [%%~$PATH:x]==[] (git]]
	local process = io.popen( "cd "..directory.." && "..gitFind.." log "..branch.." "..command.."")
	
	local output = process:read("*all")
	process:close()
	
	print(output)

	local commits_string = string.split( output, "~" )
	
	--version counting
	local major, minor, patch = 0, 0, 0
	
	for k,v in pairs( commits_string ) do
		print(v)
		local commit = sliceCommit(v)
		
		premakeDebug("\ncommit: "..commit.hash.."; "..commit.timestamp)
		
		if commit.tags then
			if commit.tags["v"] then --override
				local version = string.split( commit.tags["v"], "." )
				
				major = tonumber(version[1])
				minor = tonumber(version[2])
				patch = tonumber(version[3])
				
				premakeDebug("\tset version")
				
				commit.versionChanged = true
				
			else --default counting system
				if commit.tags["major"] then	
					major = major + 1
					minor = 0
					patch = 0
					premakeDebug("\tincrement major")
					commit.versionChanged = true
				elseif commit.tags["minor"] then
					minor = minor + 1
					patch = 0
					premakeDebug("\tincrement minor")
					commit.versionChanged = true
				elseif commit.tags["patch"] then
					patch = patch + 1
					premakeDebug("\tincrement patch")
					commit.versionChanged = true
				end
			end
		end
		
		commit.version = { --all commits have version, even if it isnt updated
			major = major,
			minor = minor,
			patch = patch
		}
			
		premakeDebug("\tversion: "..major.."."..minor.."."..patch)
		
		commits[k] = commit
	end
	
	return commits
end

local commits = getCommits()

function toHTML(s)
	return s:gsub("[\n]","<br/>")
end


local com = commits[#commits]
local version = com.version
local versionString = string.format("%i.%i.%i", version.major, version.minor, version.patch)

local longVersion = string.format("%s.%s_%s", versionString, os.date("%y%m%d", com.timestamp), com.hash)
local html = string.format(header, #commits, versionString, longVersion)


for i=#commits,1,-1 do
	local v = commits[i]
	local row = [[
		
			<div class="commit-row">
				<ul class="tags-line">
]]
	if v.versionChanged then
		row = row .. [[					<li class="tag v">v]]..v.version.major.."."..v.version.minor.."."..v.version.patch.."</li>\n"
	end
	
	if (v.tags) then
		for k,v in pairs(v.tags) do
			if type(v) == "boolean" then
				row = row .. [[					<li class="tag ]]..k.."\">"..k.."</li>\n"
			elseif k ~= "v" then
				row = row .. [[					<li class="tag ]]..k.."\">"..v.."</li>\n"
			end
		end
	end
	
	row = row ..[[				</ul>
				<div class="meta">
					<div class="timestamp">]]..v.timestamp..[[</div>
					<div class="spacer"></div>
					<div class="hash">commit <a href=]].."\""..repoCommitsURL..v.hash.."\">"..v.hash..[[</a></div>
					<div class="spacer"></div>
					<div class="username">by <a href=]].."\""..websiteURL..v.username.."\">"..v.username..[[</a></div>
				</div>
				<div class="summary">]]..toHTML(v.body)..[[
				</div>
			</div>]]
	html = html .. row
end

html = html .. footer
local file = io.open("out/output.html", "w")
file:write(html)
file:close()
print(html)
	
	
	