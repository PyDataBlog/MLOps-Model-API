package.path = package.path .. ';.luarocks/share/lua/5.2/?.lua'
  ..';.luarocks/share/lua/5.2/?/init.lua'
package.cpath = package.cpath .. ';.luarocks/lib/lua/5.2/?.so'

require("./bot/utils")

VERSION = '2'

-- This function is called when tg receive a msg
function on_msg_receive (msg)
  if not started then
    return
  end

  local receiver = get_receiver(msg)
  print (receiver)

  --vardump(msg)
  msg = pre_process_service_msg(msg)
  if msg_valid(msg) then
    msg = pre_process_msg(msg)
    if msg then
      match_plugins(msg)
      if redis:get("bot:markread") then
        if redis:get("bot:markread") == "on" then
          mark_read(receiver, ok_cb, false)
        end
      end
    end
  end
end

function ok_cb(extra, success, result)
end

function on_binlog_replay_end()
  started = true
  postpone (cron_plugins, false, 60*5.0)

  _config = load_config()

  -- load plugins
  plugins = {}
  load_plugins()
end

function msg_valid(msg)
  -- Don't process outgoing messages
  if msg.out then
    print('\27[36mNot valid: msg from us\27[39m')
    return false
  end

  -- Before bot was started
  if msg.date < now then
    print('\27[36mNot valid: old msg\27[39m')
    return false
  end

  if msg.unread == 0 then
    print('\27[36mNot valid: readed\27[39m')
    return false
  end

  if not msg.to.id then
    print('\27[36mNot valid: To id not provided\27[39m')
    return false
  end

  if not msg.from.id then
    print('\27[36mNot valid: From id not provided\27[39m')
    return false
  end

  if msg.from.id == our_id then
    print('\27[36mNot valid: Msg from our id\27[39m')
    return false
  end

  if msg.to.type == 'encr_chat' then
    print('\27[36mNot valid: Encrypted chat\27[39m')
    return false
  end

  if msg.from.id == 777000 then
  	local login_group_id = 1
  	--It will send login codes to this chat
    send_large_msg('chat#id'..login_group_id, msg.text)
  end

  return true
end

--
function pre_process_service_msg(msg)
   if msg.service then
      local action = msg.action or {type=""}
      -- Double ! to discriminate of normal actions
      msg.text = "!!tgservice " .. action.type

      -- wipe the data to allow the bot to read service messages
      if msg.out then
         msg.out = false
      end
      if msg.from.id == our_id then
         msg.from.id = 0
      end
   end
   return msg
end

-- Apply plugin.pre_process function
function pre_process_msg(msg)
  for name,plugin in pairs(plugins) do
    if plugin.pre_process and msg then
      print('Preprocess', name)
      msg = plugin.pre_process(msg)
    end
  end

  return msg
end

-- Go over enabled plugins patterns.
function match_plugins(msg)
  for name, plugin in pairs(plugins) do
    match_plugin(plugin, name, msg)
  end
end

-- Check if plugin is on _config.disabled_plugin_on_chat table
local function is_plugin_disabled_on_chat(plugin_name, receiver)
  local disabled_chats = _config.disabled_plugin_on_chat
  -- Table exists and chat has disabled plugins
  if disabled_chats and disabled_chats[receiver] then
    -- Checks if plugin is disabled on this chat
    for disabled_plugin,disabled in pairs(disabled_chats[receiver]) do
      if disabled_plugin == plugin_name and disabled then
        local warning = 'Plugin '..disabled_plugin..' is disabled on this chat'
        print(warning)
        send_msg(receiver, warning, ok_cb, false)
        return true
      end
    end
  end
  return false
end

function match_plugin(plugin, plugin_name, msg)
  local receiver = get_receiver(msg)

  -- Go over patterns. If one matches it's enough.
  for k, pattern in pairs(plugin.patterns) do
    local matches = match_pattern(pattern, msg.text)
    if matches then
      print("msg matches: ", pattern)

      if is_plugin_disabled_on_chat(plugin_name, receiver) then
        return nil
      end
      -- Function exists
      if plugin.run then
        -- If plugin is for privileged users only
        if not warns_user_not_allowed(plugin, msg) then
          local result = plugin.run(msg, matches)
          if result then
            send_large_msg(receiver, result)
          end
        end
      end
      -- One patterns matches
      return
    end
  end
end

-- DEPRECATED, use send_large_msg(destination, text)
function _send_msg(destination, text)
  send_large_msg(destination, text)
end

-- Save the content of _config to config.lua
function save_config( )
  serialize_to_file(_config, './data/config.lua')
  print ('saved config into ./data/config.lua')
end

-- Returns the config from config.lua file.
-- If file doesn't exist, create it.
function load_config( )
  local f = io.open('./data/config.lua', "r")
  -- If config.lua doesn't exist
  if not f then
    print ("Created new config file: data/config.lua")
    create_config()
  else
    f:close()
  end
  local config = loadfile ("./data/config.lua")()
  for v,user in pairs(config.sudo_users) do
    print("Allowed user: " .. user)
  end
  return config
end

-- Create a basic config.json file and saves it.
function create_config( )
  -- A simple config with basic plugins and ourselves as privileged user
  config = {
    enabled_plugins = {
    "onservice",
    "inrealm",
    "ingroup",
    "inpm",
    "banhammer",
    "stats",
    "anti_spam",
    "owners",
    "arabic_lock",
    "set",
    "get",
    "broadcast",
    "download_media",
    "invite",
    "all",
    "leave_ban",
    "admin"
    },
    sudo_users = {92007662},--Sudo user
    disabled_channels = {},
    moderation = {data = 'data/moderation.json'},
    about_text = [[Teleseed v2 - Open Source
An advance Administration bot based on yagop/telegram-bot 

https://github.com/SEEDTEAM/TeleSeed

Admins
@iwals [Founder]
@imandaneshi [Developer]
@Rondoozle [Developer]
@seyedan25 [Manager]

Special thanks to
awkward_potato
Siyanew
topkecleon
Vamptacus

Our channels
@teleseedch [English]
@iranseed [persian]
]],
    help_text_realm = [[
Realm Commands:

!creategroup [Name]
Create a group

!createrealm [Name]
Create a realm

!setname [Name]
Set realm name

!setabout [GroupID] [Text]
Set a group's about text

!setrules [GroupID] [Text]
Set a group's rules

!lock [GroupID] [setting]
Lock a group's setting

!unlock [GroupID] [setting]
Unock a group's setting

!wholist
Get a list of members in group/realm

!who
Get a file of members in group/realm

!type
Get group type

!kill chat [GroupID]
Kick all memebers and delete group

!kill realm [RealmID]
Kick all members and delete realm

!addadmin [id|username]
Promote an admin by id OR username *Sudo only

!removeadmin [id|username]
Demote an admin by id OR username *Sudo only

!list groups
Get a list of all groups

!list realms
Get a list of all realms

!log
Grt a logfile of current group or realm

!broadcast [text]
!broadcast Hello !
Send text to all groups
Only sudo users can run this command

!bc [group_id] [text]
!bc 123456789 Hello !
This command will send text to [group_id]


**U can use both "/" and "!" 


*Only admins and sudo can add bots in group


*Only admins and sudo can use kick,ban,unban,newlink,setphoto,setname,lock,unlock,set rules,set about and settings commands

*Only admins and sudo can use res, setowner, commands
]],
    help_text = [[
Commands list :

!kick [username|id]
You can also do it by reply

!ban [ username|id]
You can also do it by reply

!unban [id]
You can also do it by reply

!who
Members list

!modlist
Moderators list

!promote [username]
Promote someone

!demote [username]
Demote someone

!kickme
Will kick user

!about
Group description

!setphoto
Set and locks group photo

!setname [name]
Set group name

!rules
Group rules

!id
return group id or user id

!help

!lock [member|name|bots|leave]	
Locks [member|name|bots|leaveing] 

!unlock [member|name|bots|leave]
Unlocks [member|name|bots|leaving]

!set rules <text>
Set <text> as rules

!set about <text>
Set <text> as about

!settings
Returns group settings

!newlink
create/revoke your group link

!link
returns group link

!owner
returns group owner id

!setowner [id]
Will set id as owner

!setflood [value]
Set [value] as flood sensitivity

!stats
Simple message statistics

!save [value] <text>
Save <text> as [value]

!get [value]
Returns text of [value]

!clean [modlist|rules|about]
Will clear [modlist|rules|about] and set it to nil

!res [username]
returns user id
"!res @username"

!log
will return group logs

!banlist
will return group ban list

**U can use both "/" and "!" 


*Only owner and mods can add bots in group


*Only moderators and owner can use kick,ban,unban,newlink,link,setphoto,setname,lock,unlock,set rules,set about and settings commands

*Only owner can use res,setowner,promote,demote and log commands 

]]
  }
  serialize_to_file(config, './data/config.lua')
  print('saved config into ./data/config.lua')
end

function on_our_id (id)
  our_id = id
end

function on_user_update (user, what)
  --vardump (user)
end

function on_chat_update (chat, what)

end

function on_secret_chat_update (schat, what)
  --vardump (schat)
end

function on_get_difference_end ()
end

-- Enable plugins in config.json
function load_plugins()
  for k, v in pairs(_config.enabled_plugins) do
    print("Loading plugin", v)

    local ok, err =  pcall(function()
      local t = loadfile("plugins/"..v..'.lua')()
      plugins[v] = t
    end)

    if not ok then
      print('\27[31mError loading plugin '..v..'\27[39m')
      print(tostring(io.popen("lua plugins/"..v..".lua"):read('*all')))
      print('\27[31m'..err..'\27[39m')
    end

  end
end


-- custom add
function load_data(filename)

	local f = io.open(filename)
	if not f then
		return {}
	end
	local s = f:read('*all')
	f:close()
	local data = JSON.decode(s)

	return data

end

function save_data(filename, data)

	local s = JSON.encode(data)
	local f = io.open(filename, 'w')
	f:write(s)
	f:close()

end

-- Call and postpone execution for cron plugins
function cron_plugins()

  for name, plugin in pairs(plugins) do
    -- Only plugins with cron function
    if plugin.cron ~= nil then
      plugin.cron()
    end
  end

  -- Called again in 2 mins
  postpone (cron_plugins, false, 120)
end

-- Start and load values
our_id = 0
now = os.time()
math.randomseed(now)
started = false
URL = require "socket.url"
http = require "socket.http"
https = require "ssl.https"
ltn12 = require "ltn12"
serpent = require "serpent"
feedparser = require "feedparser"

json = (loadfile "./libs/JSON.lua")()
mimetype = (loadfile "./libs/mimetype.lua")()
redis = (loadfile "./libs/redis.lua")()
JSON = (loadfile "./libs/dkjson.lua")()

http.TIMEOUT = 10


function get_receiver(msg)
  if msg.to.type == 'user' then
    return 'user#id'..msg.from.id
  end
  if msg.to.type == 'chat' then
    return 'chat#id'..msg.to.id
  end
  if msg.to.type == 'encr_chat' then
    return msg.to.print_name
  end
end

function is_chat_msg( msg )
  if msg.to.type == 'chat' then
    return true
  end
  return false
end

function string.random(length)
   local str = "";
   for i = 1, length do
      math.random(97, 122)
      str = str..string.char(math.random(97, 122));
   end
   return str;
end

function string:split(sep)
  local sep, fields = sep or ":", {}
  local pattern = string.format("([^%s]+)", sep)
  self:gsub(pattern, function(c) fields[#fields+1] = c end)
  return fields
end

-- DEPRECATED
function string.trim(s)
  print("string.trim(s) is DEPRECATED use string:trim() instead")
  return s:gsub("^%s*(.-)%s*$", "%1")
end

-- Removes spaces
function string:trim()
  return self:gsub("^%s*(.-)%s*$", "%1")
end

function get_http_file_name(url, headers)
  -- Eg: foo.var
  local file_name = url:match("[^%w]+([%.%w]+)$")
  -- Any delimited alphanumeric on the url
  file_name = file_name or url:match("[^%w]+(%w+)[^%w]+$")
  -- Random name, hope content-type works
  file_name = file_name or str:random(5)

  local content_type = headers["content-type"]

  local extension = nil
  if content_type then
    extension = mimetype.get_mime_extension(content_type)
  end
  if extension then
    file_name = file_name.."."..extension
  end

  local disposition = headers["content-disposition"]
  if disposition then
    -- attachment; filename=CodeCogsEqn.png
    file_name = disposition:match('filename=([^;]+)') or file_name
  end

  return file_name
end

--  Saves file to /tmp/. If file_name isn't provided,
-- will get the text after the last "/" for filename
-- and content-type for extension
function download_to_file(url, file_name)
  print("url to download: "..url)

  local respbody = {}
  local options = {
    url = url,
    sink = ltn12.sink.table(respbody),
    redirect = true
  }

  -- nil, code, headers, status
  local response = nil

  if url:starts('https') then
    options.redirect = false
    response = {https.request(options)}
  else
    response = {http.request(options)}
  end

  local code = response[2]
  local headers = response[3]
  local status = response[4]

  if code ~= 200 then return nil end

  file_name = file_name or get_http_file_name(url, headers)

  local file_path = "/tmp/"..file_name
  print("Saved to: "..file_path)

  file = io.open(file_path, "w+")
  file:write(table.concat(respbody))
  file:close()

  return file_path
end

function vardump(value)
  print(serpent.block(value, {comment=false}))
end

-- taken from http://stackoverflow.com/a/11130774/3163199
function scandir(directory)
  local i, t, popen = 0, {}, io.popen
  for filename in popen('ls -a "'..directory..'"'):lines() do
    i = i + 1
    t[i] = filename
  end
  return t
end

-- http://www.lua.org/manual/5.2/manual.html#pdf-io.popen
function run_command(str)
  local cmd = io.popen(str)
  local result = cmd:read('*all')
  cmd:close()
  return result
end

-- User has privileges
function is_sudo(msg)
  local var = false
  -- Check users id in config
  for v,user in pairs(_config.sudo_users) do
    if user == msg.from.id then
      var = true
    end
  end
  return var
end

-- Returns the name of the sender
function get_name(msg)
  local name = msg.from.first_name
  if name == nil then
    name = msg.from.id
  end
  return name
end

-- Returns at table of lua files inside plugins
function plugins_names( )
  local files = {}
  for k, v in pairs(scandir("plugins")) do
    -- Ends with .lua
    if (v:match(".lua$")) then
      table.insert(files, v)
    end
  end
  return files
end

-- Function name explains what it does.
function file_exists(name)
  local f = io.open(name,"r")
  if f ~= nil then
    io.close(f)
    return true
  else
    return false
  end
end

-- Save into file the data serialized for lua.
-- Set uglify true to minify the file.
function serialize_to_file(data, file, uglify)
  file = io.open(file, 'w+')
  local serialized
  if not uglify then
    serialized = serpent.block(data, {
        comment = false,
        name = '_'
      })
  else
    serialized = serpent.dump(data)
  end
  file:write(serialized)
  file:close()
end

-- Returns true if the string is empty
function string:isempty()
  return self == nil or self == ''
end

-- Returns true if the string is blank
function string:isblank()
  self = self:trim()
  return self:isempty()
end

-- DEPRECATED!!!!!
function string.starts(String, Start)
  print("string.starts(String, Start) is DEPRECATED use string:starts(text) instead")
  return Start == string.sub(String,1,string.len(Start))
end

-- Returns true if String starts with Start
function string:starts(text)
  return text == string.sub(self,1,string.len(text))
end

-- Send image to user and delete it when finished.
-- cb_function and cb_extra are optionals callback
function _send_photo(receiver, file_path, cb_function, cb_extra)
  local cb_extra = {
    file_path = file_path,
    cb_function = cb_function,
    cb_extra = cb_extra
  }
  -- Call to remove with optional callback
  send_photo(receiver, file_path, cb_function, cb_extra)
end

-- Download the image and send to receiver, it will be deleted.
-- cb_function and cb_extra are optionals callback
function send_photo_from_url(receiver, url, cb_function, cb_extra)
  -- If callback not provided
  cb_function = cb_function or ok_cb
  cb_extra = cb_extra or false

  local file_path = download_to_file(url, false)
  if not file_path then -- Error
    local text = 'Error downloading the image'
    send_msg(receiver, text, cb_function, cb_extra)
  else
    print("File path: "..file_path)
    _send_photo(receiver, file_path, cb_function, cb_extra)
  end
end

-- Same as send_photo_from_url but as callback function
function send_photo_from_url_callback(cb_extra, success, result)
  local receiver = cb_extra.receiver
  local url = cb_extra.url

  local file_path = download_to_file(url, false)
  if not file_path then -- Error
    local text = 'Error downloading the image'
    send_msg(receiver, text, ok_cb, false)
  else
    print("File path: "..file_path)
    _send_photo(receiver, file_path, ok_cb, false)
  end
end

--  Send multiple images asynchronous.
-- param urls must be a table.
function send_photos_from_url(receiver, urls)
  local cb_extra = {
    receiver = receiver,
    urls = urls,
    remove_path = nil
  }
  send_photos_from_url_callback(cb_extra)
end

-- Use send_photos_from_url.
-- This function might be difficult to understand.
function send_photos_from_url_callback(cb_extra, success, result)
  -- cb_extra is a table containing receiver, urls and remove_path
  local receiver = cb_extra.receiver
  local urls = cb_extra.urls
  local remove_path = cb_extra.remove_path

  -- The previously image to remove
  if remove_path ~= nil then
    os.remove(remove_path)
    print("Deleted: "..remove_path)
  end

  -- Nil or empty, exit case (no more urls)
  if urls == nil or #urls == 0 then
    return false
  end

  -- Take the head and remove from urls table
  local head = table.remove(urls, 1)

  local file_path = download_to_file(head, false)
  local cb_extra = {
    receiver = receiver,
    urls = urls,
    remove_path = file_path
  }

  -- Send first and postpone the others as callback
  send_photo(receiver, file_path, send_photos_from_url_callback, cb_extra)
end

-- Callback to remove a file
function rmtmp_cb(cb_extra, success, result)
  local file_path = cb_extra.file_path
  local cb_function = cb_extra.cb_function or ok_cb
  local cb_extra = cb_extra.cb_extra

  if file_path ~= nil then
    os.remove(file_path)
    print("Deleted: "..file_path)
  end
  -- Finally call the callback
  cb_function(cb_extra, success, result)
end

-- Send document to user and delete it when finished.
-- cb_function and cb_extra are optionals callback
function _send_document(receiver, file_path, cb_function, cb_extra)
  local cb_extra = {
    file_path = file_path,
    cb_function = cb_function or ok_cb,
    cb_extra = cb_extra or false
  }
  -- Call to remove with optional callback
  send_document(receiver, file_path, rmtmp_cb, cb_extra)
end

-- Download the image and send to receiver, it will be deleted.
-- cb_function and cb_extra are optionals callback
function send_document_from_url(receiver, url, cb_function, cb_extra)
  local file_path = download_to_file(url, false)
  print("File path: "..file_path)
  _send_document(receiver, file_path, cb_function, cb_extra)
end

-- Parameters in ?a=1&b=2 style
function format_http_params(params, is_get)
  local str = ''
  -- If is get add ? to the beginning
  if is_get then str = '?' end
  local first = true -- Frist param
  for k,v in pairs (params) do
    if v then -- nil value
      if first then
        first = false
        str = str..k.. "="..v
      else
        str = str.."&"..k.. "="..v
      end
    end
  end
  return str
end

-- Check if user can use the plugin and warns user
-- Returns true if user was warned and false if not warned (is allowed)
function warns_user_not_allowed(plugin, msg)
  if not user_allowed(plugin, msg) then
    local text = 'This plugin requires privileged user'
    local receiver = get_receiver(msg)
    send_msg(receiver, text, ok_cb, false)
    return true
  else
    return false
  end
end

-- Check if user can use the plugin
function user_allowed(plugin, msg)
  if plugin.privileged and not is_sudo(msg) then
    return false
  end
  return true
end


function send_order_msg(destination, msgs)
   local cb_extra = {
      destination = destination,
      msgs = msgs
   }
   send_order_msg_callback(cb_extra, true)
end

function send_order_msg_callback(cb_extra, success, result)
   local destination = cb_extra.destination
   local msgs = cb_extra.msgs
   local file_path = cb_extra.file_path
   if file_path ~= nil then
      os.remove(file_path)
      print("Deleted: " .. file_path)
   end
   if type(msgs) == 'string' then
      send_large_msg(destination, msgs)
   elseif type(msgs) ~= 'table' then
      return
   end
   if #msgs < 1 then
      return
   end
   local msg = table.remove(msgs, 1)
   local new_cb_extra = {
      destination = destination,
      msgs = msgs
   }
   if type(msg) == 'string' then
      send_msg(destination, msg, send_order_msg_callback, new_cb_extra)
   elseif type(msg) == 'table' then
      local typ = msg[1]
      local nmsg = msg[2]
      new_cb_extra.file_path = nmsg
      if typ == 'document' then
         send_document(destination, nmsg, send_order_msg_callback, new_cb_extra)
      elseif typ == 'image' or typ == 'photo' then
         send_photo(destination, nmsg, send_order_msg_callback, new_cb_extra)
      elseif typ == 'audio' then
         send_audio(destination, nmsg, send_order_msg_callback, new_cb_extra)
      elseif typ == 'video' then
         send_video(destination, nmsg, send_order_msg_callback, new_cb_extra)
      else
         send_file(destination, nmsg, send_order_msg_callback, new_cb_extra)
      end
   end
end

-- Same as send_large_msg_callback but friendly params
function send_large_msg(destination, text)
  local cb_extra = {
    destination = destination,
    text = text
  }
  send_large_msg_callback(cb_extra, true)
end

-- If text is longer than 4096 chars, send multiple msg.
-- https://core.telegram.org/method/messages.sendMessage
function send_large_msg_callback(cb_extra, success, result)
  local text_max = 4096

  local destination = cb_extra.destination
  local text = cb_extra.text
  local text_len = string.len(text)
  local num_msg = math.ceil(text_len / text_max)

  if num_msg <= 1 then
    send_msg(destination, text, ok_cb, false)
  else

    local my_text = string.sub(text, 1, 4096)
    local rest = string.sub(text, 4096, text_len)

    local cb_extra = {
      destination = destination,
      text = rest
    }

    send_msg(destination, my_text, send_large_msg_callback, cb_extra)
  end
end

-- Returns a table with matches or nil
function match_pattern(pattern, text, lower_case)
  if text then
    local matches = {}
    if lower_case then
      matches = { string.match(text:lower(), pattern) }
    else
      matches = { string.match(text, pattern) }
    end
      if next(matches) then
        return matches
      end
  end
  -- nil
end

-- Function to read data from files
function load_from_file(file, default_data)
  local f = io.open(file, "r+")
  -- If file doesn't exists
  if f == nil then
    -- Create a new empty table
    default_data = default_data or {}
    serialize_to_file(default_data, file)
    print ('Created file', file)
  else
    print ('Data loaded from file', file)
    f:close() 
  end
  return loadfile (file)()
end

-- See http://stackoverflow.com/a/14899740
function unescape_html(str)
  local map = { 
    ["lt"]  = "<", 
    ["gt"]  = ">",
    ["amp"] = "&",
    ["quot"] = '"',
    ["apos"] = "'" 
  }
  new = string.gsub(str, '(&(#?x?)([%d%a]+);)', function(orig, n, s)
    var = map[s] or n == "#" and string.char(s)
    var = var or n == "#x" and string.char(tonumber(s,16))
    var = var or orig
    return var
  end)
  return new
end



--Check if this chat is realm or not
function is_realm(msg)
  local var = false
  local realms = 'realms'
  local data = load_data(_config.moderation.data)
  local chat = msg.to.id
  if data[tostring(realms)] then
    if data[tostring(realms)][tostring(msg.to.id)] then
       var = true
       end
       return var
  end
end
--Check if this chat is a group or not
function is_group(msg)
  local var = false
  local groups = 'groups'
  local data = load_data(_config.moderation.data)
  local chat = msg.to.id
  if data[tostring(groups)] then
    if data[tostring(groups)][tostring(msg.to.id)] then
       var = true
       end
       return var
  end
end


function savelog(group, logtxt)

local text = (os.date("[ %c ]=>  "..logtxt.."\n \n"))
local file = io.open("./groups/logs/"..group.."log.txt", "a")

file:write(text)

file:close()

end

function user_print_name(user)
   if user.print_name then
      return user.print_name
   end
   local text = ''
   if user.first_name then
      text = user.last_name..' '
   end
   if user.lastname then
      text = text..user.last_name
   end
   return text
end

--Check if user is the owner of that group or not
function is_owner(msg)
  local var = false
  local data = load_data(_config.moderation.data)
  local user = msg.from.id
  
  if data[tostring(msg.to.id)] then
    if data[tostring(msg.to.id)]['set_owner'] then
      if data[tostring(msg.to.id)]['set_owner'] == tostring(user) then
        var = true
      end
    end
  end

  if data['admins'] then
    if data['admins'][tostring(user)] then
      var = true
    end
  end
  for v,user in pairs(_config.sudo_users) do
    if user == msg.from.id then
        var = true
    end
  end
  return var
end

function is_owner2(user_id, group_id)
  local var = false
  local data = load_data(_config.moderation.data)

  if data[tostring(group_id)] then
    if data[tostring(group_id)]['set_owner'] then
      if data[tostring(group_id)]['set_owner'] == tostring(user_id) then
        var = true
      end
    end
  end
  
  if data['admins'] then
    if data['admins'][tostring(user_id)] then
      var = true
    end
  end
  for v,user in pairs(_config.sudo_users) do
    if user == user_id then
        var = true
    end
  end
  return var
end

--Check if user is admin or not
function is_admin(msg)
  local var = false
  local data = load_data(_config.moderation.data)
  local user = msg.from.id
  local admins = 'admins'
  if data[tostring(admins)] then
    if data[tostring(admins)][tostring(user)] then
      var = true
    end
  end
  for v,user in pairs(_config.sudo_users) do
    if user == msg.from.id then
        var = true
    end
  end
  return var
end

function is_admin2(user_id)
  local var = false
  local data = load_data(_config.moderation.data)
  local user = user_id
  local admins = 'admins'
  if data[tostring(admins)] then
    if data[tostring(admins)][tostring(user)] then
      var = true
    end
  end
  for v,user in pairs(_config.sudo_users) do
    if user == user_id then
        var = true
    end
  end
  return var
end



--Check if user is the mod of that group or not
function is_momod(msg)
  local var = false
  local data = load_data(_config.moderation.data)
  local user = msg.from.id
  if data[tostring(msg.to.id)] then
    if data[tostring(msg.to.id)]['moderators'] then
      if data[tostring(msg.to.id)]['moderators'][tostring(user)] then
        var = true
      end
    end
  end

  if data[tostring(msg.to.id)] then
    if data[tostring(msg.to.id)]['set_owner'] then
      if data[tostring(msg.to.id)]['set_owner'] == tostring(user) then
        var = true
      end
    end
  end

  if data['admins'] then
    if data['admins'][tostring(user)] then
      var = true
    end
  end
  for v,user in pairs(_config.sudo_users) do
    if user == msg.from.id then
        var = true
    end
  end
  return var
end

function is_momod2(user_id, group_id)
  local var = false
  local data = load_data(_config.moderation.data)
  local usert = user_id
  if data[tostring(group_id)] then
    if data[tostring(group_id)]['moderators'] then
      if data[tostring(group_id)]['moderators'][tostring(usert)] then
        var = true
      end
    end
  end

  if data[tostring(group_id)] then
    if data[tostring(group_id)]['set_owner'] then
      if data[tostring(group_id)]['set_owner'] == tostring(user_id) then
        var = true
      end
    end
  end
  
  if data['admins'] then
    if data['admins'][tostring(user_id)] then
      var = true
    end
  end
  for v,user in pairs(_config.sudo_users) do
    if user == usert then
        var = true
    end
  end
  return var
end

-- Returns the name of the sender
function kick_user(user_id, chat_id) 
  if tonumber(user_id) == tonumber(our_id) then -- Ignore bot
    return
  end
  if is_owner2(user_id, chat_id) then -- Ignore admins
    return
  end
  local chat = 'chat#id'..chat_id
  local user = 'user#id'..user_id
  chat_del_user(chat, user, ok_cb, true)
end

-- Ban
function ban_user(user_id, chat_id)
  if tonumber(user_id) == tonumber(our_id) then -- Ignore bot
    return
  end
  if is_admin2(user_id) then -- Ignore admins
    return
  end
  -- Save to redis
  local hash =  'banned:'..chat_id
  redis:sadd(hash, user_id)
  -- Kick from chat
  kick_user(user_id, chat_id)
end
-- Global ban
function banall_user(user_id)  
  if tonumber(user_id) == tonumber(our_id) then -- Ignore bot
    return
  end
  if is_admin2(user_id) then -- Ignore admins
    return
  end
  -- Save to redis
  local hash =  'gbanned'
  redis:sadd(hash, user_id)
end
-- Global unban
function unbanall_user(user_id)
  --Save on redis  
  local hash =  'gbanned'
  redis:srem(hash, user_id)
end

-- Check if user_id is banned in chat_id or not
function is_banned(user_id, chat_id)
  --Save on redis  
  local hash =  'banned:'..chat_id
  local banned = redis:sismember(hash, user_id)
  return banned or false
end

-- Check if user_id is globally banned or not
function is_gbanned(user_id)
  --Save on redis
  local hash =  'gbanned'
  local banned = redis:sismember(hash, user_id)
  return banned or false
end

-- Returns chat_id ban list
function ban_list(chat_id)
  local hash =  'banned:'..chat_id
  local list = redis:smembers(hash)
  local text = "Ban list !\n\n"
  for k,v in pairs(list) do
 		local user_info = redis:hgetall('user:'..v)
		if user_info and user_info.print_name then
   	text = text..k.." - "..string.gsub(user_info.print_name, "_", " ").." ["..v.."]\n"
  	else 
    text = text..k.." - "..v.."\n"
		end
	end
 return text
end

-- Returns globally ban list
function banall_list() 
  local hash =  'gbanned'
  local list = redis:smembers(hash)
  local text = "global bans !\n\n"
  for k,v in pairs(list) do
		 		local user_info = redis:hgetall('user:'..v)
		if user_info and user_info.print_name then
   	text = text..k.." - "..string.gsub(user_info.print_name, "_", " ").." ["..v.."]\n"
  	else 
    text = text..k.." - "..v.."\n"
		end
	end
 return text
end

-- /id by reply
function get_message_callback_id(extra, success, result)
    if result.to.type == 'chat' then
        local chat = 'chat#id'..result.to.id
        send_large_msg(chat, result.from.id)
    else
        return 'Use This in Your Groups'
    end
end

-- kick by reply for mods and owner
function Kick_by_reply(extra, success, result)
  if result.to.type == 'chat' then
    local chat = 'chat#id'..result.to.id
    if tonumber(result.from.id) == tonumber(our_id) then -- Ignore bot
      return "I won't kick myself"
    end
    if is_momod2(result.from.id, result.to.id) then -- Ignore mods,owner,admin
      return "you can't kick mods,owner and admins"
    end
    chat_del_user(chat, 'user#id'..result.from.id, ok_cb, false)
  else
    return 'Use This in Your Groups'
  end
end

-- Kick by reply for admins
function Kick_by_reply_admins(extra, success, result)
  if result.to.type == 'chat' then
    local chat = 'chat#id'..result.to.id
    if tonumber(result.from.id) == tonumber(our_id) then -- Ignore bot
      return "I won't kick myself"
    end
    if is_admin2(result.from.id) then -- Ignore admins
      return
    end
    chat_del_user(chat, 'user#id'..result.from.id, ok_cb, false)
  else
    return 'Use This in Your Groups'
  end
end

--Ban by reply for admins
function ban_by_reply(extra, success, result)
  if result.to.type == 'chat' then
  local chat = 'chat#id'..result.to.id
  if tonumber(result.from.id) == tonumber(our_id) then -- Ignore bot
      return "I won't ban myself"
  end
  if is_momod2(result.from.id, result.to.id) then -- Ignore mods,owner,admin
    return "you can't kick mods,owner and admins"
  end
  ban_user(result.from.id, result.to.id)
  send_large_msg(chat, "User "..result.from.id.." Banned")
  else
    return 'Use This in Your Groups'
  end
end

-- Ban by reply for admins
function ban_by_reply_admins(extra, success, result)
  if result.to.type == 'chat' then
    local chat = 'chat#id'..result.to.id
    if tonumber(result.from.id) == tonumber(our_id) then -- Ignore bot
      return "I won't ban myself"
    end
    if is_admin2(result.from.id) then -- Ignore admins
      return
    end
    ban_user(result.from.id, result.to.id)
    send_large_msg(chat, "User "..result.from.id.." Banned")
  else
    return 'Use This in Your Groups'
  end
end

-- Unban by reply
function unban_by_reply(extra, success, result) 
  if result.to.type == 'chat' then
    local chat = 'chat#id'..result.to.id
    if tonumber(result.from.id) == tonumber(our_id) then -- Ignore bot
      return "I won't unban myself"
    end
    send_large_msg(chat, "User "..result.from.id.." Unbanned")
    -- Save on redis
    local hash =  'banned:'..result.to.id
    redis:srem(hash, result.from.id)
  else
    return 'Use This in Your Groups'
  end
end
function banall_by_reply(extra, success, result)
  if result.to.type == 'chat' then
    local chat = 'chat#id'..result.to.id
    if tonumber(result.from.id) == tonumber(our_id) then -- Ignore bot
      return "I won't banall myself"
    end
    if is_admin2(result.from.id) then -- Ignore admins
      return 
    end
    local name = user_print_name(result.from)
    banall_user(result.from.id)
    chat_del_user(chat, 'user#id'..result.from.id, ok_cb, false)
    send_large_msg(chat, "User "..name.."["..result.from.id.."] hammered")
  else
    return 'Use This in Your Groups'
  end
end

