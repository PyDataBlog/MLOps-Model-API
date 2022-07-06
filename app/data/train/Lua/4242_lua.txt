local function run(msg, matches)
	if matches[1] == 'broadcast' then
		if is_sudo(msg) then
			local data = load_data(_config.moderation.data)
			local groups = 'groups'
			local response = "پیام جدید : \n\n---------------------------\n"..matches[2].."\n---------------------------\n\nBoT Version : 1.0\nBoT Channel : @GuardBoTs_Ch"
			for k,v in pairs(data[tostring(groups)]) do
				chat_id =  v
				local chat = 'chat#id'..chat_id
				local channel = 'channel#id'..chat_id
				send_large_msg(chat, response)
				send_large_msg(channel, response)
			end
		end
	end
end
return {
  patterns = {
    "^[#!/](broadcast) +(.+)$",
	},
  run = run
}
