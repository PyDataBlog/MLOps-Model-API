return text
  end
  end
 if matches[1]:lower() == 'info' and not matches[2] then
  local receiver = get_receiver(msg)
  local Reply = msg.reply_id
  if msg.reply_id then
    msgr = get_message(msg.reply_id, action_by_reply, {receiver=receiver, Reply=Reply})
  else
  if msg.from.username then
   Username = '@'..msg.from.username
   else
   Username = 'ندارد'
   end
   local text = 'نام : '..(msg.from.first_name or 'ندارد')..'\n'
   local text = text..'فامیل : '..(msg.from.last_name or 'ندارد')..'\n'  
   local text = text..'یوزر : '..Username..'\n'
   local text = text..'ایدی کاربری : '..msg.from.id..'\n\n'
   local hash = 'rank:'..msg.to.id..':variables'
  if hash then
    local value = redis:hget(hash, msg.from.id)
    if not value then
    if msg.from.id == tonumber(Arian) then
     text = text..'مقام : مدیر کل ربات (Executive Admin) \n\n'
    elseif is_sudo(msg) then
     text = text..'مقام : ادمین ربات (Admin) \n\n'
    elseif is_owner(msg) then
     text = text..'مقام : مدیر کل گروه (Owner) \n\n'
    elseif is_momod(msg) then
     text = text..'مقام : مدیر گروه (Moderator) \n\n'
    else
     text = text..'مقام : کاربر (Member) \n\n'
    end
    else
     text = text..'مقام : '..value..'\n'
    end
  end
    
   local uhash = 'user:'..msg.from.id
    local user = redis:hgetall(uhash)
     local um_hash = 'msgs:'..msg.from.id..':'..msg.to.id
   user_info_msgs = tonumber(redis:get(um_hash) or 0)
   text = text..'تعداد پیام های فرستاده شده : '..user_info_msgs..'\n\n'
   if msg.to.type == 'chat' then
   text = text..'نام گروه : '..msg.to.title..'\n'
     text = text..'ایدی گروه : '..msg.to.id
    end
  text = text..'\n\n https://telegram.me/BlacklifeTGch Team'
    return send_msg(receiver, text, ok_cb, true)
    end
  end
  if matches[1]:lower() == 'info' and matches[2] then
   local user = matches[2]
   local chat2 = msg.to.id
   local receiver = get_receiver(msg)
   if string.match(user, '^%d+$') then
    user_info('user#id'..user, action_by_id, {receiver=receiver, user=user, text=text, chat2=chat2})
    elseif string.match(user, '^@.+$') then
      username = string.gsub(user, '@', '')
      msgr = res_user(username, res_user_callback, {receiver=receiver, user=user, text=text, chat2=chat2})
   end
  end
end

return {
  description = 'Know your information or the info of a chat members.',
  usage = {
  '#info: Return your info and the chat info if you are in one.',
  '(Reply)#info: Return info of replied user if used by reply.',
  '#info <id>: Return the info\'s of the <id>.',
  '#info @<user_name>: Return the member @<user_name> information from the current chat.',
  '#setrank <userid> <rank>: change members rank.',
  '(Reply)!setrank <rank>: change members rank.',
  },
  patterns = {
  "^[#]([Ii][Nn][Ff][Oo])$",
  "^[#]([Ii][Nn][Ff][Oo]) (.*)$",
  "^[#]([Ss][Ee][Tt][Rr][Aa][Nn][Kk]) (%d+) (.*)$",
  "^[#]([Ss][Ee][Tt][Rr][Aa][Nn][Kk]) (.*)$",
  },
  run = run
}

end

— By Arian
