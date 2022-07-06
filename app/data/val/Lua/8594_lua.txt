-- data saved to moderation.json
-- check moderation plugin
do

local function create_group(msg)
    -- superuser and admins only (because sudo are always has privilege)
    if not is_admin(msg) then
        return " بـرایـ ساختـ گـروهـ بـایـد پـول بپـردازیـد به ادمین مراجعهـ کنـیـد : @ThisIsArman یـا درگـروه زیـر درخواست کنیـدhttps://telegram.me/joinchat/B-C-GQIfRi4PCmCNXEAezA"     
    end
    local group_creator = msg.from.print_name
    create_group_chat (group_creator, group_name, ok_cb, false)
	return ' گروه '..string.gsub(group_name, '_', ' ')..' با موفقیت ساختهــ شد ' 
end

local function set_description(msg, data)
    if not is_momod(msg) then
        return " تنها مدیر های گروه مجاز هستند⛔️ "
    end
    local data_cat = 'description'
	data[tostring(msg.to.id)][data_cat] = deskripsi
	save_data(_config.moderation.data, data)

	return 'Set group description to:\n'..deskripsi
end

local function get_description(msg, data)
    local data_cat = 'description'
    if not data[tostring(msg.to.id)][data_cat] then
		return ' هیچ توضیحی وجود ندارد❌ '
	end
    local about = data[tostring(msg.to.id)][data_cat]
    local about = string.gsub(msg.to.print_name, "_", " ")..':\n\n'..about
    return 'About '..about
end

local function set_rules(msg, data)
    if not is_momod(msg) then
        return " تنها مدیر های گروه مجاز هستند⛔️ "
    end
    local data_cat = 'rules'
	data[tostring(msg.to.id)][data_cat] = rules
	save_data(_config.moderation.data, data)

	return 'Set group rules to:\n'..rules
end

local function get_rules(msg, data)
    local data_cat = 'rules'
    if not data[tostring(msg.to.id)][data_cat] then
		return ' هیچ قانونی وجود ندارد❗️ '
	end
    local rules = data[tostring(msg.to.id)][data_cat]
    local rules = string.gsub(msg.to.print_name, '_', ' ')..' rules:\n\n'..rules
    return rules
end

-- lock/unlock group name. bot automatically change group name when locked
local function lock_group_name(msg, data)
    if not is_momod(msg) then
        return " تنها مدیر های گروه مجاز هستند⛔️ "
    end
    local group_name_set = data[tostring(msg.to.id)]['settings']['set_name']
    local group_name_lock = data[tostring(msg.to.id)]['settings']['lock_name']
	if group_name_lock == 'yes' then
	    return ' نام گروه فقل شد🔒 '
	else
	    data[tostring(msg.to.id)]['settings']['lock_name'] = 'yes'
	    save_data(_config.moderation.data, data)
	    data[tostring(msg.to.id)]['settings']['set_name'] = string.gsub(msg.to.print_name, '_', ' ')
	    save_data(_config.moderation.data, data)
	return ' نام گروه فقل شد🔒 '
	end
end

local function unlock_group_name(msg, data)
    if not is_momod(msg) then
        return " تنها مدیر های گروه مجاز هستند⛔️ "
    end
    local group_name_set = data[tostring(msg.to.id)]['settings']['set_name']
    local group_name_lock = data[tostring(msg.to.id)]['settings']['lock_name']
	if group_name_lock == 'no' then
	    return ' نام قفل نیست🔓 '
	else
	    data[tostring(msg.to.id)]['settings']['lock_name'] = 'no'
	    save_data(_config.moderation.data, data)
	return ' قفل نام گروه برداشته شد🔓 '
	end
end

--lock/unlock group member. bot automatically kick new added user when locked
local function lock_group_member(msg, data)
    if not is_momod(msg) then
        return " تنها مدیر های گروه مجاز هستند⛔️ "
    end
    local group_member_lock = data[tostring(msg.to.id)]['settings']['lock_member']
	if group_member_lock == 'yes' then
	    return ' اعضا گروه از قبل قفل است🔒 '
	else
	    data[tostring(msg.to.id)]['settings']['lock_member'] = 'yes'
	    save_data(_config.moderation.data, data)
	end
	return ' اعضا گروه فقل شد🔒 '
end

local function unlock_group_member(msg, data)
    if not is_momod(msg) then
        return " تنها مدیر های گروه مجاز هستند⛔️ "
    end
    local group_member_lock = data[tostring(msg.to.id)]['settings']['lock_member']
	if group_member_lock == 'no' then
	    return ' اعضا قفل نیست🔓 '
	else
	    data[tostring(msg.to.id)]['settings']['lock_member'] = 'no'
	    save_data(_config.moderation.data, data)
	return ' قفل اعضا گروه برداشته شد🔓 '
	end
end

--lock/unlock group photo. bot automatically keep group photo when locked
local function lock_group_photo(msg, data)
    if not is_momod(msg) then
        return " تنها مدیر های گروه مجاز هستند⛔️ "
    end
    local group_photo_lock = data[tostring(msg.to.id)]['settings']['lock_photo']
	if group_photo_lock == 'yes' then
	    return ' عکس گروه از قبل قفل بوده است🔒 '
	else
	    data[tostring(msg.to.id)]['settings']['set_photo'] = 'waiting'
	    save_data(_config.moderation.data, data)
	end
	return ' عکســ جدیـیـد را بـفرسـتــید📷  '
end

local function unlock_group_photo(msg, data)
    if not is_momod(msg) then
        return " تنها مدیر های گروه مجاز هستند⛔️ "
    end
    local group_photo_lock = data[tostring(msg.to.id)]['settings']['lock_photo']
	if group_photo_lock == 'no' then
	    return ' عکس گروه قفل نیست🔓 '
	else
	    data[tostring(msg.to.id)]['settings']['lock_photo'] = 'no'
	    save_data(_config.moderation.data, data)
	return ' قفل عکس گروه برداشته شد🔓 '
	end
end

local function set_group_photo(msg, success, result)
  local data = load_data(_config.moderation.data)
  local receiver = get_receiver(msg)
  if success then
    local file = 'data/photos/chat_photo_'..msg.to.id..'.jpg'
    print('File downloaded to:', result)
    os.rename(result, file)
    print('File moved to:', file)
    chat_set_photo (receiver, file, ok_cb, false)
    data[tostring(msg.to.id)]['settings']['set_photo'] = file
    save_data(_config.moderation.data, data)
    data[tostring(msg.to.id)]['settings']['lock_photo'] = 'yes'
    save_data(_config.moderation.data, data)
    send_large_msg(receiver, 'Photo saved!', ok_cb, false)
  else
    print('Error downloading: '..msg.id)
    send_large_msg(receiver, 'Failed, please try again!', ok_cb, false)
  end
end
-- show group settings
local function show_group_settings(msg, data)
    if not is_momod(msg) then
        return " تنها مدیر های گروه مجاز هستند⛔️ "
    end
    local settings = data[tostring(msg.to.id)]['settings']
    local text = "Group settings:\nLock group name : "..settings.lock_name.."\nLock group photo : "..settings.lock_photo.."\nLock group member : "..settings.lock_member
    return text
end

function run(msg, matches)
    --vardump(msg)
    if matches[1] == 'creategroup' and matches[2] then
        group_name = matches[2]
        return create_group(msg)
    end
    if not is_chat_msg(msg) then
	    return "تنها در گروه کار میکند❗️"
	end
    local data = load_data(_config.moderation.data)
    local receiver = get_receiver(msg)
    if msg.media then
    	if msg.media.type == 'photo' and data[tostring(msg.to.id)]['settings']['set_photo'] == 'waiting' and is_chat_msg(msg) and is_momod(msg) then
    		load_photo(msg.id, set_group_photo, msg)
    	end
    end
    if data[tostring(msg.to.id)] then
		local settings = data[tostring(msg.to.id)]['settings']
		if matches[1] == 'setabout' and matches[2] then
		    deskripsi = matches[2]
		    return set_description(msg, data)
		end
		if matches[1] == 'about' then
		    return get_description(msg, data)
		end
		if matches[1] == 'setrules' then
		    rules = matches[2]
		    return set_rules(msg, data)
		end
		if matches[1] == 'rules' then
		    return get_rules(msg, data)
		end
		if matches[1] == 'group' and matches[2] == 'lock' then --group lock *
		    if matches[3] == 'name' then
		        return lock_group_name(msg, data)
		    end
		    if matches[3] == 'member' then
		        return lock_group_member(msg, data)
		    end
		    if matches[3] == 'photo' then
		        return lock_group_photo(msg, data)
		    end
		end
		if matches[1] == 'group' and matches[2] == 'unlock' then --group unlock *
		    if matches[3] == 'name' then
		        return unlock_group_name(msg, data)
		    end
		    if matches[3] == 'member' then
		        return unlock_group_member(msg, data)
		    end
		    if matches[3] == 'photo' then
		    	return unlock_group_photo(msg, data)
		    end
		end
		if matches[1] == 'group' and matches[2] == 'settings' then
		    return show_group_settings(msg, data)
		end
		if matches[1] == 'chat_rename' then
		    if not msg.service then
		        return " داری سعی میکنی منو مسخره کنی؟ "
		    end
		    local group_name_set = settings.set_name
		    local group_name_lock = settings.lock_name
		    local to_rename = 'chat#id'..msg.to.id
		    if group_name_lock == 'yes' then
		        if group_name_set ~= tostring(msg.to.print_name) then
		            rename_chat(to_rename, group_name_set, ok_cb, false)
		        end
		    elseif group_name_lock == 'no' then
                return nil
            end
		end
		if matches[1] == 'setname' and is_momod(msg) then
		    local new_name = string.gsub(matches[2], '_', ' ')
		    data[tostring(msg.to.id)]['settings']['set_name'] = new_name
		    save_data(_config.moderation.data, data) 
		    local group_name_set = data[tostring(msg.to.id)]['settings']['set_name']
		    local to_rename = 'chat#id'..msg.to.id
		    rename_chat(to_rename, group_name_set, ok_cb, false)
		end
		if matches[1] == 'setphoto' and is_momod(msg) then
		    data[tostring(msg.to.id)]['settings']['set_photo'] = 'waiting'
	        save_data(_config.moderation.data, data)
	        return ' عکســ جدیـیـد را بـفرسـتـیـید📷 '
		end
		if matches[1] == 'chat_add_user' then
		    if not msg.service then
		        return " داری سعی میکنی منو مسخره کنی؟ "
		    end
		    local group_member_lock = settings.lock_member
		    local user = 'user#id'..msg.action.user.id
		    local chat = 'chat#id'..msg.to.id
		    if group_member_lock == 'yes' then
		        chat_del_user(chat, user, ok_cb, true)
		    elseif group_member_lock == 'no' then
                return nil
            end
		end
		if matches[1] == 'chat_delete_photo' then
		    if not msg.service then
		        return " داری سعی میکنی منو مسخره کنی؟ "
		    end
		    local group_photo_lock = settings.lock_photo
		    if group_photo_lock == 'yes' then
		        chat_set_photo (receiver, settings.set_photo, ok_cb, false)
		    elseif group_photo_lock == 'no' then
                return nil
            end
		end
		if matches[1] == 'chat_change_photo' and msg.from.id ~= 0 then
		    if not msg.service then
		        return " داری سعی میکنی منو مسخره کنی؟ "
		    end
		    local group_photo_lock = settings.lock_photo
		    if group_photo_lock == 'yes' then
		        chat_set_photo (receiver, settings.set_photo, ok_cb, false)
		    elseif group_photo_lock == 'no' then
		    	return nil
		    end
		 end
    end
end


return {
  description = "پلاگین مدیریت تنظیمات جانبی💪 ", 
  usage = {
    "!creategroup <group_name> : ســاخــت گــروهــ مـدیــریــت شــده (تنها سازنده اصلی بات) ",     
    "!setabout <description> : گزاشتن توضیحات برای گروه📖 ",
    "!about : توضیحات گروه😉 ",
    "!setrules <rules> : گزاشتن  قوانین گروه📄 ",
    "!rules : نمایش قوانین گروه📊 ",
    "!setname <new_name> : گزاشتن نام برای گروه〰 ",
    "!setphoto : گزاشتن عکس برای گروه📷 ",
    "!group <lock|unlock> name : فقل/بازکردن نام گروه✔️❌ ",
    "!group <lock|unlock> photo : فقل/بازکردن عکس گروه✔️❌ ",
    "!group <lock|unlock> member : فقل/بازکردن اعضا گروه✔️❌ ",		
    "!group settings : نمایش تنظیمات گروه❌✔️ "
    },
  patterns = {
    "^!(creategroup) (.*)$",
    "^/(creategroup) (.*)$",
    "^!(setabout) (.*)$",
    "^/(setabout) (.*)$",
    "^!(about)$",
    "^/(about)$",
    "^!(setrules) (.*)$",
    "^/(setrules) (.*)$",
    "^!(rules)$",
    "^/(rules)$",
    "^!(setname) (.*)$",
    "^/(setname) (.*)$",
    "^!(setphoto)$",
    "^/(setphoto)$",
    "^!(group) (lock) (.*)$",
    "^/(group) (lock) (.*)$",
    "^!(group) (unlock) (.*)$",
    "^/(group) (unlock) (.*)$",
    "^!(group) (settings)$",
    "^/(group) (settings)$",
    "^!!tgservice (.+)$",
    "%[(photo)%]",
  }, 
  run = run,
}

end
