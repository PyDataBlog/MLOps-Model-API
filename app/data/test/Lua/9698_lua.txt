--[[ 
$ :) 
-- - ( #MASCO_ماسكو ) - -- 
$ :) 
-- - ( @Ahmedjabbar1) - -- 
$ :) 
--Channel-( @TeamTop )-- 
$ :) 
]]-- 
do 
local function iq_abs(msg,matches) 
local reply_id = msg['id'] 

    if matches[1] == "chat_add_user"  then -- Channel @TeamTop 
      return '🚏 - Welcome to groups'..'\n' 
..'name : '..(msg.from.first_name or ' ')..'\n' 
..'user : @'..msg.from.username..'\n' 
..'ID : '..msg.from.id..'\n' 
..'GB_name : '..msg.to.title..'\n' 
..'GB_ID : '..msg.to.id..'\n' 
..'Channel : @TeamTop'..'\n' 
..'time : '..os.date(' %T', os.time())..'\n' 
..'📆 - : '.. os.date('!%A %B:%d:%Y\n', timestamp)..'\n'

    elseif matches[1] == "chat_add_user_link" then 
      return '🚏 - Welcome to groups'..'\n' 
..'name : '..(msg.from.first_name or ' ')..'\n' 
..'user : @'..msg.from.username..'\n' 
..'ID : '..msg.from.id..'\n' 
..'GB_name : '..msg.to.title..'\n' 
..'GB_ID : '..msg.to.id..'\n' 
..'Channel : @TeamTop'..'\n' 
..'time : '..os.date(' %T', os.time())..'\n' 
..'📆 - : '.. os.date('!%A %B:%d:%Y\n', timestamp)..'\n' 
    end 

    if matches[1] == "chat_del_user"  then 
       local bye_name = msg.action.user.first_name 
       return '🚀🚏 وداعـﮩـاً '..bye_name 
   end 
end 

return { 
    patterns = { 
        "^!!tgservice (chat_add_user)$", 
        "^!!tgservice (chat_add_user_link)$", 
        "^!!tgservice (chat_del_user)$", 
    }, 
 run = iq_abs, 
} 
end 
--
