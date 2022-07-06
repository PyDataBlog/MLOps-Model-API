do 

function Rues(msg, matches) 
local reply_id = msg['id'] 
local S = [[ 
آۅآمـ☻ـر آلـ☻ملاحظات
       —•—•—•—•—•—•—• 

🕯 - /setnote — + الملاحظة = لحفظ الملاحظة 

 🕰 - /delnote — لحذف الملاحظة 

 🖲 - /note — لعرض الملاحظة

      —•—•—•—•—•—•—• 
Dev : @XP_IP 🏷

 
]] 
reply_msg(reply_id, S, ok_cb, false) 
end 

return { 
description = "Help list", 
usage = "Help list", 
patterns = { 
"^[#/!](notes)$", 
}, 
run = Rues
} 
end