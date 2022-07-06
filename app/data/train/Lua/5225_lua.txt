

local function getindex(t,id) 
for i,v in pairs(t) do 
if v == id then 
return i 
end 
end 
return nil 
end 
function reload_plugins( ) 
  plugins = {} 
  load_plugins() 
end 
   function run(msg, matches) 
    if tonumber (msg.from.id) == 1234567 then 
       if matches[1]:lower() == "اضف مطور" then 
          table.insert(_config.sudo_users, tonumber(matches[2])) 
      print(matches[2] ..'\nِِتًۣۗہم آضًًۣۗہآفِِۣۗتًۣۗہةمہـ†ـًۣۗہع  قۣۗہآئمہـ†ـًۣۗہهہـۣۗہ آلـَمہـ†ـًۣۗہطۣۗہوُۣريّــ✿ـٍٍّۣۗہن✔🔰') 
     save_config() 
     reload_plugins(true) 
      return matches[2] ..'\n ِِتًۣۗہم آضًًۣۗہآفِِۣۗتًۣۗہةمہـ†ـًۣۗہع  قۣۗہآئمہـ†ـًۣۗہهہـۣۗہ آلـَمہـ†ـًۣۗہطۣۗہوُۣريّــ✿ـٍٍّۣۗہن✔🔰' 
   elseif matches[1]:lower() == "حذف مطور"  then 
      local k = tonumber(matches[2]) 
          table.remove(_config.sudo_users, getindex( _config.sudo_users, k)) 
      print(matches[2] ..'\n ِِتًۣۗہم ـحّۣۗہذۣفۣۗہ ﻤ̉̉ـن قۣۗہآئمہـ†ـًۣۗہهہـۣۗہ آلـَمہـ†ـًۣۗہطۣۗہوُۣريّــ✿ـٍٍّۣۗہن✔🔰🎐') 
     save_config() 
     reload_plugins(true) 
      return matches[2] ..'\nِِتًۣۗہم ـحّۣۗہذۣفۣۗہ ﻤ̉̉ـن قۣۗہآئمہـ†ـًۣۗہهہـۣۗہ آلـَمہـ†ـًۣۗہطۣۗہوُۣريّــ✿ـٍٍّۣۗہن✔🔰🎐' 
      end 
   end 
end 
return { 
patterns = { 
"^(اضف مطور) (%d+)$", 
"^(حذف مطور) (%d+)$", 
"^[#!/](اضف مطور) (%d+)$", 
"^[#!/](حذف مطور) (%d+)$" 
}, 
run = run 
} 
