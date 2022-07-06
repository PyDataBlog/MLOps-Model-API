
local function run(msg, matches)
    if is_momod(msg) then
        return
    end
    local data = load_data(_config.moderation.data)
    if data[tostring(msg.to.id)] then
        if data[tostring(msg.to.id)]['settings'] then
            if data[tostring(msg.to.id)]['settings']['fosh'] then
                lock_fosh = data[tostring(msg.to.id)]['settings']['fosh']
            end
        end
    end
    local chat = get_receiver(msg)
    local user = "user#id"..msg.from.id
    if lock_fosh == "yes" then
       delete_msg(msg.id, ok_cb, true)
    end
end
 
return {
  patterns = {
      
          "کیر",
          "کوس",
	  "کون",
	  "کونی",
	  "جنده",
	  "سکسی",
          "کیری",
	  "قحبه",
	  "کونده",
	  "کوسده",
          "سکس",
	  "پورن",
	  "خایه",
	  "اوبی",
	  "کاندوم",
	  "kir",
	  "kos",
	  "jende",
	  "گاییدم",
	  "گایید",
	},
  run = run
}



