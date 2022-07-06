--[[ -- تم التعديل و التعريب بواسطه @KNSLTHM

--[[ 
                                         Dev @KNSLTHM
                                         Dev @NAHAR2_BOT
         
       
              CH > @NENO_CH
--]]
do

local function pre_process(msg)

    local fwd = 'mate:'..msg.to.id
    if redis:get(fwd) and not is_momod(msg) and msg.fwd_from then
            delete_msg(msg.id, ok_cb, true)
            return "ok"
end
        return msg
    end

  


local function moody(msg, matches)
    chat_id =  msg.to.id
    
    if is_momod(msg) and matches[1] == 'قفل التوجيه'  then
      
            
                    local hash = 'mate:'..msg.to.id
                    redis:set(hash, true)
                    return ""
  elseif is_momod(msg) and matches[1] == 'فتح التوجيه' then
      local hash = 'mate:'..msg.to.id
      redis:del(hash)
      return ""
end

end

return {
    patterns = {
        '^(قفل التوجيه)$', 
        '^(فتح التوجيه)$',
        '^[/!#](قفل التوجيه)$', 
        '^[/!#](فتح التوجيه)$'
    },
run = GENERAL,
pre_process = pre_process 
}
end

--[[ -- تم التعديل و التعريب بواسطه @KNSLTHM

--[[ 
                                         Dev @KNSLTHM
                                         Dev @NAHAR2_BOT
         
       
              CH > @NENO_CH
--]]
