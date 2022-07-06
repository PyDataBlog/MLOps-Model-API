
local function phplua(msg,matches) 
local mahdi = matches[2] 
return mahdi 
end 

  return  { 
    patterns = { 
      "^(كول)(.+)", 
       "^(اكول)(.+)", 
        "^(يكول)(.+)", 
}, 
  run = phplua, 
} 
