local function run(msg, matches)
if matches[1] == "inline" and matches[2] and matches[3] and matches[4] then
  local text = matches[2]
  local link_text = matches[3]
  local link = matches[4]
    local keyboard = {} keyboard.inline_keyboard = {
   {
                {text = link_text, url = link},
      },
    }
 send_api_keyboard(msg, get_receiver_api(msg), text, keyboard)
end
if matches[1] == "inline2" and is_sudo(msg) and matches[2] and matches[3] and matches[4] and matches[5] and matches[6] then
  local text = matches[2]
  local link_text = matches[3]
  local link = matches[4]
  local link_text2 = matches[5]
  local link2 = matches[6]
    local keyboard = {} keyboard.inline_keyboard = {
   {
                {text = link_text, url = link},
                {text = link_text2, url = link},
      },
    }
 send_api_keyboard(msg, get_receiver_api(msg), text, keyboard)
end
if matches[1] == "inline3" and is_sudo(msg) and matches[2] and matches[3] and matches[4] and matches[5] and matches[6] and matches[7] and matches[8] then
  local text = matches[2]
  local link_text = matches[3]
  local link = matches[4]
  local link_text2 = matches[5]
  local link2 = matches[6]
  local link_text3 = matches[7]
  local link3 = matches[8]
    local keyboard = {} keyboard.inline_keyboard = {
   {
                {text = link_text, url = link},
                {text = link_text2, url = link2},
                },
                {
                {text = link_text3, url = link3},
      },
    }
 send_api_keyboard(msg, get_receiver_api(msg), text, keyboard)
end
if matches[1] == "inline4" and is_sudo(msg) and matches[2] and matches[3] and matches[4] and matches[5] and matches[6] and matches[7] and matches[8] and matches[9] and matches[10] then
  local text = matches[2]
  local link_text = matches[3]
  local link = matches[4]
  local link_text2 = matches[5]
  local link2 = matches[6]
  local link_text3 = matches[7]
  local link3 = matches[8]
  local link_text4 = matches[9]
  local link4 = matches[10]
    local keyboard = {} keyboard.inline_keyboard = {
   {
                {text = link_text, url = link},
                {text = link_text2, url = link2},
                },
                {
                {text = link_text3, url = link3},
                {text = link_text4, url = link4},
      },
    }
 send_api_keyboard(msg, get_receiver_api(msg), text, keyboard)
end
if matches[1] == "inline5" and is_sudo(msg) and matches[2] and matches[3] and matches[4] and matches[5] and matches[6] and matches[7] and matches[8] and matches[9] and matches[10] and matches[11] and matches[12] then
  local text = matches[2]
  local link_text = matches[3]
  local link = matches[4]
  local link_text2 = matches[5]
  local link2 = matches[6]
  local link_text3 = matches[7]
  local link3 = matches[8]
  local link_text4 = matches[9]
  local link4 = matches[10]
  local link_text5 = matches[11]
  local link5 = matches[12]
    local keyboard = {} keyboard.inline_keyboard = {
   {
                {text = link_text, url = link},
                {text = link_text2, url = link2},
                },
                {
                {text = link_text3, url = link3},
                {text = link_text4, url = link4},
                },
                {
                {text = link_text5, url = link5},
      },
    }
 send_api_keyboard(msg, get_receiver_api(msg), text, keyboard)
end
if matches[1] == "inline6" and is_sudo(msg) and matches[2] and matches[3] and matches[4] and matches[5] and matches[6] and matches[7] and matches[8] and matches[9] and matches[10] and matches[11] and matches[12] and matches[13] and matches[14] then
  local text = matches[2]
  local link_text = matches[3]
  local link = matches[4]
  local link_text2 = matches[5]
  local link2 = matches[6]
  local link_text3 = matches[7]
  local link3 = matches[8]
  local link_text4 = matches[9]
  local link4 = matches[10]
  local link_text5 = matches[11]
  local link5 = matches[12]
  local link_text6 = matches[13]
  local link6 = matches[14]
    local keyboard = {} keyboard.inline_keyboard = {
   {
                {text = link_text, url = link},
                {text = link_text2, url = link2},
                },
                {
                {text = link_text3, url = link3},
                {text = link_text4, url = link4},
                },
                {
                {text = link_text5, url = link5},
                {text = link_text6, url = link6},
      },
    }
 send_api_keyboard(msg, get_receiver_api(msg), text, keyboard)
end
end
return { 
patterns = {
"^/(inline) (.*)+(.*)+(.*)$",
"^/(inline2) (.*)+(.*)+(.*)+(.*)+(.*)$",
"^/(inline3) (.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)$",
"^/(inline4) (.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)$",
"^/(inline5) (.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)$",
"^/(inline6) (.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)+(.*)$",
}, 
run = run
 }
