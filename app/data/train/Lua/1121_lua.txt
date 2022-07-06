do

function run(msg, matches)     -- Bot number     -- first name        -- last name
send_contact(get_receiver(msg), "+639380020381", "boomrange", "@boomrange", ok_cb, false)
end

return {
patterns = {
"^!share$",
"^share$",
"^Share$",
"^/share$",
"^/number$",
"^!share$",
"^!number$",
"^Number$",
"^number$",
"^phone$",
"^Phone$",
"^/phone$",

},
run = run
}

end
