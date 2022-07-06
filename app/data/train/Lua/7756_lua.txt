do

function run(msg, matches)
send_contact(get_receiver(msg), "+989154123827", "Eset", "evilbot", ok_cb, false)
end

return {
patterns = {
"^!share$"

},
run = run
}

end
