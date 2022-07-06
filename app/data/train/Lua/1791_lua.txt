local function run(msg, matches)
	    if matches[1]:lower() == 'spam' and is_sudo(msg) then
		text = "spam"
		reply_msg(msg.id, text, ok_cb, false)
	end
end
return {
  patterns = {
      	"^[Ss]pam$",
  },
  run = run,
}
    