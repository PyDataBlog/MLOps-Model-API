local function run(msg)
if msg.text == "سلام" then
	return "؟سلام عشقم خوبی"
end
if msg.text == "کیر" then
	return "تو کسس ننت"
end
if msg.text == "Hello" then
	return "Hi bb"
end
if msg.text == "hello" then
	return "Hi honey"
end
if msg.text == "کسس ننت" then
	return "کیر آقام دست ننت"
end
if msg.text == "Salam" then
	return "Salam aleykom"
end
if msg.text == "salam" then
	return "va aleykol asalam"
end
if msg.text == "ali" then
	return "با باباییم چیکار داری؟"
end
if msg.text == "ناموسا" then
	return "ننت و آقام چالوسن"
end
if msg.text == "علی" then
	return "علی بیا این پشمک کارت داره!
end
if msg.text == "ZAC" then
	return "Barash bezan sak"
end
if msg.text == "pob" then
	return "Yes?"
end
if msg.text == "خوبی" then
	return "فدات تو خوبی"
end	
if msg.text == "Pob" then
	return "What?"
end
if msg.text == "bot" then
	return "hum?"
end
if msg.text == "Bot" then
	return "Huuuum?"
end
if msg.text == "?" then
	return "Hum??"
end
if msg.text == "Bye" then
	return "Babay"
end
if msg.text == "bye" then
	return "Bye Bye"
end
end

return {
	description = "Chat With Robot Server", 
	usage = "chat with robot",
	patterns = {
		"^[Hh]i$",
		"^[Hh]ello$",
		"^[Zz]ac$",
		"^ZAC$",
		"^[Bb]ot$",
		"^[Uu]mbrella$",
		"^[Bb]ye$",
		"^?$",
		"^سلام$",
		"^خوبی$",
		"^علی$",
		"^کس ننت$",
		"^ناموسا$",
		"^ali$",
		"^Ali$",
		"^pob$",
		"^کیر$",
		"^[Ss]alam$",
		}, 
	run = run,
    --privileged = true,
	pre_process = pre_process
}
