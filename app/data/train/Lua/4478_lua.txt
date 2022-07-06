do function run(msg, matches)
  if matches[1]:lower() == 'salam' then
  return "Salam"
  end
  if matches[1]:lower() == 'khobi' then
  return "Mersi to khobi?"
  end
  if matches[1]:lower() == 'سلام' then
  return "سلام"
 end
  if matches[1]:lower() == 'خوبی' then
  return "مرسی تو خوبی؟"
  end
  if matches[1]:lower() == 'خوبم' then
  return "خوشحالم که خوبی"
  end
  if matches[1]:lower() == 'ممنون' then
  return "خواهش میکنم"
  end
  if matches[1]:lower() == 'چه خبر' then
  return "سلامتی"
  end
  if matches[1]:lower() == 'اسمت چیه' then
  return "نمیدونم"
  end
  if matches[1]:lower() == 'گروه' then
  return "هر گروه قیمتش 2 تومنه"
  end
  if matches[1]:lower() == 'خر' then
  return "عمته😁"
end
if matches[1]:lower() == 'هه' then
  return "๏_๏"
  end
if matches[1]:lower() == 'bk' then
  return "B K k b K"
end
if matches[1]:lower() == 'چطورید' then
  return "مرسی تو چطوری؟"
end
if matches[1]:lower() == 'چطوری' then
  return "مرسی تو چطوری؟"
  end
  if matches[1]:lower() == 'چت نکنید' then
  return "چت نکنید دیگه چقد زر میزنید 😡"
 end
 if matches[1]:lower() == 'چت ممنوع' then
  return "آقا چت ممنوعه ساکت باشید!"
 end
 if matches[1]:lower() == 'کس نگو' then
  return " راست میگه دیگه کس نگو 😒"
 end
 if matches[1]:lower() == 'کس نگید' then
  return "کس نگید دیگه 😡"
 end
 if matches[1]:lower() == 'امیر' then
  return "بابامه 💋"
 end
  if matches[1]:lower() == 'ban' then
  return "بری دیگه برنگردی 😂"
 end
 if matches[1]:lower() == 'gb' then
  return "اوه به فاک رفت 🙄"
 end
end
  return {
  patterns = {
    "^([Aa][Mi][Ii][Rr])$",
    "^([Kk][Hh][Oo][Bb][Ii])?$",
    "^(سلام)$",
    "^(شلام)$",
    "^(خوبی)$",
    "^(خوبی؟)$",
    "^(خوبم)$",
    "^(ممنون)$",
    "^(چه خبر)$",
    "^(اسمت چیه)$",
    "^(اسمت چیه؟)$",
    "^(گروه)$",
    "^(خر)$",
    "^(هه)$",
    "^([Bb][Kk])$",
    "^(چطورید؟)$",
    "^(چطورید)$",
    "^([Gg][Pp])$",
    "^(کس نگو)$",
	"^(این کیه پسرم)$",
    "^(کس نگید)$",
    "^(چت نکنید)$",
    "^(چت ممنوع)$",
    "^(امیر)$",
    "^([Bb][Aa][Nn])?$",
    "^([Gg][Bb])?$"
  },
  run = run
}
end
