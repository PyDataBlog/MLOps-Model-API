do

function run(msg, matches)

local mods = [[ 
راهنما فارسی مدیران :
@xXDangerXx

!kick [Username | ID | Reply]
اخراج فرد
!ban [username | ID | Reply]
بن کردن فرد
!unban [Username | ID | Reply]
آن بن کردن فرد
!banlist
لیست بن شده ها
!id [Username | Id | Reply]
ایدی
!res @username
اطلاعات یوزرنیم
!res [Reply]
اطلاعات با ریپلی
!lock [ads | name | tag | leave | badw | member | chat | farsi | bots]
قفل (تبلیغ|تگ|خرئج|فحش|اعضا|چت|فارسی|ربات ها(
!unlock [ads | name | tag | leave | badw | member | chat | farsi | bots]
بازکردن قفل 
!all
تمامی اطلاعات گروه
!addsudo
اضافه کردن ادمین اصلی به ربات
!set[photo | name]
تنظیم عکس|نام
!set [rules | about]
تنظیم قوانین|درباره
!tagall [MSG]
تگ کردن همه
!who
کیست؟
!filter [+-?] [word]
فیلتر کردن کلمه یا حذف فیلتر
!set [value] [MSG]
تنظیم پیام ارزش
!info [Username | ID | Reply]
اطلاعات فرد
!plugins [-+] (Plug Name) chat
اضافه و یا حذف پلاگین
==========================
به علاوه تمامی دستورات رنک :
member
==========================
 جهت دریافت راهنمای رنک های دیگر میتوانید از دستورات زیر استفاده کنید :
 
 !help owner 
 راهنمای صاحبان
 
 !help mod
راهنمای مدیران

!help member
راهنمای اعضای معمولی

 ]]

 
 local admin = [[ 
 
 راهنمای فارسی ادمین ها :
 @boomrange @xXDangerXx
 
 !banall [Reply | ID | Username]
 بن کردن از تمامی گروه ها
 !unbanall [ID]
 آنبن کردن از تمامی گروه ها
 !add 
 اضافه کردن گروه
 !rem
 حذف گروه
 !leave
 خروج ربات
 !setowner [ID | Reply]
 تنظیم صاحب
 !kill chat [ID]
 پاک کردن چت
 !bc [GP-ID] [MSG]
 سند تو آل
 !all [GP-ID]
 تمامی اطلاعات گروه
 ==========================
 به علاوه تمامی دستورات رنک های :
 owner
 و
 mod
 و
 member
========================== 
 جهت دریافت راهنمای رنک های دیگر میتوانید از دستورات زیر استفاده کنید :
 
 !help owner 
 راهنمای صاحبان
 
 !help mod
راهنمای مدیران

!help member
راهنمای اعضای معمولی
 
 ]]
 
 local owner = [[
 
 راهنمای فارسی صاحبان گروه :
 @xXDangerXx
 
 !promote [Reply | ID | Username]
 اضافه کردن مدیر
 !demote [Reply | ID | Username]
 حذف مدیر
 !setowner [ID | Reply]
 تغیر صاحب گروه
 !addsudo
 اضافه کردن ادمین
 !setrank [Reply | ID | Username]
 تنظیم مقام
 ==========================
 به علاوه تمامی دستورات رنک های :
 mod
 و
 member
 ==========================
 جهت دریافت راهنمای رنک های دیگر میتوانید از دستورات زیر استفاده کنید :
 
 !help owner 
 راهنمای صاحبان
 
 !help mod
راهنمای مدیران

!help member
راهنمای اعضای معمولی
 
 ]]
 
 local member = [[
 
 راهنمای فارسی عضو ها :
 @xXDangerXx
 
 !calc [formula]
 ماشین حساب
 !get [Value]
 دریافت اطلاعات
 !filterlist
 لیست فیلتر شده ها
 !info
 اطلاعات
 !id
 دریافت ایدی
 !me
 مقام شما
 !voice [MSG]
 تبدیل متن به صدا
 !time
 زمان
 !time [Area]
 زمان در منطقه جغرافیایی مشخص
 !tex [Msg]
 تبدیل متن به عکس
 !feedback [msg]
 فرستادن نظرات خود
 !share
 شیر کردن شماره ربات
 !boomrange
 اطلاعات سازنده
 !sticker [TXT]
 تبدیل متن به استیکر 
 ==========================
 جهت دریافت راهنمای رنک های دیگر میتوانید از دستورات زیر استفاده کنید :
 
 !help owner 
 راهنمای صاحبان
 
 !help mod
راهنمای مدیران

!help member
راهنمای اعضای معمولی
 
 ]]
 
 local sudo = [[
 
 راهنما فارسی سودو ها :
 @xXDangerXx
 
 !addadmin [ID | Username]
 اضافه کردن ادمین
 !removeadmin [ID | Username]
 حذف ادمین
 !creategroup [Name]
 ساخت گروه
 !up [Name.format] [Text]
 در حال کار
 !dl [name.format]
 دانلود
 !echo> [name.format] [Text]
 تکرار متن
 !invite [Username | ID | Reply]
 دعوت
 !stats bot
 آمار ربات
 !plugins 
 پلاگین ها
 !plugins [+-] [Plugname]
 اضافه و یا حذف پلاگین
 !reload
 بازنگری پلاگین ها
 ==========================
 به علاوه دسترسی به تمامی دستورات رنک های دیگر
 ==========================
 جهت دریافت راهنمای رنک های دیگر میتوانید از دستورات زیر استفاده کنید :
 
 !help owner 
 راهنمای صاحبان
 
 !help mod
راهنمای مدیران

!help member
راهنمای اعضای معمولی
 ]]
 
 if msg.to.type == 'chat' and matches[1]:lower() == 'help' and matches[2] == nil then 
     
    if is_sudo(msg) then 
        return sudo
    elseif is_admin(msg) then 
        return admin 
    elseif is_owner(msg) then 
        return owner
    elseif is_momod(msg) then 
         return mods
    else 
        return member
        end
end
 if msg.to.type == 'chat' and matches[1]:lower() == 'help' and matches[2]:lower() == 'owner' then 
     if is_owner(msg) then 
         return owner
    else return 'Only Admins or higher can See this !'
    end 
end

 if msg.to.type == 'chat' and matches[1]:lower() == 'help' and matches[2]:lower() == 'mod' then
     if is_momod(msg) then 
         return mods
    else return 'only Owners Or higher Can See this !'
    end 
end 

 if msg.to.type == 'chat' and matches[1]:lower() == 'help' and matches[2]:lower() == 'member' then
         return member
    end 
end

    
return {
 
  patterns = {
    "^!(help)$",
	"^([Hh]elp)$",
	"^/(help)$",
	
	"^!(help) (mod)$",
	"^!(help) (owner)$",
	"^!(help) (member)$",
  }, 
  run = run 
}

end
