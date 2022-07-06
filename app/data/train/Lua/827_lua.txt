local w = display.contentWidth
local h = display.contentHeight

local daysInYear = 365.2425   -- # days based on gregorian cal
local weeksInYear = daysInYear / 7
local daysInMonth = daysInYear / 12   --average # days a month
local weeksInMonth = daysInMonth / 7 -- # of weeks in a month
local secInMin = 60    -- # of seconds in a minute
local secInHour = 60 * secInMin
local secInDay = 24 * secInHour
local secInWeek = 7 * secInDay
local secInMonth = daysInMonth *secInDay  -- ave # of sec/month
local secInYear = daysInYear *secInDay   --# seconds in a year
local todaysDate = os.date("*t")
print (todaysDate.isdst, todaysDate.yday, todaysDate.wday)

-- load background to be used for dismissing keyboard
local background = display.newRect(w/2, h/2, w, h)
background:setFillColor(0, .4, .4)
local submit = display.newRect(w/2, 3*h/4, 300, 100)
local submitText = display.newText("Submit", submit.x, submit.y, nil, 36)
submitText:setFillColor(0)

-- Get the users name and birthdate
local nameInstructions = display.newText("Enter your name", w/2, 50, nil, 24 )
local usersName = native.newTextField(w/2, 100, 350, 50)
usersName.inputType = "default"
local bdayInstructions = display.newText("Enter your birthdate (mm/dd/yyyy)", w/2, 160, nil, 24)
local bday = native.newTextField(w/2, 200, 350, 50)
bday.inputType = "default"

local function keyboardListener (event)
     native.setKeyboardFocus(nil)
end

local function calculateAge()
  -- Get the birth year, month, and day
  local bdayLen = string.len(bday.text)
  local birthYear = string.sub(bday.text, bdayLen-3, bdayLen)
  local birthMonth = string.sub(bday.text, 1,2)
  local birthDay = string.sub(bday.text, 4, 5)
  -- print(birthYear, birthMonth, birthDay)
  bday:removeSelf()
  usersName:removeSelf()
  nameInstructions:removeSelf()
  bdayInstructions:removeSelf()
  submit:removeSelf()
  submitText:removeSelf()
  -- How many seconds from 1/1/1 to today
  local totalSecToday = (todaysDate.year * secInYear) + (todaysDate.month * secInMonth) + (todaysDate.day * secInDay)
  -- How many seconds from year 0 to birthdate
  local totalSecBday = (birthYear * secInYear) + (birthMonth * secInMonth) + (birthDay * secInDay)
  local totalSecAlive = totalSecToday - totalSecBday
  local secAliveText = display.newText(usersName.text..", you have been alive for :", w/2, 100, native.systemFont, 30)
  local secAlive = display.newText(totalSecAlive.." seconds", w/2, 200, native.systemFont, 30)
  local secAliveText2 = display.newText("and counting", w/2, 300, native.systemFont, 30)
end

background:addEventListener("tap", keyboardListener)
submit:addEventListener("tap", calculateAge)