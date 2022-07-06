-- Textclock widget
mytextclock = awful.widget.textclock({ align = "right" }, "%a %d %b, %H:%M:%S", 1)
-- Show calendar
calendar2.addCalendarToWidget(mytextclock)
