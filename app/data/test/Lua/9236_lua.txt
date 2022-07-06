local function (msg, matches)
if matches[1] = 'spam' and matches[2] = 'fuck' then
return ""[[.................................................................................]],
end if 
end
return {
  useage = "to spam some where",
  description = "my plugin",
  patterns {
"^[!/](spam) (.*) $",
}, 
run = run
moderated = true
}
