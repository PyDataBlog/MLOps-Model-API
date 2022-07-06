do
function run(msg, matches)
  return "📶↘️ \n +"..(msg.from.phone or '')
end
return { 
  description = "number", 
  usage = "number ",
  patterns = {
    "^[#/](Mn)$",
  },
  run = run
}
end