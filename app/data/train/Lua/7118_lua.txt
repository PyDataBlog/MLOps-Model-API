do

function run(msg, matches)
  return 'Bot '.. VERSION .. [[ 
ورژن ربات 4
ایدی سودو
@ashi_admin1_kaboos
]]
end

return {
  description = "Shows bot version", 
  usage = "ورژن: Shows bot version",
  patterns = {
    "^ورژن$",
    "^[Vv]ersion$"
  }, 
  run = run 
}

end
