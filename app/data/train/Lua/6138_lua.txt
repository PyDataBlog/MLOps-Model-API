if Server then
  Console:Subscribe("testfire", function( _c )
      Chat:Send(Player.GetById(0), "<(orange)direct(white)> Test(orange)fire(white)!", Color.White)
      Chat:Broadcast("<(cyan)broadcast(white)> Test(orange)fire(white)!", Color.White)
      Player.GetById(0):SendChatMessage("<(green)instance(white)> Test(orange)fire(white)!", Color.White)
      Network:Broadcast("testfire", {})
  end )
end

if Client then
  Network:Subscribe("testfire", function(_c)
    Chat:Print("<(purple)client(white)> Test(orange)fire(white)!", Color.White)
  end)
end
