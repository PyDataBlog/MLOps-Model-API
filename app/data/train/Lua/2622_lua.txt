concommand.Add( "smite", function( ply, cmd, args )
  local TargetString = table.concat( args ) 
  local TargetPlayer = GetPlayerByName( ply, TargetString )
  if TargetPlayer = nil then
    ply:ChatPrint( "This is not a valid target" )
  else
    -- TODO: smite TargetPlayer
  end
end )

function GetPlayerByName( ply, target )
	for k,v in pairs( player.GetAll() ) do
		if v:Nick() == target then
			return v
		end
	end
	return nil
end
