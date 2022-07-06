
function onUse(cid, item, frompos, item2, topos)

if item.itemid == 1945 then

	doTransformItem(item.uid,1946)
	doTransformItem(21000,1220)
 else
 	doTransformItem(item.uid,1945)
	estatuapos = {x=609, y=140, z=14, stackpos=253}
	creature = getThingfromPos(estatuapos)
	if creature.itemid > 0 then
		estatuapos.x = estatuapos.x + 1
		doTeleportThing(creature.uid,estatuapos)
	end
 	doTransformItem(item.uid,1945)
 	doTransformItem(21000,1219)
 
end

 return 1
end

