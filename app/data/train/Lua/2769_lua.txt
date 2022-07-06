
function onUse(cid, item, frompos, item2, topos)

if item.itemid == 1945 then

	doTransformItem(item.uid,1946)
	doTransformItem(1200,469)
 else
 	doTransformItem(item.uid,1945)
	estatuapos = {x=454, y=434, z=7, stackpos=253}
	creature = getThingfromPos(estatuapos)
	if creature.itemid > 0 then
		estatuapos.y = estatuapos.y + 1
		doTeleportThing(creature.uid,estatuapos)
	end
 	doTransformItem(1200,1444)
 end

 return 1
end