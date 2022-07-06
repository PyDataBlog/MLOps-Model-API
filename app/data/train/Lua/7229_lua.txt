--declare variables to be used to figure out whether the player is using anti gravity, neg. gravity, etc.
antigravity = false
heavygravity = false
NegGravity = false
player = minetest.get_player_by_name("singleplayer")
player = minetest.get_player_by_name("singleplayer")
player = minetest.get_player_by_name("singleplayer")

minetest.register_craftitem("sciencing:AntiGravity", {
	description = "Self Contained Anti-Gravity Device",
	inventory_image = "sciencing_GenericControl.png",
	on_use = function (user)
		local player = minetest.get_player_by_name("singleplayer")
		if antigravity == false then
		player:set_physics_override({
			gravity = 0.1
		})
		antigravity = true
		heavygravity = false
		NegGravity = false
		--Helmet Update is a callback I made to be called upon every time the player changes gravity
		HelmetUpdate()
		else
		player:set_physics_override({
			gravity = 1
		})
		antigravity = false
		heavygravity = false
		NegGravity = false
		HelmetUpdate()
		end
		end
})

minetest.register_craft({
	output = "sciencing:AntiGravity 1",
	recipe = {
		{"default:steel_ingot", "default:steel_ingot", "default:steel_ingot"},
		{"default:steel_ingot", "default:mese_crystal", "default:steel_ingot"},
		{"", "", "default:steel_ingot"}
	}
})

minetest.register_craftitem("sciencing:AntiAntiGravity", {
	description = "Self Contained Reinforced-Gravity Device",
	inventory_image = "sciencing_GenericControlOpp.png",
	on_use = function (user)
		local player = minetest.get_player_by_name("singleplayer")
		if heavygravity == false then
		player:set_physics_override({
			gravity = 2
		})
		antigravity = false
		heavygravity = true
		NegGravity = false
		HelmetUpdate()
		else
		player:set_physics_override({
			gravity = 1
		})
		heavygravity = false
		antigravity = false
		NegGravity = false
		HelmetUpdate()
		end
		end
})

minetest.register_craft({
	output = "sciencing:AntiAntiGravity 1",
	recipe = {
		{"sciencing:AntiGravity", "sciencing:AntiGravity"}
	}
})

minetest.register_craftitem("sciencing:NegativeGravity", {
	description = "Self Contained Negative-Gravity Device",
	inventory_image = "sciencing_GenericControlNeg.png",
	on_use = function (user)
		local player = minetest.get_player_by_name("singleplayer")
		if NegGravity == false then
		player:set_physics_override({
			gravity = -1
		})
		NegGravity = true
		antigravity = false
		heavygravity = false
		HelmetUpdate()
		else
		player:set_physics_override({
			gravity = 1
		})
		NegGravity = false
		antigravity = false
		heavygravity = false
		HelmetUpdate()
		end
		end
})

minetest.register_craft({
	output = "sciencing:NegativeGravity 1",
	recipe = {
		{"sciencing:AntiGravity", "sciencing:AntiAntiGravity"}
	}
})

superboots = false

minetest.register_craftitem("sciencing:SuperBoots", {
	description = "Advanced Momentum Generating Boots",
	inventory_image = "sciencing_Boots.png",
	on_use = function (user)
		replace_with_item = nil
		if superboots == false then
		local player = minetest.get_player_by_name("singleplayer")
		superboots = true
		player:set_physics_override({
			jump = 2,
			speed = 2
		})
		else
		local player = minetest.get_player_by_name("singleplayer")
		player:set_physics_override({
			jump = 1,
			speed = 1
		})
		superboots = false
		end
		end
})

minetest.register_craft({
	output = "sciencing:SuperBoots 1",
	recipe = {
		{"default:steel_ingot", ""},
		{"default:steel_ingot", "default:steel_ingot"},
		{"sciencing:AntiGravity", "default:steel_ingot"}
	}
})

minetest.register_craftitem("sciencing:Pulverizer", {
	description = "Experimental Incendiary Device",
	inventory_image = "experimental_tester_tool_1.png",
	on_use = function (itemstack, user, pointed_thing)
		if pointed_thing.type == "node" then
			minetest.place_node(pointed_thing.above, {name="fire:basic_flame"})	
		end
	end
})

minetest.register_craft({
	output = "sciencing:Pulverizer 1",
	recipe = {
		{"default:steel_ingot", "default:steel_ingot", "default:steel_ingot"},
		{"", "default:mese_crystal", "sciencing:AntiAntiGravity"},
		{"default:steel_ingot", "default:steel_ingot", "default:steel_ingot"}
	}
})

--define the pug entity for use with the cannon

minetest.register_entity("sciencing:pug", {
	hp_max = 6,
	weight = 10,
	physical = true,
	timer=0,
	visual = "sprite",
	visual_size = {x=1, y=1},
	textures = {"sciencing_sciencePug.png"},
	lastpos={},
	collisionbox = {-0.3,-0.3,-0.5, 0.3,0.3,0.5},
	makes_footstep_sound = false,
    	automatic_rotate = false
})

minetest.register_craftitem("sciencing:PugCannon", {
	description = "50 Caliber Puggy Cannon",
	inventory_image = "Cannon.png",
	on_use = function (itemstack, user, pointed_thing)
		if pointed_thing.type == "node" then
			local person = minetest.get_player_by_name("singleplayer")
			local personpos = person:getpos()
			--spawn a pug at the player's position and use a lengthy distance formula to find the velocity, then multiply it by a number.
			local pug = minetest.add_entity({x=personpos.x, y=personpos.y + 1, z=personpos.z}, "sciencing:pug")
			pug:setvelocity({x= 2*(pointed_thing.above.x - personpos.x),y= 2 * (pointed_thing.above.y - personpos.y), z= 2*(pointed_thing.above.z - personpos.z)})
		end
	end
})

minetest.register_craft({
	output = "sciencing:PugCannon 1",
	recipe = {
		{"default:steel_ingot", "default:steel_ingot", "default:steel_ingot"},
		{"", "default:mese_crystal", "sciencing:AntiGravity"},
		{"default:steel_ingot", "default:steel_ingot", "default:steel_ingot"}
	}
})

helmet = false

local antdisplay
local hevdisplay
local negdisplay

minetest.register_craftitem("sciencing:SciFiHelmet", {
	description = "HEV Suit Helmet",
	inventory_image = "sciencing_helmet.png",
	on_use = function (itemstack, user, pointed_thing)
		if helmet == false then
		player = minetest.get_player_by_name("singleplayer")
		helmet = true
		--If I do not call HelmetUpdate when I put on the helmet, I will not see anything the helmet shows me until I use antigravity or something . . . 
		HelmetUpdate()
		else
		if idx then
		player:hud_remove(idx)
		end
		if idx2 then
			player:hud_remove(idx2)
		end
		if idx3 then
			player:hud_remove(idx3)
		end
		if View then
			player:hud_remove(View)
		end
		helmet = false
		end
	end
})
minetest.register_craft({
	output = "sciencing:SciFiHelmet 1",
	recipe = {
		{"default:steel_ingot", "default:steel_ingot", "default:steel_ingot"},
		{"default:steel_ingot", "default:mese_crystal", "default:steel_ingot"}
	}
})

--This is called every time you change gravity.
--I wanted to add a futuristic health display to this list, but there is no callback for the player's heath changing that I know of.
--If there is one that I am missing, I would appreciate if you let me know (or better yet, if you incorperated it yourself)

function HelmetUpdate()
	if helmet == true then
	if antigravity == true then
			antdisplay = "ON"
	else
			antdisplay = "OFF"
	end

	if heavygravity == true then
			hevdisplay = "ON"
	else
			hevdisplay = "OFF"
	end
	
	if NegGravity == true then
			negdisplay = "ON"
	else
			negdisplay = "OFF"
	end

	if idx then
		player:hud_remove(idx)
	end
	if idx2 then
		player:hud_remove(idx2)
	end
	if idx3 then
		player:hud_remove(idx3)
	end
	if View then
		player:hud_remove(View)
	end

	idx = player:hud_add({
		 	hud_elem_type = "text",
			position = {x = 0.1, y = 0.1},
		 	offset = {x=0, y = 0},
			scale = {x = 100, y = 100},
		 	text = "AntiGravity: " .. antdisplay,
			number = "0xFFFFFF"
	})
		idx2 = player:hud_add({
		 	hud_elem_type = "text",
			position = {x = 0.1, y = 0.13},
		 	offset = {x=0, y = 0},
			scale = {x = 100, y = 100},
		 	text = "Heavy Gravity: " .. hevdisplay,
			number = "0xFFFFFF"
		})
		idx3 = player:hud_add({
		 	hud_elem_type = "text",
			position = {x = 0.1, y = 0.16},
		 	offset = {x=0, y = 0},
			scale = {x = 100, y = 100},
		 	text = "Negative Gravity: " .. negdisplay,
			number = "0xFFFFFF"
		})
	View = player:hud_add({
        	hud_elem_type = "image",
        	position = {x = 0, y = 0},
        	offset = {x=0, y = 0},
		scale = {x = -100, y = -100},
		alignment = {x=1, y=1},
        	text = "HelmetView.png"
	})
else
if idx then
		player:hud_remove(idx)
	end
	if idx2 then
		player:hud_remove(idx2)
	end
	if idx3 then
		player:hud_remove(idx3)
	end
	if View then
		player:hud_remove(View)
	end
end
end


