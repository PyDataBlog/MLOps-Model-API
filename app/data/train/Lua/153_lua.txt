dofile(minetest.get_modpath(minetest.get_current_modname()).."/zcg.lua")
dofile(minetest.get_modpath(minetest.get_current_modname()).."/cooking.lua")
dofile(minetest.get_modpath(minetest.get_current_modname()).."/protection.lua")
dofile(minetest.get_modpath(minetest.get_current_modname()).."/master.lua")

--Inventory Plus
inventory_plus = {}

inventory_plus.set_inventory_formspec = function(player, formspec)
	minetest.show_formspec(player:get_player_name(), "custom", formspec)
end

minetest.register_on_player_receive_fields(function(player,formname,fields)
	if fields.main then
		local name = player:get_player_name()
		local formspec_armor = armor:get_armor_formspec(name)
		if formspec_armor ~= nil then
			minetest.show_formspec(player:get_player_name(), "armor", formspec_armor)
		end
	end
end)

--book
minetest.register_craft({
	output = 'encyclopedia:crafts_book',
	recipe = {
		{'group:stick', 'group:stick', 'group:stick'},
		{'group:stick', 'default:book', 'group:stick'},
		{'group:stick', 'group:stick', 'group:stick'},
	}
})

minetest.register_craft({
	output = 'encyclopedia:cooking_book',
	recipe = {
		{'default:coal_lump', 'default:coal_lump', 'default:coal_lump'},
		{'default:coal_lump', 'default:book', 'default:coal_lump'},
		{'default:coal_lump', 'default:coal_lump', 'default:coal_lump'},
	}
})

minetest.register_craft({
	output = 'encyclopedia:protection_book',
	recipe = {
		{'default:steel_ingot', 'default:steel_ingot', 'default:steel_ingot'},
		{'default:steel_ingot', 'encyclopedia:crafts_book', 'default:steel_ingot'},
		{'default:steel_ingot', 'default:steel_ingot', 'default:steel_ingot'},
	}
})


minetest.register_craft({
	output = 'encyclopedia:master_book',
	recipe = {
		{'encyclopedia:cooking_book', '', ''},
		{'encyclopedia:protection_book', '', ''},
		{'encyclopedia:crafts_book', '', ''},
	}
})
