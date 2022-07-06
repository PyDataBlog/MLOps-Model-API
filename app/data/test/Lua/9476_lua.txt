
-- Define Block Nodes

-------Define Blocks of Resources



minetest.register_node("morethings:red_block", {
	description = "Red Block",
	tiles = {"morethings_red_block.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = 'craft "morethings:red_ingot" 9',
	is_ground_content = true,
})

minetest.register_node("morethings:red_block2", {
	description = "Red Block2",
	tiles = {"red_block2.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = 'craft "morethings:red_ingot" 9',
	is_ground_content = true,
})

minetest.register_node("morethings:red_block2empowered", {
	description = "Red Block2Empowered",
	tiles = {"red_block2empowered.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = 'craft "morethings:red_ingot" 9',
	is_ground_content = true,
})

minetest.register_node("morethings:krypton_block", {
	description = "Krypton Block",
	tiles = {"morethings_krypton_block.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = 'craft "morethings:krypton_ingot" 9', 
	is_ground_content = true,
})

minetest.register_node("morethings:flint_block", {
	description = "Flint Block",
	tiles = {"morethings_flint_block.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = 'craft "default:flint" 9',
	is_ground_content = true,
})

minetest.register_node("morethings:ghost_block", {
	description = "Ghost Block",
	tiles = {"morethings_ghost_block.png"},
	light_source = default.LIGHT_MAX - 14,
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = 'craft "morethings:ghost_ingot" 9',
	is_ground_content = true,
})
-------Define Tiles

minetest.register_node("morethings:tile", {
	description = "Tile",
	tiles = {"morethings_tile.png"},
	groups = {stone=2, cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = "morethings:tile",
	is_ground_content = true,
})

minetest.register_node("morethings:mossy_tile", {
	description = "Mossy Tile",
	tiles = {"morethings_mossy_tile.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = "morethings:tile",
	is_ground_content = true,
})

minetest.register_node("morethings:iron_tile", {
	description = "Iron Tile",
	tiles = {"morethings_iron_tile.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = "morethings:iron_tile",
	is_ground_content = true,
})

minetest.register_node("morethings:copper_tile", {
	description = "Copper Tile",
	tiles = {"morethings_copper_tile.png"},
	groups = {cracky=3},
	drop = "morethings:copper_tile",
	is_ground_content = true,
})

minetest.register_node("morethings:bronze_tile", {
	description = "Bronze Tile",
	tiles = {"morethings_bronze_tile.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = "morethings:bronze_tile",
	is_ground_content = true,
})

minetest.register_node("morethings:gold_tile", {
	description = "Gold Tile",
	tiles = {"morethings_gold_tile.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = "morethings:gold_tile",
	is_ground_content = true,
})

minetest.register_node("morethings:mese_tile", {
	description = "MESE Tile",
	tiles = {"morethings_mese_tile.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = "morethings:mese_tile",
	is_ground_content = true,
})

minetest.register_node("morethings:diamond_tile", {
	description = "Diamond Tile",
	tiles = {"morethings_diamond_tile.png"},
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = "morethings:diamond_tile",
	is_ground_content = true,
})

-------Define Other Blocks

minetest.register_node("morethings:random_block", {
	description = "Random Block",
	tiles = {"morethings_random_block.png"},
	paramtype2 = "facedir",
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = "morethings:random_block",
	is_ground_content = true,
})

minetest.register_node("morethings:laminat", {
	description = "Laminat",
	tiles = {"morethings_laminat.png"},
	paramtype2 = "facedir",
	groups = {cracky=3},
	sounds = default.node_sound_wood_defaults(),
	drop = "morethings:laminat",
	is_ground_content = true,
})

minetest.register_node("morethings:parkett", {
	description = "Parkett",
	tiles = {"morethings_parkett.png"},
	paramtype2 = "facedir",
	groups = {cracky=3},
	sounds = default.node_sound_wood_defaults(),
	drop = "morethings:parkett",
	is_ground_content = true,
})

minetest.register_node("morethings:steel_plate", {
	description = "Steel Plate",
	drawtype = "glasslike_framed",

	tiles = {"steel_plate.png", "steel_plate_2.png"},
	inventory_image = minetest.inventorycube("steel_plate.png"),

	paramtype = "light",
	sunlight_propagates = true,
	is_ground_content = true,

	groups = {cracky = 3, },
	sounds = default.node_sound_stone_defaults()
})

minetest.register_node("morethings:carpet_brown", {
	description = "Brown Carpet",
	drawtype = "glasslike_framed",

	tiles = {"morethings_carpet_brown.png", "morethings_carpet_brown_full.png"},
	inventory_image = minetest.inventorycube("morethings_carpet_brown.png"),

	paramtype = "light",
	sunlight_propagates = true,
	is_ground_content = true,

	groups = {cracky = 3, },
	sounds = default.node_sound_stone_defaults()
})

minetest.register_node("morethings:carpet_orange", {
	description = "Orange Carpet",
	drawtype = "glasslike_framed",

	tiles = {"morethings_carpet_orange.png", "morethings_carpet_orange_full.png"},
	inventory_image = minetest.inventorycube("morethings_carpet_orange.png"),

	paramtype = "light",
	sunlight_propagates = true,
	is_ground_content = true,

	groups = {cracky = 3, },
	sounds = default.node_sound_stone_defaults()
})

-------Define Stairs



-------Define Model Depends Blocks

minetest.register_node("morethings:red_crystal", {
	description = "Red Crystal",
	mesh = "red_crystal.obj",
	tiles = {"red_crystal.png"},
	paramtype = "light",
	drawtype = "mesh",
	groups = {cracky = 1},
	drop = "morethings:red_crystal_shards",
	use_texture_alpha = true,
	sounds = default.node_sound_glass_defaults(),
	light_source = 10,
	selection_box = {
		type = "fixed",
		fixed = {-0.3, -0.5, -0.3, 0.3, 0.35, 0.3}
	},
})

minetest.register_node("morethings:red_ingot2", {
	description = "Red Ingot",
	mesh = "ingot.obj",
	tiles = {"morethings_red_ingot2.png"},
	paramtype = "light",
	drawtype = "mesh",
	groups = {cracky = 1},
	drop = "morethings:red_ingot",
	use_texture_alpha = true,
	sounds = default.node_sound_stone_defaults(),
	light_source = 10,
	selection_box = {
		type = "fixed",
		fixed = {-0.3, -0.5, -0.3, 0.3, 0.35, 0.3}
	},
})

minetest.register_node("morethings:red_crystal_empowered", {
	description = "Red Crystal Empowered",
	mesh = "red_crystal_empowered.obj",
	tiles = {"red_crystal_empowered.png"},
	paramtype = "light",
	drawtype = "mesh",
	groups = {cracky = 1},
	drop = "morethings:red_crystal3",
	use_texture_alpha = true,
	sounds = default.node_sound_glass_defaults(),
	light_source = 10,
	selection_box = {
		type = "fixed",
		fixed = {-0.3, -0.5, -0.3, 0.3, 0.35, 0.3}
	},
})

minetest.register_node("morethings:red_crystal4", {
	description = "Red Crystal Empowered2",
	mesh = "red_crystal_empowered.obj",
	tiles = {"red_crystal_empowered2.png"},
	paramtype = "light",
	drawtype = "mesh",
	groups = {cracky = 1},
	drop = "morethings:red_crystal4",
	use_texture_alpha = true,
	sounds = default.node_sound_glass_defaults(),
	light_source = 10,
	selection_box = {
		type = "fixed",
		fixed = {-0.3, -0.5, -0.3, 0.3, 0.35, 0.3}
	},
})

-------Define Light Blocks

minetest.register_node("morethings:lightblock", {
	description = "Light Block",
	tiles = {"morethings_lightblock.png"},
	light_source = default.LIGHT_MAX - 14,
	groups = {cracky=3},
	sounds = default.node_sound_stone_defaults(),
	drop = "morethings:lightblock",
	is_ground_content = true,
})

--Define Block Crafting recipes

minetest.register_craft({
	output = "morethings:red_block",
	recipe = {
		{"morethings:red_ingot", "morethings:red_ingot", "morethings:red_ingot"},
		{"morethings:red_ingot", "morethings:red_ingot", "morethings:red_ingot"},
		{"morethings:red_ingot", "morethings:red_ingot", "morethings:red_ingot"}
	}
})

minetest.register_craft({
	output = "morethings:krypton_block",
	recipe = {
		{"morethings:krypton_ingot", "morethings:krypton_ingot", "morethings:krypton_ingot"},
		{"morethings:krypton_ingot", "morethings:krypton_ingot", "morethings:krypton_ingot"},
		{"morethings:krypton_ingot", "morethings:krypton_ingot", "morethings:krypton_ingot"}
	}
})

minetest.register_craft({
	output = "morethings:ghost_block",
	recipe = {
		{"morethings:ghost_ingot", "morethings:ghost_ingot", "morethings:ghost_ingot"},
		{"morethings:ghost_ingot", "morethings:ghost_ingot", "morethings:ghost_ingot"},
		{"morethings:ghost_ingot", "morethings:ghost_ingot", "morethings:ghost_ingot"}
	}
})

minetest.register_craft({
	output = "morethings:flint_block",
	recipe = {
		{"default:flint", "default:flint", "default:flint"},
		{"default:flint", "default:flint", "default:flint"},
		{"default:flint", "default:flint", "default:flint"}
	}
})

minetest.register_craft({
	output = "morethings:lightblock",
	recipe = {
		{"default:stone", "default:cobble", "default:stone"},
		{"default:cobble", "default:torch", "default:cobble"},
		{"default:stone", "default:cobble", "default:stone"}
	}
})

minetest.register_craft({
	output = "morethings:random_block",
	recipe = {
		{"morethings:red_ingot_empowered", "morethings:red_ingot_empowered", "morethings:obsidian_ingot"},
		{"morethings:red_ingot_empowered", "morethings:red_ingot_empowered", "morethings:obsidian_ingot"},
		{"morethings:obsidian_ingot", "morethings:obsidian_ingot", "morethings:red_ingot_empowered"}
	}
})

minetest.register_craft({
	output = "morethings:parkett",
	recipe = {
		{"default:junglewood", "default:wood", "default:junglewood"},
		{"default:wood", "default:junglewood", "default:wood"},
		{"default:junglewood", "default:wood", "default:junglewood"}
	}
})

minetest.register_craft({
	output = "morethings:laminat",
	recipe = {
		{"default:wood", "default:junglewood", "default:wood"},
		{"default:junglewood", "default:wood", "default:junglewood"},
		{"default:wood", "default:junglewood", "default:wood"}
	}
})

minetest.register_craft({
	output = "morethings:tile",
	recipe = {
		{"", "default:stone", ""},
		{"default:stone", "default:stone", "default:stone"},
		{"", "default:stone", ""}
	}
})

minetest.register_craft({
	output = "morethings:iron_tile 6",
	recipe = {
		{"", "", ""},
		{"default:steel_ingot", "morethings:tile", ""},
		{"", "", ""}
	}
})

minetest.register_craft({
	output = "morethings:copper_tile 6",
	recipe = {
		{"", "", ""},
		{"default:copper_ingot", "morethings:tile", ""},
		{"", "", ""}
	}
})

minetest.register_craft({
	output = "morethings:bronze_tile 6",
	recipe = {
		{"", "", ""},
		{"default:bronze_ingot", "morethings:tile", ""},
		{"", "", ""}
	}
})

minetest.register_craft({
	output = "morethings:gold_tile 6",
	recipe = {
		{"", "", ""},
		{"default:gold_ingot", "morethings:tile", ""},
		{"", "", ""}
	}
})

minetest.register_craft({
	output = "morethings:mossy_tile 6",
	recipe = {
		{"", "", ""},
		{"default:mossycobble", "morethings:tile", ""},
		{"", "", ""}
	}
})

minetest.register_craft({
	output = "morethings:mese_tile 16",
	recipe = {
		{"", "", ""},
		{"default:mese_crystal", "morethings:tile", ""},
		{"", "", ""}
	}
})

minetest.register_craft({
	output = "morethings:mese_tile",
	recipe = {
		{"", "", ""},
		{"default:mese_shard", "morethings:tile", ""},
		{"", "", ""}
	}
})

minetest.register_craft({
	output = "morethings:diamond_tile 20",
	recipe = {
		{"", "", ""},
		{"default:diamond", "morethings:tile", ""},
		{"", "", ""}
	}
})