data:extend ({
	{
		type = "technology",
		name = "projectx",
		icon = "__projectx__/graphics/technologies/projectx.png",
		icon_size = 128,
		effects =
		{
			{
				type = "unlock-recipe",
				recipe = "express-inserter"
			}
		},
		prerequisites = {"logistics-3", "automation-3"},
		unit =
		{
			count = 300,
			ingredients =
			{
				{"science-pack-1", 1},
				{"science-pack-2", 1},
				{"science-pack-3", 1}
			},
			time = 15
		},
	}
})
