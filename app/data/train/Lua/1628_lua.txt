data:extend({
    {
        type = "solar-panel",
        name = "solar-panel-v2",
        icon = "__TechTier5Extension__/graphics/icons/solar-panel-v2.png",
        flags = {"placeable-neutral", "player-creation"},
        minable = {hardness = 0.2, mining_time = 0.5, result = "solar-panel-v2"},
        max_health = 100,
        corpse = "big-remnants",
        collision_box = {{-1.4, -1.4}, {1.4, 1.4}},
        selection_box = {{-1.5, -1.5}, {1.5, 1.5}},
        energy_source =
        {
            type = "electric",
            usage_priority = "primary-output"
        },
        picture =
        {
            filename = "__base__/graphics/entity/solar-panel/solar-panel.png",
            priority = "high",
            width = 104,
            height = 96
        },
        production = "108kW"
    },
    {
        type = "item",
        name = "solar-panel-v2",
        icon = "__TechTier5Extension__/graphics/icons/solar-panel-v2.png",
        flags = {"goes-to-quickbar"},
        subgroup = "energy",
        order = "d[solar-panel]-a[solar-panel]",
        place_result = "solar-panel-v2",
        stack_size = 50
    },
    {
        type = "recipe",
        name = "solar-panel-v2",
        enabled = "false",
        ingredients =
        {
            {"solar-panel", 1},
      {"electronic-circuit", 20},
      {"copper-plate", 8},
            {"processing-unit", 4}
        },
        result = "solar-panel-v2"
    },
	    {
        type = "technology",
        name = "extended-energy",
        icon = "__TechTier5Extension__/graphics/tech/solar-panel-v2.png",
        effects =
        {
            {
                type = "unlock-recipe",
                recipe = "basic-accumulator-v2"
            },
            {
                type = "unlock-recipe",
                recipe = "solar-panel-v2"
            }
        },
        prerequisites = {
            "electric-energy-accumulators-1",
            "electric-energy-distribution-2",
            "solar-energy"
        },
        unit = {
            count = 200,
            ingredients = {
                { "science-pack-1", 1 },
                { "science-pack-2", 1 },
                { "science-pack-3", 1 }
            },
            time = 30
        },
        order = "c-k-c",
    }
    })