function fourthMapItems()
    itemsLayer.sprites = {
        earthArtefact = {
            image = love.graphics.newImage(
                "assets/sprites/items/8-artefacts/earth.png"),
            x = 405,
            y = 405,
            active = active24
        },
        windArtefact = {
            image = love.graphics.newImage(
                "assets/sprites/items/8-artefacts/wind.png"),
            x = 1455,
            y = 405,
            active = active25
        },
        fireArtefact = {
            image = love.graphics.newImage(
                "assets/sprites/items/8-artefacts/fire.png"),
            x = 405,
            y = 750,
            active = active26
        },
        iceArtefact = {
            image = love.graphics.newImage(
                "assets/sprites/items/8-artefacts/ice.png"),
            x = 1455,
            y = 750,
            active = active27
        }
    }
end