-- Main file used to set up the basics for this project, including flurry, any ads and also push notifications

local analytics = require("analytics")
local storyboard = require("storyboard")

-- analytics and push
local flurryKey = ""
local appKey

-- splash screen or intro video

-- application specific setup and first scene load (usually the main menu)
storyboard.loadScene("MainMenu")

