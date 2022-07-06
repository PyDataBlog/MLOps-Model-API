-- You now have a pet!

local function speak(event)
    -- Your pet should respond with pet.say()
    pet:say("Mooh!")
end

-- This tells your pet to run the speak() function when she hears something
pet:on("hear", speak)

-- Talk to your pet!
hero:say("Hello Kitty")
