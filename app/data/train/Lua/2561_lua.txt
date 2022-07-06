---------------------------------------------------------------------------------------------------
-- func: menu test
-- desc: used to test menu's
---------------------------------------------------------------------------------------------------

cmdprops =
{
    permission = 3,
    parameters = "i"
};

function onTrigger(player,menu)
    player:sendMenu(menu);
end