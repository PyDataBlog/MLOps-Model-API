local combat = Combat()
combat:setParameter(COMBAT_PARAM_TYPE, COMBAT_DROWNDAMAGE)
combat:setParameter(COMBAT_PARAM_EFFECT, CONST_ME_YELLOW_RINGS)

local area = createCombatArea({
{0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0},
{0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0},
{0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0},
{0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0},
{0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0},
{1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1},
{0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0},
{0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0},
{0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0},
{0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0},
{0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0},
}
)
combat:setArea(area)
combat:setCondition(condition1)
combat:setCondition(condition2)

local condition2 = Condition(CONDITION_DRUNK)
condition2:setParameter(CONDITION_PARAM_TICKS, 40000)
combat:setCondition(condition2)

local condition1 = Condition(CONDITION_DROWN)
condition1:setTicks(40000)
condition1:setParameter(CONDITION_PARAM_DELAYED, 1)
condition1:setParameter(CONDITION_PARAM_TICKINTERVAL, 1500)


local function CastSpell(cid, var)
    local player = Player(cid)
    local level = player:getLevel()
    local maglevel = player:getMagicLevel()
    min = -((level / 5) + (maglevel * 0.5) + 6)
    max = -((level / 5) + (maglevel * 0.7) + 7)
    condition1:setParameter(CONDITION_PARAM_PERIODICDAMAGE, math.random(min,max))
    combat:setCondition(condition1)
	return true
    end
   
function onCastSpell(creature, var)
    CastSpell(creature:getId(), var)
    return combat:execute(creature, var)
end