local keywordHandler = KeywordHandler:new()
local npcHandler = NpcHandler:new(keywordHandler)
NpcSystem.parseParameters(npcHandler)
local talkState = {}

function onCreatureAppear(cid)                npcHandler:onCreatureAppear(cid)            end
function onCreatureDisappear(cid)             npcHandler:onCreatureDisappear(cid)            end
function onCreatureSay(cid, type, msg)            npcHandler:onCreatureSay(cid, type, msg)        end
function onThink()                    npcHandler:onThink()                    end

local shopModule = ShopModule:new()
npcHandler:addModule(shopModule)

shopModule:addBuyableItem({'beer bottle'}, 7495, 100, 'beer bottle')
shopModule:addBuyableItem({'fresh mushroom beer'}, 18305, 500, 'fresh mushroom beer')
shopModule:addBuyableItem({'stale mushrom beer'}, 18306, 500, 'stale mushrom beer')

npcHandler:addModule(FocusModule:new())