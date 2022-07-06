-- CONFIG
local WIDTH = 300
local HEIGHT = 30
local X = 0
local Y = -170

local moving = 0
local autorun = false
local cameraMove = false
local turnOrAction = false

local barShow = 0
local nextAuto
local autoShotActive = false

local barFrame = CreateFrame("Frame", "AutoShotBarFrame", UIParent)
barFrame:SetScale(768 / GetCVar("gxResolution"):match("%d+x(%d+)") / GetCVar("uiscale"))
barFrame:SetHeight(HEIGHT)
barFrame:SetWidth(WIDTH)
barFrame:SetPoint("CENTER", X, Y)

barFrame:SetBackdrop({
	bgFile = [[Interface\AddOns\TaliaivoAutoShot\8x8.blp]],  
	edgeFile = [[Interface\AddOns\TaliaivoAutoShot\8x8.blp]],
	tile = false,
	tileSize = 1,
	edgeSize = 1,
	insets = {
		left = 1,
		right = 1,
		top = 1,
		bottom = 1,
	}
})
barFrame:SetBackdropColor(0, 0, 0, 0.7)
barFrame:SetBackdropBorderColor(0, 0, 0)

barFrame:Hide()

local bar = CreateFrame("StatusBar", "AutoShotBar", barFrame)
bar:SetStatusBarTexture([[Interface\AddOns\TaliaivoAutoShot\8x8.blp]])
bar:SetStatusBarColor(0.67, 0.83, 0.45)
bar:SetMinMaxValues(0, 100)
bar:SetPoint("BOTTOMLEFT", 1, 1)
bar:SetPoint("TOPRIGHT", -1, -1)

local text = bar:CreateFontString()
text:SetFont(STANDARD_TEXT_FONT, 20, "OUTLINE")
text:SetPoint("TOPRIGHT")

local stopText = bar:CreateFontString()
stopText:SetFont(STANDARD_TEXT_FONT, 20, "OUTLINE")
stopText:SetPoint("TOPLEFT")

bar:SetScript("OnUpdate", function(self, elapsed)
	if self.time then
		self.time = self.time + elapsed
		if self.time >= self.duration then
			self:SetValue(100)
			self.time = nil
			text:SetText(nil)
			if barShow == 0 then
				nextAuto = nil
				barFrame:Hide()
				stopText:SetText(nil)
			end
		else
			text:SetText(format("%.1f", self.duration - self.time))
			self:SetValue(100 * (self.time / self.duration))
		end
	end

	if moving ~= 0 and nextAuto and autoShotActive then
		local time = nextAuto - GetTime() - 0.5
		if time >= 0.2 then
			stopText:SetText(format("|cff00ff00%.1f|r", time))
		elseif time >= 0 then
			stopText:SetText(format("|cffffff00%.1f|r", time))
		else
			stopText:SetText(format("|cffff0000%.1f|r", time))
		end
	end
end)

local shotTimeIndicator = CreateFrame("Frame", "AutoShotBarIndicator", bar)
local bg = shotTimeIndicator:CreateTexture()
bg:SetTexture(0.8, 0.2, 0)
bg:SetAllPoints()
shotTimeIndicator:SetPoint("TOPRIGHT", bar, "BOTTOMRIGHT", 0, 5)

function bar:StartBar()
	self.time = 0
	nextAuto = nextAuto or GetTime()
	self.duration = max(0.5, nextAuto - GetTime())
	local x = bar:GetWidth() * (self.duration - 0.5) / self.duration
	shotTimeIndicator:SetPoint("BOTTOMLEFT", x, 0)
	barFrame:Show()
end

local f = CreateFrame("Frame")

f:RegisterEvent("UNIT_SPELLCAST_SUCCEEDED")
f:RegisterEvent("START_AUTOREPEAT_SPELL")
f:RegisterEvent("STOP_AUTOREPEAT_SPELL")
f:RegisterEvent("PLAYER_REGEN_DISABLED")
f:RegisterEvent("PLAYER_REGEN_ENABLED")
f:SetScript("OnEvent", function(self, event, unit, spell)
	if event == "UNIT_SPELLCAST_SUCCEEDED" then
		if unit == "player" and spell == "Auto Shot" then
			nextAuto = GetTime() + UnitRangedDamage("player")
			bar:StartBar()
		end
	elseif event == "PLAYER_REGEN_DISABLED" or event == "START_AUTOREPEAT_SPELL" then
		barShow = barShow + 1
		if event == "START_AUTOREPEAT_SPELL" then
			autoShotActive = true
			if moving == 0 and IsSpellInRange("Auto Shot") == 1 then bar:StartBar() end
		end
	elseif event == "PLAYER_REGEN_ENABLED" or event == "STOP_AUTOREPEAT_SPELL" then
		barShow = barShow - 1
		if barShow == 0 and not bar.time then
			nextAuto = nil
			barFrame:Hide()
			stopText:SetText(nil)
		end
		if event == "STOP_AUTOREPEAT_SPELL" then autoShotActive = false end
	end
end)

local function Start()
	moving = moving + 1
end

local function StartAndStopAutorun()
	Start()
	if autorun then
		moving = moving - 1
		autorun = false
	end
end

local function Stop()
	moving = moving - 1
	if moving <= 0 then
		if nextAuto then
			local time = nextAuto - GetTime() - 0.5
			if time >= 0.2 then
				stopText:SetText(format("|cff00ff00%.1f|r", time))
			elseif time >= 0 then
				stopText:SetText(format("|cffffff00%.1f|r", time))
			else
				stopText:SetText(format("|cffff0000%.1f|r", time))
			end
		end
		if autoShotActive and IsSpellInRange("Auto Shot") == 1 then bar:StartBar() end
	end
end

hooksecurefunc("MoveForwardStart", StartAndStopAutorun)
hooksecurefunc("MoveBackwardStart", StartAndStopAutorun)
hooksecurefunc("StrafeLeftStart", Start)
hooksecurefunc("StrafeRightStart", Start)
hooksecurefunc("MoveAndSteerStart", StartAndStopAutorun)

hooksecurefunc("MoveForwardStop", Stop)
hooksecurefunc("MoveBackwardStop", Stop)
hooksecurefunc("StrafeLeftStop", Stop)
hooksecurefunc("StrafeRightStop", Stop)
hooksecurefunc("MoveAndSteerStop", Stop)

hooksecurefunc("CameraOrSelectOrMoveStart", function()
	cameraMove = true
	if turnOrAction then
		StartAndStopAutorun()
	end
end)
hooksecurefunc("CameraOrSelectOrMoveStop", function()
	cameraMove = false
	if turnOrAction then
		Stop()
	end
end)

hooksecurefunc("TurnOrActionStart", function()
	turnOrAction = true
	if cameraMove then
		StartAndStopAutorun()
	end
end)
hooksecurefunc("TurnOrActionStop", function()
	turnOrAction = false
	if cameraMove then
		Stop()
	end
end)

hooksecurefunc("ToggleAutoRun", function()
	autorun = not autorun
	if autorun then
		Start()
	else
		Stop()
	end
end)

local jumping = false

local function Jump_OnUpdate(self)
	if not IsFalling() then
		Stop()
		jumping = false
		self:SetScript("OnUpdate", nil)
	end
end

hooksecurefunc("JumpOrAscendStart", function()
	if not jumping then
		Start()
		jumping = true
		f:SetScript("OnUpdate", Jump_OnUpdate)
	end
end)