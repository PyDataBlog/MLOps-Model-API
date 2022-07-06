-- Scripted by Fkids ( http://fkids.net )

class "Spinners"

function Spinners:__init()

	-- Key bindings
	self.nextKey = 38
	self.prevKey = 40
	-- Currently set to arrow key up/down
	-- You can change it to string.byte("KeyHere")
	
	-- Applies to all players
	self.preset = "Forearm"
	-- "Head", "Forearm", "Default".
	
	-- Show name of active particle system
	self.debug = false

	-- List of enabled particle systems
	self.path = {
		"cs_bloodshot_01.psmb",
		"cs_carwindow_01.psmb",
		"cs_electric_small_01.psmb",
		"cs_electric_small_02.psmb",
		"cs_fire_small_01.psmb",
		"cs_fire_small_02.psmb",
		"cs_steam_small_01.psmb",
		"fx_bubble.psmb",
		"fx_bulhit_flash_strong_large_03.psmb",
		"fx_bulhit_flash_strong_medium_01.psmb",
		"fx_bulhit_flesh_01.psmb",
		"fx_bulhit_metal_medium_05.psmb",
		"fx_bulhit_metal_medium_06.psmb",
		"fx_bulhit_metal_medium_07.psmb",
		"fx_bulhit_metal_medium_08.psmb",
		"fx_bulhit_metal_small_01.psmb",
		"fx_bulhit_metal_small_02.psmb",
		"fx_bulhit_metal_small_03.psmb",
		"fx_bulhit_metal_small_04.psmb",
		"fx_bulhit_metal_small_05.psmb",
		"fx_bulhit_snow_small_01.psmb",
		"fx_c4_flash_01.psmb",
		"fx_c4_flash_02.psmb",
		"fx_env_pipeline_ventpop_360_10.psmb",
		"fx_exp_electric_large_10.psmb"
	}
		
	Events:Subscribe("PostTick", self, self.PostTick)
	Events:Subscribe("Render", self, self.Render)
	Events:Subscribe("KeyUp", self, self.KeyUp)
	
end

function Spinners:KeyUp(args)

	if LocalPlayer:InVehicle() then return end
	
	if args.key == self.nextKey then
		if (LocalPlayer:GetValue("Spinner") or 0) + 1 > #self.path then
			Network:Send("SetSpinner", 0)
		else
			Network:Send("SetSpinner", (LocalPlayer:GetValue("Spinner") or 0) + 1)
		end
		return
	end
	
	if args.key == self.prevKey then
		if (LocalPlayer:GetValue("Spinner") or 0) - 1 < 0 then
			Network:Send("SetSpinner", #self.path)
		else
			Network:Send("SetSpinner", (LocalPlayer:GetValue("Spinner") or 0) - 1)
		end
		return
	end
	
end

function Spinners:PostTick(pid)

	local players = {}
	for player in Client:GetStreamedPlayers() do
		table.insert(players, player)
	end
	table.insert(players, LocalPlayer)
	
	for _, player in pairs(players) do
		if IsValid(player) and not player:InVehicle() then
			local value = player:GetValue("Spinner") or 0
			if self.path[value] then
				local pos = player:GetPosition()
				local dist = 1
				if self.preset == "Head" then
					pos = player:GetBonePosition("ragdoll_Head") + Vector3(0, 0.1, 0)
					dist = 0.2
				end
				if self.preset == "Forearm" then
					pos = player:GetBonePosition("ragdoll_LeftForeArm") - Vector3(0, 0.4, 0) - player:GetAngle() * Vector3(0.05, 0, 0)
					dist = 0.1
				end
				local angle = Angle(math.rad(Game:GetTime()*1000), 0, 0)
				ClientParticleSystem.Play(AssetLocation.Game, {
					path = self.path[value],
					position = pos + angle * Vector3(dist, 0.1, 0),
					angle = angle
				})
				angle.yaw = angle.yaw + math.pi
				ClientParticleSystem.Play(AssetLocation.Game, {
					path = self.path[value],
					position = pos + angle * Vector3(dist, 0.1, 0),
					angle = angle
				})
			end
		end
    	end
	
end

function Spinners:Render()
	if not self.debug then return end
	Render:DrawText(Vector2(200, 40), tostring(self.path[self.values[LocalPlayer:GetId() + 1]] or ""), Color(255, 255, 255), TextSize.Large)
end

Spinners = Spinners()
