local player = game.Players.LocalPlayer
local mouse = player:GetMouse()
local camera = game.Workspace.CurrentCamera
local screen = player.PlayerGui:WaitForChild("Screen")

do -- Visual Renders
	holo = Instance.new("Part", camera) do
		holo.Anchored = true
		holo.FormFactor = Enum.FormFactor.Custom
		do
			holo.BackSurface = Enum.SurfaceType.SmoothNoOutlines
			holo.BottomSurface = Enum.SurfaceType.SmoothNoOutlines
			holo.FrontSurface = Enum.SurfaceType.SmoothNoOutlines
			holo.LeftSurface = Enum.SurfaceType.SmoothNoOutlines
			holo.RightSurface = Enum.SurfaceType.SmoothNoOutlines
			holo.TopSurface = Enum.SurfaceType.SmoothNoOutlines
		end
		holo.Size = Vector3.new(4,4,4)
		holo.BrickColor = BrickColor.Blue()
		holo.CanCollide = false
		holo.Transparency = 0.5
		holo.Name = "Builder"
	end
	outline = Instance.new("SelectionBox", holo) do
		outline.Adornee = holo
		outline.SurfaceTransparency = 0.5
		outline.Transparency = 0
		outline.LineThickness = 0.05
		outline.Color3 = BrickColor.Blue().Color
		outline.SurfaceColor3 = BrickColor.Blue().Color
		outline.Name = "Outline"
	end
	local beam = Instance.new("BlockMesh", holo) do
		beam.Scale = Vector3.new(0.99, 300, 0.99)
		beam.Name = "Beam"
	end
	mouse.TargetFilter = holo
end

do -- Verify Collisions
	collider = Instance.new("Part", holo) do
		collider.Anchored = true
		collider.FormFactor = Enum.FormFactor.Custom
		do
			collider.BackSurface = Enum.SurfaceType.SmoothNoOutlines
			collider.BottomSurface = Enum.SurfaceType.SmoothNoOutlines
			collider.FrontSurface = Enum.SurfaceType.SmoothNoOutlines
			collider.LeftSurface = Enum.SurfaceType.SmoothNoOutlines
			collider.RightSurface = Enum.SurfaceType.SmoothNoOutlines
			collider.TopSurface = Enum.SurfaceType.SmoothNoOutlines
		end
		collider.Transparency = 1
		collider.CanCollide = false
		collider.Name = "Collision Detector"
	end
	local sizeMinimizer = Vector3.new(-0.03, -0.03, -0.03)
	holo.Changed:connect(function()
		collider.Size = holo.Size + sizeMinimizer
		collider.Position = holo.CFrame.p
	end)
	collider.Size = holo.Size + sizeMinimizer
		collider.Position = holo.CFrame.p
end

local function visualRender()
	local hit, target = mouse.Hit, mouse.Target
	if hit and target then
		local renderPos = Vector3.new(
			math.floor(hit.p.X + 0.5),
			math.floor(hit.p.Y + 0.5),
			math.floor(hit.p.Z + 0.5)
		)
		if mouse.TargetSurface == Enum.NormalId.Top then
			renderPos = renderPos + Vector3.new(0, holo.Size.Y/2, 0)
		elseif mouse.TargetSurface == Enum.NormalId.Bottom then
			renderPos = renderPos + Vector3.new(0, -holo.Size.Y/2, 0)
		elseif mouse.TargetSurface == Enum.NormalId.Front then
			renderPos = renderPos + Vector3.new(0, 0, -holo.Size.Y/2)
		elseif mouse.TargetSurface == Enum.NormalId.Back then
			renderPos = renderPos + Vector3.new(0, 0, holo.Size.Y/2)
		elseif mouse.TargetSurface == Enum.NormalId.Left then
			renderPos = renderPos + Vector3.new(-holo.Size.Y/2, 0, 0)
		elseif mouse.TargetSurface == Enum.NormalId.Right then
			renderPos = renderPos + Vector3.new(holo.Size.Y/2, 0, 0)
		end
		holo.CFrame = CFrame.new(renderPos)
	end
end

do -- Material GUI
	local materialPicker = screen.BuilderControls.Material
	materialSelection = Enum.Material.SmoothPlastic
	materialPicker.Display.MouseButton1Down:connect(function()
		materialPicker.Options.Visible = true
	end)
	materialPicker.Options.MouseLeave:connect(function()
		materialPicker.Options.Visible = false
	end)
	local options = materialPicker.Options
	local materials = {
		"Brick", "Cobblestone", "Concrete", "CorrodedMetal", "DiamondPlate", "Fabric", "Foil",
		"Granite", "Grass", "Marble", "Metal", "Neon", "Pebble", "Plastic", "Sand",
		"Slate", "SmoothPlastic", "Wood", "WoodPlanks"
	}
	for index, material in pairs(materials) do
		local button = options.MaterialTemplate:Clone()
		button.Parent = options
		button.Name, button.Text = material, material
		button.Position = UDim2.new(0, 0, 0, (index-1) * 27)
		options.CanvasSize = UDim2.new(0, 0, 0, (index) * 27)
		button.Visible = true
		button.MouseButton1Down:connect(function()
			materialPicker.Display.Text = button.Text
			materialSelection = Enum.Material[button.Name]
			options.Visible = false
		end)
	end
end

do -- Color GUI
	local colorPicker = screen.BuilderControls.Color
	colorSelection = BrickColor.palette(math.random(0, 63)) do
		colorPicker.BackgroundColor3 = colorSelection.Color
		colorPicker.Display.Text = colorSelection.Name
	end
	colorPicker.Display.MouseButton1Down:connect(function()
		colorPicker.Options.Visible = true
	end)
	colorPicker.Options.MouseLeave:connect(function()
		colorPicker.Options.Visible = false
	end)
	local options = colorPicker.Options
	local colors = 63
	for x = 0, colors do
		local button = options.ColorTemplate:Clone()
		button.Parent = options
		button.Name = tostring(x)
		button.Text = BrickColor.palette(x).Name
		button.BackgroundColor3 = BrickColor.palette(x).Color
		
		button.Position = UDim2.new(0, 0, 0, x * 27)
		options.CanvasSize = UDim2.new(0, 0, 0, x * 27)
		button.Visible = true
		
		button.MouseButton1Down:connect(function()
			local color = BrickColor.palette(tonumber(button.Name))
			colorPicker.Display.Text = color.Name
			colorPicker.BackgroundColor3 = color.Color
			colorSelection = color
			print(color.Name)
			options.Visible = false
		end)
	end
end

local buildDebounce = true
local function build()
	if buildDebounce then
	  buildDebounce = false
		collider.CanCollide = true
		local collisions = collider:GetTouchingParts()
		collider.CanCollide = false
		if #collisions <= 0 then
			local block = Instance.new("Part", game.Workspace)
			block.Anchored = true
			block.FormFactor = Enum.FormFactor.Custom
			do
				block.BackSurface = Enum.SurfaceType.SmoothNoOutlines
				block.BottomSurface = Enum.SurfaceType.SmoothNoOutlines
				block.FrontSurface = Enum.SurfaceType.SmoothNoOutlines
				block.LeftSurface = Enum.SurfaceType.SmoothNoOutlines
				block.RightSurface = Enum.SurfaceType.SmoothNoOutlines
				block.TopSurface = Enum.SurfaceType.SmoothNoOutlines
			end
			block.Size = holo.Size
			block.CFrame = CFrame.new(holo.CFrame.p)
			block.Transparency = 0
			block.CanCollide = true
			block.Material = materialSelection
			block.BrickColor = colorSelection
			block.Name = "Block"
			wait(0.05)
		else
			outline.Color3 = BrickColor.Red().Color
			wait(0.2)
			outline.Color3 = BrickColor.Blue().Color
		end
	  buildDebounce = true
	end
	visualRender()
end

mouse.Move:connect(visualRender)
mouse.Button1Down:connect(build)
visualRender()
