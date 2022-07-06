ITEM.Name = 'Captain Hat'
ITEM.Price = 0
ITEM.Model = 'models/lordvipes/generalpepperhat/generalpepperhat.mdl'
ITEM.Attachment = 'eyes'

function ITEM:OnEquip(ply, modifications)
	ply:PS_AddClientsideModel(self.ID)
end

function ITEM:OnHolster(ply)
	ply:PS_RemoveClientsideModel(self.ID)
end

function ITEM:ModifyClientsideModel(ply, model, pos, ang)
	model:SetModelScale(0.775, 0)
	pos = pos + (ang:Forward() * -3.42) + (ang:Right() * -0.2) +  (ang:Up() * -0.001)
	ang:RotateAroundAxis(ang:Right(), 0)
	ang:RotateAroundAxis(ang:Up(), 0)
	ang:RotateAroundAxis(ang:Forward(), 0)
	
	return model, pos, ang
end