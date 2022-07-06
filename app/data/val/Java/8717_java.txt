package com.neveagleson.ecocraft.core.utility;

import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.entity.player.EntityPlayer;

/**
 * Created by NevEagleson on 4/03/2015
 */
public class PlayerEntityFilter extends EntityFilter
{
    @Override
    public boolean isEntityValid(EntityLivingBase entity)
    {
        return entity instanceof EntityPlayer != negate;
    }
}
