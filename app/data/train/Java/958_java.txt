package com.silvermatch.advancedMod.block;

import com.silvermatch.advancedMod.init.ModBlocks;
import com.silvermatch.advancedMod.reference.Reference;
import com.silvermatch.advancedMod.utility.Names;

import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.world.World;

public class BlockFrenchFlag extends BlockAdvancedMod{

	public BlockFrenchFlag() {
		setBlockName(Names.Blocks.FRENCH_FLAG);
		setBlockTextureName(Reference.MOD_ID_LOWER + ":" + Names.Blocks.FRENCH_FLAG);
	}
	
	@Override
	 public boolean onBlockActivated(World world, int x, int y, int z, EntityPlayer entityPlayer, int side, float hitX, float hitY, float hitZ)
	    {
			if (world.isAirBlock(x, y+1, z)){
				world.setBlock(x, y+1, z, ModBlocks.frenchFlag);	
			}
			
	        return true;
	    }
}
