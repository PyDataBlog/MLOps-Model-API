package Yogibear617.mods.FuturisticCraft.CreativeTabs;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import net.minecraft.block.Block;
import net.minecraft.creativetab.CreativeTabs;

public class FCSBModCreativeTab extends CreativeTabs {

	public FCSBModCreativeTab(int par1, String par2Str) {
		super(par1, par2Str);
	}
	@SideOnly(Side.CLIENT)
	public int getTabIconItemIndex() {
		return Block.stoneBrick.blockID;
	}

}
