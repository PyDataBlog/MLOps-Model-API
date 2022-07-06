package org.venice.venicemod.generations;

import java.util.Random;

import org.venice.venicemod.VeniceMod;

import cpw.mods.fml.common.IWorldGenerator;
import net.minecraft.world.World;
import net.minecraft.world.chunk.IChunkProvider;

public class TitaniumGeneraton extends OreGeneration  {

	public TitaniumGeneraton(){
		this.setInBlock( VeniceMod.oreTitanium );
		this.setAppearInOverworld( true );
		this.setChans( 20 );
		this.setMinY( 50 );
		this.setMaxY( 100 );
		this.setMinVienSize( 6 );
		this.setMaxVienSize( 15 );
	}

}
