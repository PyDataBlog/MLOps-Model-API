package com.orcinuss.reinforcedtools.item.tools;

import com.google.common.collect.Sets;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraft.item.EnumRarity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemTool;

import java.util.Set;

public class ItemRTMHarvester extends ItemTool{

    private static final Set blocksToBreak = Sets.newHashSet(new Block[]{Blocks.glowstone});

    public EnumRarity rarity;
    public ItemRTMHarvester(Item.ToolMaterial material)
    {
        this(material, EnumRarity.common);
    }
    public ItemRTMHarvester(Item.ToolMaterial material, EnumRarity rarity ){
        super(0.0F, material, blocksToBreak);
        this.rarity = rarity;
        this.maxStackSize = 1;
    }



}
