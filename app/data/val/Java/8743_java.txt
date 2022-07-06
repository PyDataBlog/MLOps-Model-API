package com.kingothepig.tutorialmod.creativetab;

import com.kingothepig.tutorialmod.init.ModItems;
import com.kingothepig.tutorialmod.reference.Reference;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.Item;

public class CreativeTabMod {
    public static final CreativeTabs MOD_TAB = new CreativeTabs(Reference.MOD_ID.toLowerCase()){
        @Override
        public Item getTabIconItem(){
            return ModItems.mapleLeaf;
        }
    };
}
