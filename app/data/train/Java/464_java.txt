package com.calebmeyer.bettercrafting.creativetab;

import com.calebmeyer.bettercrafting.constants.Project;
import com.calebmeyer.bettercrafting.initialization.ModItems;

import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.Item;

public class CreativeTabBCT {
    public static final CreativeTabs BETTER_CRAFTING_TABLES_TAB = new CreativeTabs(Project.MOD_ID) {

        /**
         * This method returns an item, whose icon is used for this creative tab
         *
         * @return an item to use for the creative tab's icon
         */
        @Override
        public Item getTabIconItem() {
            return ModItems.craftingPanel;
        }

        /**
         * Gets the label for this creative tab.
         *
         * @return the label
         */
        @Override
        public String getTranslatedTabLabel() {
            return Project.MOD_NAME;
        }
    };
}
