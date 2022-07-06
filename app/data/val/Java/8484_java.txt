package com.meep.core.libs.configs;

import net.minecraftforge.common.config.Configuration;

/**
 * Created by poppypoppop on 22/12/2014.
 */
public class BiomeConfigHandler {
    //Catagories
    public static String general = "General";
    public static String ids = "Biome IDs";

    //Options
    public static int oasisID;
    public static void configOptions(Configuration config) {
        oasisID = config.getInt("Oasis Biome ID", ids, 40, 40, 128, "Oasis Biome ID");
    }
}
