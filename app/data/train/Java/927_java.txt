package Eac.event;

import Eac.Eac;
import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.gameevent.PlayerEvent;
import net.minecraft.item.ItemStack;

public class EacOnItemPickup extends Eac {
    @SubscribeEvent
    public void EacOnItemPickup(PlayerEvent.ItemPickupEvent e) {
        if (e.pickedUp.getEntityItem().isItemEqual(new ItemStack(oreAir))) {
            e.player.addStat(airoremined, 1);
        }
        else if (e.pickedUp.getEntityItem().isItemEqual(new ItemStack(oreShadow))) {
            e.player.addStat(shadoworemined, 1);
        }
    }
}



