package com.tage.rpgutil.client.gui;

import net.minecraftforge.client.event.RenderGameOverlayEvent;
import cpw.mods.fml.common.eventhandler.Cancelable;
import cpw.mods.fml.common.eventhandler.Event;
import cpw.mods.fml.common.eventhandler.EventPriority;

@Cancelable
public class GuiHealth extends Event
{
    public static enum ElementType
    {
        ARMOR,
        HEALTH,
    }
}