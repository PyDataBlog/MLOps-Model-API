/*
 * @author     ucchy
 * @license    LGPLv3
 * @copyright  Copyright ucchy 2014
 */
package org.bitbucket.ucchy.delay;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.scheduler.BukkitRunnable;

/**
 * Delay Command Plugin
 * @author ucchy
 */
public class DelayCommandPlugin extends JavaPlugin {

    /**
     * @see org.bukkit.plugin.java.JavaPlugin#onCommand(org.bukkit.command.CommandSender, org.bukkit.command.Command, java.lang.String, java.lang.String[])
     */
    @Override
    public boolean onCommand(CommandSender sender, Command command, String label, String[] args) {

        if ( args.length < 2 ) {
            return false;
        }

        final ArrayList<String> commands = new ArrayList<String>();
        boolean isAsync = false;
        int ticks = 0;

        if ( isDigit(args[0]) ) {
            // /delay (ticks) (command...)

            for ( int index=1; index<args.length; index++ ) {
                commands.add(args[index]);
            }

            ticks = Integer.parseInt(args[0]);

        } else if ( args[0].equalsIgnoreCase("async") && isDigit(args[1]) && args.length >= 3 ) {
            // /delay async (ticks) (command...)

            isAsync = true;
            for ( int index=2; index<args.length; index++ ) {
                commands.add(args[index]);
            }

            ticks = Integer.parseInt(args[1]);

        } else {
            return false;
        }

        final CommandSender comSender = sender;

        BukkitRunnable runnable = new BukkitRunnable() {

            @Override
            public void run() {
                StringBuilder builder = new StringBuilder();
                for ( String com : commands ) {
                    builder.append(com + " ");
                }
                Bukkit.dispatchCommand(comSender, builder.toString().trim());
            }

        };

        if ( isAsync ) {
            runnable.runTaskLaterAsynchronously(this, ticks);
        } else {
            runnable.runTaskLater(this, ticks);
        }

        return true;
    }

    private boolean isDigit(String value) {
        return value.matches("^[0-9]{1,9}$");
    }
}
