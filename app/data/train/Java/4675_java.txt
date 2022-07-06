package command;

import org.junit.Test;

/**
 * Created by eder on 19/10/15.
 */
public class RemoteCeilingFanTest {

    @Test
    public void testRemoteControlCeilingFan(){
        RemoteControl remoteControl = new RemoteControl();

        CeilingFan ceilingFan = new CeilingFan("Living room");

        CeilingFanHightCommand ceilingFanHightCommand = new CeilingFanHightCommand(ceilingFan);
        CeilingFanMediumCommand ceilingFanMediumCommand = new CeilingFanMediumCommand(ceilingFan);
        CeilingFanOffCommand ceilingFanOffCommand = new CeilingFanOffCommand(ceilingFan);

        remoteControl.setCommand(0, ceilingFanHightCommand, ceilingFanOffCommand);
        remoteControl.setCommand(1, ceilingFanMediumCommand, ceilingFanOffCommand);

        remoteControl.onButtonWasPushed(0);
        remoteControl.offButtonWasPushed(0);
        System.out.println(remoteControl);
        remoteControl.undoButtonWasPushed();

        remoteControl.onButtonWasPushed(1);
        System.out.println(remoteControl);
        remoteControl.undoButtonWasPushed();


    }

}
