package team.gif;

import edu.wpi.first.wpilibj.Joystick;
import edu.wpi.first.wpilibj.buttons.Button;
import edu.wpi.first.wpilibj.buttons.DigitalIOButton;
import edu.wpi.first.wpilibj.buttons.JoystickButton;
import team.gif.commands.*;

public class OI {
    
    public static final Joystick leftStick = new Joystick(1);
    public static final rightStick = new Joystick(2);
    public static final auxStick = new Joystick(3);
    
    private final Button leftTrigger = new JoystickButton(leftStick, 1);
    private final Button right2 = new JoystickButton(rightStick, 2);
    private final Button right3 = new JoystickButton(rightStick, 3);
    private final Button right6 = new JoystickButton(rightStick, 6);
    private final Button right7 = new JoystickButton(rightStick, 7);
    
    public static final Button auxTrigger = new JoystickButton(rightStick, 1);
    
    public OI() {
        leftTrigger.whileHeld(new ShifterHigh());
        
        right2.whileHeld(new CollectorReceive());
        right2.whenPressed(new EarsOpen());
        
        right3.whileHeld(new CollectorPass());
        right3.whenPressed(new EarsOpen());
        right3.whenReleased(new CollectorStandby());
        right3.whenReleased(new EarsClosed());
        
        right6.whileHeld(new BumperUp());
        right7.whileHeld(new CollectorRaise());
    }
}

