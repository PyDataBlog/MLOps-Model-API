package petrichor;
import behavior.Aggressive;
import behavior.Behavior;
import robocode.*;
import java.awt.Color;

/**
 * Petrichor - a robot by Piper Chester and Michael Timbrook
 */
public class Petrichor extends AdvancedRobot
{
    private Behavior active;

	/**
	 * Petrichor's default behavior. Initialization of robot occurs here.
	 */
	public void run() {
	
		setColors(Color.blue, Color.cyan, Color.green); // Body, gun, radar

		// Set behavior
        active = new Aggressive(this);

        // Sit in the behaviors run loop
        while (active.runLoop());

	}

	/**
	 * What to do when you see another robot.
	 */
	public void onScannedRobot(ScannedRobotEvent e) {
		active.onScannedRobot(e);
	}

	/**
	 * What to do when you're hit by a bullet.
	 */
	public void onHitByBullet(HitByBulletEvent e) {
		active.onHitByBullet(e);
	}
	
	/**
	 * onHitWall: What to do when you hit a wall
	 */
	public void onHitWall(HitWallEvent e) {
		active.onHitWall(e);
	}	
}
