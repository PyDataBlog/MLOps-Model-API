package org.squirrel;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.Timer;

import org.squirrel.managers.PrisonerControllor;
import org.squirrel.managers.inputManager;
import org.squirrel.objects.Player;
import org.squirrel.ui.Hud;
import org.squirrel.world.World;

public class Game extends JPanel implements ActionListener{

	private static final long serialVersionUID = -8805039320208612585L;
	public static String name = JOptionPane.showInputDialog(null,"What is your name?","Welcome to Prison Survival", JOptionPane.QUESTION_MESSAGE);
	Timer gameLoop;
	Player player;
	PrisonerControllor prict;
	Hud hud;
	World world1;
	
	public Game(){
		setFocusable(true);
		
		gameLoop = new Timer(10, this);
		gameLoop.start();
		
		player = new Player(300, 300);
		prict = new PrisonerControllor();
		hud = new Hud();
		world1 = new World();
		addKeyListener(new inputManager(player));
	}
	
	public void paint(Graphics g){
		super.paint(g);
		Graphics2D g2d = (Graphics2D) g;

		//Camera
		int offsetMaxX = 1600 - 800;
		int offsetMaxY = 1200 - 600;
		int offsetMinX = 0;
		int offsetMinY = 0;
		
		int camX = player.getxPos() - 800 /2;
		int camY = player.getyPos() - 600 /2;
		
		//if (camX > offsetMaxX){
		//    camX = offsetMaxX;
		//}
		//else if (camX < offsetMinX){
		//    camX = offsetMinX;
		//}		
		//if (camY > offsetMaxY){
		//    camY = offsetMaxY;
		//}
		//else if (camY < offsetMinY){
		//    camY = offsetMinY;
		//}

		g2d.translate(-camX, -camY);
		// Render everything
		world1.draw(g2d);
		hud.draw(g2d);
		prict.draw(g2d);
		player.draw(g2d);
		g.translate(camX, camY);
		
	}
	@Override
	public void actionPerformed(ActionEvent e) {
		try {
			player.update();
			hud.update();
			prict.update();
			world1.update();
			repaint();
		} catch (Exception e1) {
			e1.printStackTrace();
		}

	}

}
