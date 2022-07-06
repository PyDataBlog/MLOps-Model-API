/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package threads;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 *
 * @author mars
 */
public class Bounce {

    public static void main(String[] args) {
        JFrame frame = new BounceFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }
}

/**
 * A Ball that moves and bounces off the edges of a rectangle
 *
 */
class Ball {

    private static final int XSIZE = 15;
    private static final int YSIZE = 15;
    private double x = 0;
    private double y = 0;
    private double dx = 1;
    private double dy = 1;

    /**
     * Moves the ball to the next position,reversing direction if it hits one of
     * the edges
     */
    public void move(Rectangle2D bounds) {
        x += dx;
        y += dy;
        if (x < bounds.getMinX()) {
            x = bounds.getMinX();
            dx = -dx;
        }
        if (x + XSIZE >= bounds.getMaxX()) {
            x = bounds.getMaxX() - XSIZE;
            dx = -dx;
        }
        if (y < bounds.getMinY()) {
            y = bounds.getMinY();
            dy = -dy;
        }
        if (y + YSIZE >= bounds.getMaxY()) {
            y = bounds.getMaxY() - YSIZE;
            dy = -dy;
        }
    }

    /**
     * Gets the shape of the ball at its current position
     */
    public Ellipse2D getShape() {
        return new Ellipse2D.Double(x, y, XSIZE, YSIZE);
    }
}

/**
 * The panel that draws the balls.
 */
class BallPanel extends JPanel {

    private ArrayList<Ball> balls = new ArrayList<>();

    /**
     * Add a ball to the panel
     *
     * @param b the ball to add
     */
    public void add(Ball b) {
        balls.add(b);
    }

    @Override
    public void paintComponents(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g;
        for (Ball b : balls) {
            g2.fill(b.getShape());
        }
    }
}

/**
 * The frame with the panel and buttons
 *
 */
class BounceFrame extends JFrame {

    private BallPanel panel;
    private static final int DEFAULT_WIDTH = 450;
    private static final int DEFAULT_HEIGHT = 350;
    private final int STEPS = 1000;
    private final int DELAY = 3;

    /**
     * Constructs the frame with the panel for showing the bouncing ball and
     * start and close buttons
     */
    public BounceFrame() {
        setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);
        setTitle("Bounce");

        panel = new BallPanel();
        add(panel, BorderLayout.CENTER);
        JPanel buttonPanel = new JPanel();
        addButton(buttonPanel, "Start", new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //To change body of generated methods, choose Tools | Templates.
                addBall();
            }
        });
        addButton(buttonPanel, "Close", new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //To change body of generated methods, choose Tools | Templates.
                System.exit(0);
            }
        });
        add(buttonPanel, BorderLayout.SOUTH);
    }

    /**
     * Adds a button to a container
     *
     * @param c the container
     * @param title the button title
     * @param listener the action listener for the button
     */
    public void addButton(Container c, String title, ActionListener listener) {
        JButton button = new JButton(title);
        c.add(button);
        button.addActionListener(listener);
    }

    /**
     * Adds a bouncing ball to the panel and makes it bounce 1,000 times
     */
    public void addBall() {
        try {
            Ball ball = new Ball();
            panel.add(ball);

            for (int i = 1; i <= STEPS; i++) {
                ball.move(panel.getBounds());
                panel.paint(panel.getGraphics());
                Thread.sleep(DELAY);
            }
        } catch (InterruptedException e) {

        }
    }
}
