package Aula_4.Carro;

import java.awt.Graphics2D;

public class RodaAros extends Roda {

	int n;

	public RodaAros(int n, int raio, int xi, int yi) {
		super(raio, xi, yi, 0);
		this.n = n;
	}
	
	/*public RodaAros(int xi, int yi) {
		super(17, xi, yi, 0);
		n = 8;
		raio = 17;
	}*/

	public void draw(Graphics2D g2d) {
		double ang_inc = 2 * Math.PI / n;
		for (int i = 0; i < n; i++) {
			g2d.drawLine(xi, yi, (int) (Math.cos(ang_inicial) * raio + xi), (int) (Math.sin(ang_inicial) * raio + yi));
			ang_inicial += ang_inc;
		}
		
		g2d.drawOval(xi - raio, yi - raio, raio * 2, raio * 2);
	}

	public void avanca(int npixel) {
		xi -= npixel;
		ang_inicial -= Math.toRadians(10);
	}

	public void recua(int npixel) {
		xi += npixel;
		ang_inicial += Math.toRadians(10);
	}
	
}