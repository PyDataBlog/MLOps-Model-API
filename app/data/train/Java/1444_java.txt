package dkalymbaev.triangle;

class Triangle {
	/**
	 * This class defines points a b and c as peacks of triangle.
	 */
	public Point a;
	public Point b;
	public Point c;

	/**
	 * Creating of a new objects.
	 * @param a is the length of the first side.
	 * @param b is the length of the second side.
	 * @param c is the length of the third side.
	 */
	public Triangle(Point a, Point b, Point c) {
		this.a = a;
		this.b = b;
		this.c = c;
	}

	public double area() {
		double ab = a.distanceTo(b);
		double bc = b.distanceTo(c);
		double ac = a.distanceTo(c);
		double halfperim = ((ab + bc + ac) / 2);
		double area = Math.sqrt(halfperim * (halfperim - ab) * (halfperim - bc) * (halfperim - ac));
		if (ab > 0 && bc > 0 && ac > 0) {
			return area;
		} else {
			return 0;
		}
	}
}