public class Eagle implements Animal {

	/**
	 * Constructor with package access only
	 * Only classes in the same package can instantiate it, like the AnimalFactory
	 */
	Eagle(){}
	
	@Override
	public void move() {
		System.out.println("Eagle flying...");
	}
}
