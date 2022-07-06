
/** A class describing a Snake */
public class Snake extends Animal{
	private boolean poisonous;

	/** Constructor */
	public Snake(String latinName, boolean poisonous) {
		super(latinName);
		this.poisonous = poisonous;
		setFriendlyName("no name");
		
	}
	
	public boolean isPoisonous(){
		return poisonous;	
	}
	
	@Override
	public String getInfo() {
		String poisonousStatus;
		
		if (poisonous){
			poisonousStatus = "poisonous";
		} else{
			poisonousStatus = "not poisonous";
		}
		
		return " the snake named " + getFriendlyName() + " lat " + getLatinName() + " is "+ poisonousStatus;
	}
}
