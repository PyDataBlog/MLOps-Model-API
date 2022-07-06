package gof.behaviour.mediator;

public class Colleague1 extends Colleague {

	public Colleague1(Mediator m){
		super(m);
	}
	
	@Override
	public void action() {
		System.out.println("Colleague1");
	}

}
