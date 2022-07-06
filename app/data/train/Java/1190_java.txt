public class  Generic{
	public static void main(String[] args){
		Car <String> car1 = new Car <String> ();
		car1.setName("Buick");
		car1.setPrice("100");
		System.out.printf("name=%s,price=%s\n",car1.getName(),car1.getPrice());

		Car <Integer> car2 = new Car <Integer> ();
		car2.setName(001);
		car2.setPrice(100);
		System.out.printf("name=%d,price=%d\n",car2.getName(),car2.getPrice());

		Integer[] array = {1,2,3,4,5,6,7,8,9};
		car2.print(array);
	}
}

/*generic class*/
class Car <T> {
	private T name;
	private T price;

	public Car(){
		this.name = null;
		this.price = null;
	}

	public Car(T name,T price){
		this.name = name;
		this.price = price;
	}

	public void setName(T name){
		this.name = name;
	}

	public T getName(){
		return this.name;
	}

	public void setPrice(T price){
		this.price = price;
	}

	public T getPrice(){
		return this.price;
	}

	public <A> void print(A[] array){
		for (A var:array)
			System.out.printf("%s",var);
	}
}
