import java.util.*;
public class ejemplo2 {
	public static void main(String[] args)
	{
		Scanner reader = new Scanner(System.in);
		int n1;
		int n2;

		int suma;

		System.out.print("Ingrese el 1er numero: ");
		n1 = reader.nextInt();

		System.out.print("Ingrese el 2do numero: ");
		n2 = reader.nextInt();

		suma = n1 + n2;
		System.out.println("suma de " + n1 + " y " + n2 + " es " + suma);
		System.exit(0);
	}
}