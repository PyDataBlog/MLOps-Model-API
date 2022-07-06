package sergio;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Calendar;
import java.util.GregorianCalendar;

//Ejercicio Metodos 18
//Realiza una clase llamada milibreria que contenga al menos cinco de los metodos realizados.
//Usalas de 3 formas diferentes
//Autor: Sergio Tobal
//Fecha: 12-02-2012
public class EjerMetods18 {

	/**
	 * @param args
	 * @throws IOException 
	 * @throws NumberFormatException 
	 */
	public static void main(String[] args) throws NumberFormatException, IOException {
		BufferedReader lectura = new BufferedReader(new InputStreamReader(System.in));
		int numsend=10,edad;
		char nombre;
		boolean nombreceive;
		String msgname = null;
		
		System.out.println("Dame tu inicial:");
		nombre=lectura.readLine().charAt(0);
		
		nombreceive=EsMayus(nombre);
		if (nombreceive==true) {
			msgname="MAYUSCULAS";
		} else if (nombreceive==false) {
			msgname="minusculas";
		}
		EsPerfecto(numsend);
		System.out.println("Tu primer numero perfecto es "+numsend+" porque tienes "+(edad=ObtenerEdad())+" años, y tu inicial esta escrita en "+msgname);
		
	}
	
	private static boolean EsPerfecto(int numsend) {
		int perfect=0;
		for (int i = 1; i < numsend; i++) {
            if (numsend % i == 0) {
                    perfect += i;
            }
		}
		
		if (perfect == numsend) {
            return true;
		}else {
            return false;
		}
	}
	
	private static int ObtenerEdad()throws NumberFormatException, IOException{
		BufferedReader lectura = new BufferedReader(new InputStreamReader(System.in));
		int year,month,day;
		
		System.out.println("Dime tu dia de nacimiento: ");
		day=Integer.parseInt(lectura.readLine());
		System.out.println("Dime tu mes de nacimiento: ");
		month=Integer.parseInt(lectura.readLine());
		System.out.println("Dime tu año de nacimiento: ");
		year=Integer.parseInt(lectura.readLine());
		
		Calendar cal = new GregorianCalendar(year, month, day);
	    Calendar now = new GregorianCalendar();
	    int edad = now.get(Calendar.YEAR) - cal.get(Calendar.YEAR);
	    if ((cal.get(Calendar.MONTH) > now.get(Calendar.MONTH)) || (cal.get(Calendar.MONTH) == now.get(Calendar.MONTH) && cal.get(Calendar.DAY_OF_MONTH) > now.get(Calendar.DAY_OF_MONTH)))
	    {
	      edad--;
	    }
	    return edad;
	}
	
	private static int Factorial(int num) {
		int factorial=1;
		
//		Va multiplicando el numero del usuario por 1 hasta que el numero llega ha cero y retorna
//		la multiplicacion de todos los numeros
		while (num!=0) {
			factorial=factorial*num;
			num--;
		}
		return factorial;
	}
	
	private static boolean ValFecha(int day, int month) {
		
		
		if ((day>=1 && day<=31) && (month>=1 && month<=12)) {
			return true;
		}else {
			return false;
		}
	}
	private static boolean EsMayus(char nombre) {
		boolean opt=true;
		
//		La funcion isUpperCase comprueba que el contenido de num sea mayuscula
		if (Character.isUpperCase(nombre) == true) {
			opt=true;
//		La funcion isLowerCase comprueba que el contenido de num sea minuscula
		} else if(Character.isLowerCase(nombre) == true){
			opt=false;
		}

		return opt;
	}
}
