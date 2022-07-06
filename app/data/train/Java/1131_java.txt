package testtravis;

import static org.junit.Assert.*;

import org.junit.Test;

public class Operar_unit {

	@Test
    public void testSumar() {
        System.out.println("Sumar dos numeros");
        int numero1 = 6;
        int numero2 = 6;
        Operaciones instance = new Operaciones();
        int expResult = 12;
        int result = instance.sumar(numero1, numero2);
        assertEquals(expResult, result);
    
    }


    @Test
    public void testRestar() {
        System.out.println("Restar dos numeros");
        int numero1 = 4;
        int numero2 = 2;
        Operaciones instance = new Operaciones();
        int expResult = 2;
        int result = instance.restar(numero1, numero2);
        assertEquals(expResult, result);
   
    }


    @Test
    public void testMultiplicar() {
        System.out.println("Multiplicar dos numeros");
        int numero1 = 3;
        int numero2 =3;
        Operaciones instance = new Operaciones();
        int expResult = 9;
        int result = instance.multiplicar(numero1, numero2);
        assertEquals(expResult, result);
      
    }

    @Test
    public void testDividir() {
        System.out.println("Dividir Dos numeros");
        int numero1 = 6;
        int numero2 = 3;
        Operaciones instance = new Operaciones();
        int expResult = 2;
        int result = instance.dividir(numero1, numero2);
        assertEquals(expResult, result);

    }
}
