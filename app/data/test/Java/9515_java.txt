package ejercicio32;

import java.util.Scanner;

/**
 * Ejercicio 32: Hacer un programa que nos permita introducir un numero por
 * teclado y sobre el se realicen las siguientes operaciones: comprobar si es
 * primo, hallar su factorial o imprimir su tabla de multiplicar.
 *
 * @author meny
 * @since 13/05/2015
 * @ver 1.0
 */
public class ejercicio32 {

    public static void main(String[] args) {
        Scanner escaner = new Scanner(System.in);

        //--- Variables
        int a = 0, n = 0;
        int x;
        int Resultado, y = 0;
        double factorial = 0;

        System.out.println("Introduce un numero:");
        n = escaner.nextInt();
        System.out.println("\n");

        System.out.println("Que deseas hacer? \n 1) Comprobar si es Primo \n "
                + "2) Hallar su Factorial \n 3) Imprimir tabla de multiplicar");
        x = escaner.nextInt();
        System.out.println("");

        switch (x) {
            case 1: {
                if (n >= 1) {
                    for (int i = 1; i < (n + 1); i++) {
                        if (n % i == 0) {
                            a++;
                        }
                    }

                    if (a != 2) {
                        System.out.println("No es Primo");
                    } else {
                        System.out.println("Si es Primo");
                    }
                } else {
                    System.out.println("El numero no puede ser procesado");
                }
                break;
            }

            case 2: {
                factorial = n;
                for (int i = n; i > 2; i--) {
                   factorial = factorial * (i - 1);
                }
                System.out.println("El Factorial de "+n+" es: " + factorial);
                break;
            }

            case 3: {
                if (n <= 10) {
                    for (int i = 0; i <= 10; i++) {
                        Resultado = n * y;
                        System.out.println(n + " x " + y + " = " + Resultado);
                        y++;
                    }
                } else {
                    System.out.println("El numero introducido es mayor que 10");
                }
                break;
            }
            default:
                System.out.println("Opcion invalida");
        }

    }
}
