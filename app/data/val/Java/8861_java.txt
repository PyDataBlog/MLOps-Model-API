/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package aula01;

import java.util.Scanner;
/**
 *
 * @author Aluno
 */
public class Exemplo1 {
    
    public static void main(String[] args)
    {
        /* Objeto Scan vai auxiliar na entrada de dados via teclado */
        Scanner scan = new Scanner(System.in);
        System.out.println("Qual o seu nome ?");
        
        /* Metodo nextLine lê uma string do teclado */
        String nome = scan.nextLine();
        System.out.println("Seja Bem-Vindo" + nome);
        System.out.println("Qual a sua idade ?");
        
        /* Metodo nextInt lê um inteiro do teclado */
        int idade = scan.nextInt();
        System.out.println("Sua idade eh " + idade + " anos.");
    }
}
