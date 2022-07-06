/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mx.neftaly.hackerrank.algorithms;

import java.util.Scanner;

/**
 *
 * @author samas
 */
public class Kangaroo {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        try (Scanner in = new Scanner(System.in)) {
            int x1 = in.nextInt();
            int v1 = in.nextInt();
            int x2 = in.nextInt();
            int v2 = in.nextInt();

            if (x2 >= x1 && v2 >= v1) {
                System.out.println("NO");
            } else if ((x1 - x2) % (v2 - v1) == 0) {
                System.out.println("YES");
            } else {
                System.out.println("NO");
            }
        }
    }

}
