package algorithms.sorting;

import java.util.Scanner;

/*
 * Sample Challenge
 * This is a simple challenge to get things started. Given a sorted array (ar)
 * and a number (V), can you print the index location of V in the array?
 * 
 * Input Format:
 * There will be three lines of input:
 * 
 * V - the value that has to be searched.
 * n - the size of the array.
 * ar - n numbers that make up the array.
 * 
 * Output Format:
 * Output the index of V in the array.
 * 
 * Constraints:
 * 1 <= n <= 1000
 * -1000 <= V <= 1000, V is an element of ar
 * It is guaranteed that V will occur in ar exactly once.
 * 
 * Sample Input:
 * 4
 * 6
 * 1 4 5 7 9 12
 * 
 * Sample Output:
 * 1
 */
public class FindIndexIntro {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        
        int target = sc.nextInt();
        int arraySize = sc.nextInt();
        
        int targetIndex = -1;
        int arrayIndex = 0;
        
        while (arrayIndex < arraySize) {
            if (target == sc.nextInt()) {
                targetIndex = arrayIndex;
                break;
            }
            arrayIndex++;
        }
        sc.close();
        
        System.out.println(targetIndex);
    }
}