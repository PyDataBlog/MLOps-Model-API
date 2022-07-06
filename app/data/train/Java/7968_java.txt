/*
 * The MIT License
 *
 * Copyright 2015 Sanket K.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package sortingexamples;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Sanket K
 */
public class SortingExamples {
    
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {    
        SortingExamples obj = new SortingExamples();
        List<Integer> data = new LinkedList<>();
       
        
        // TODO add random numbers in a loop
        data.add(100);
        data.add(34);
        data.add(39);
        data.add(3);
        data.add(3);
        data.add(3);
        //System.out.println("Unsorted list = " + data);
        
        //if (obj.insertionSort(data)) {
        //System.out.println("Sorted list = " + data);
        //}        
        
        int [] data2 = {3,5,6,7,8,9,2,2,1000,4111, 377, 178, 3726, 1, 9, 67754, 56425};
        System.out.println("Unsorted = " + Arrays.toString(data2));
        
        obj.quickSort(0, data2.length - 1, data2);
        
        System.out.println("Sorted = " + Arrays.toString(data2));
        
    }
    
    public SortingExamples() {
        
    }    
    
    private void dumpState(int j, List<Integer> list) {
        for(int i =0; i < list.size(); i++ ) {
            
           if (i == j) {
               System.out.print("(" + list.get(i) + ")");
           }else {
               System.out.print(list.get(i));
           }
           System.out.print(", ");
        }
        try {
            TimeUnit.SECONDS.sleep(2);
        } catch (InterruptedException ex) {
            Logger.getLogger(SortingExamples.class.getName()).log(Level.SEVERE, null, ex);
        }
        System.out.println("");
    }
    
    public boolean insertionSort(List<Integer> mylist) {
        if (null == mylist) {
            System.out.println("Cannot sort a null list");
            return false;
        }
        
        if (mylist.isEmpty()) {
            System.out.println("Cannot sort an empty list");
            return false;
        }
        
        for (int i = 1; i < mylist.size(); i++) {
            int j = i;
            int testvar = mylist.get(i);
            System.out.print("Testing " + testvar + ": ");
            dumpState(j, mylist);
            
            while (j > 0 && mylist.get(j-1) > testvar ) {
                System.out.println(mylist.get(j-1) + " > " + testvar);
                mylist.set(j, mylist.get(j-1));
                //unsorted.set(j-1,0);
                j = j-1;
                
                //dumpState(testvar, data);
            }
            mylist.set(j, testvar);
        }
        return true;
    }
    
    public boolean shellSort(List<Integer> unsorted) {
        return false;
    }
    
    private void dump(int i, int j, int pivot, int[] d) {
        System.out.println("");
        for (int k = 0; k < 40; k++) System.out.print("-");
        System.out.println("");
        int close = 0;
        
        for(int k=0; k < d.length; k++) {
            if (k == i) {
                System.out.print("i(");
                close++;
            }
            if (k == j) {
                System.out.print("j(");
                close++;
            }
            if (d[k] == pivot) {
                System.out.print("p(");
                close++;
            }
            System.out.print(d[k]);
            while(close > 0) {
                System.out.print(")");
                close--;
            }
            System.out.print(", ");
        }
        System.out.println("");
        
    }
    
    public boolean quickSort(int low, int high, int[] d) {
        int i = low, j = high;
        System.out.println("\n\nQuicksort called: low value = " + d[low] + " high value = " + d[high]);
        // Pivots can be selected many ways
        int p = low + (high - low)/2;
        int pivot = d[p];
        
        System.out.println("pivot = " + pivot);
        while (i <= j) {
            dump(i, j, pivot, d);
            System.out.println("\ninc i until d[i]>pivot  & dec j until d[j]<pivot");
            while (d[i] < pivot) i++;
            
            while (d[j] > pivot) j--;
            
            dump(i, j, pivot, d);
            if (i <= j) {
                swap(i,j, d);
                System.out.println("\nSwap i & j");
                dump(i, j, pivot, d);
                i++;
                j--;
            }
        }
        if (low < j) {
            System.out.println("low < j");    
            quickSort(low, j, d);
        }
        if (i < high) {
            System.out.println("i < high");
            quickSort(i, high, d);
        }
        return false;
    }
    
    private void swap(int i, int j, int[] d) {
        int temp = d[i];
        d[i] = d[j];
        d[j] = temp;
    }
}
