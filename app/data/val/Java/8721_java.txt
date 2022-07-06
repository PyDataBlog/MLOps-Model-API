package org.defascat.presentation.basic;

import java.util.Arrays;
import java.util.List;
/*
javap -c ../demo/target/classes/org/defascat/presentation/unsorted/IntCache.class 
*/
public class IntCache {
    public static void main(String[] args) {
        List<Integer> a = Arrays.asList(22);
        System.out.println(a);
    }
}
