package org.swcraft.javase.collections.map;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.junit.Test;

public class MapImplementationsTest {

	@Test
	public void testHashMap() throws Exception {
		Map<String, Integer> map = new HashMap<>();
		map.put("A", 1);
		map.put("C", 2);
		map.put("B", 5);
		map.put(null, 3);
		
		System.out.println(map);
		System.out.println(map.containsKey(null));
		
		Integer integer = map.get(null);
		System.out.println(integer);
	}
	
	@Test
	public void testLinkedHashMap() throws Exception {
		Map<String, Integer> map = new LinkedHashMap<>();
		map.put("A", 1);
		map.put("C", 2);
		map.put("B", 5);
		map.put(null, 3);
		
		System.out.println(map);
	}
	
	@Test
	public void testTreeMap() throws Exception {
		Map<String, Integer> map = new TreeMap<>();
		map.put("A", 1);
		map.put("C", 2);
		map.put("B", 5);
		// map.put(null, 3);
		
		System.out.println(map);
	}
	
	@Test
	public void testIterator() throws Exception {
		Map<String, Integer> map = new HashMap<>();
		map.put("A", 1);
		map.put("C", 2);
		map.put("B", 5);
		
		// Iterate over keys
		for (String key : map.keySet()) {
			System.out.println(key);
		}
		
		// Iterate over values
		for (Integer value : map.values()) {
			System.out.println(value);
		}
		
		// Iterate over entries
		for (Entry<String, Integer> entry : map.entrySet()) {
			System.out.println("Key: " + entry.getKey());
			System.out.println("Value: " + entry.getValue());
		}
		
		// Iterate with foreach
		map.forEach((key, value) -> {
			System.out.println("Key: " + key);
			System.out.println("Value: " + value);
		});
	}
}
