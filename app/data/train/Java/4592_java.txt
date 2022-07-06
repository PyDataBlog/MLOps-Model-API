package com.drogon.test;

import java.io.IOException;
import java.util.Iterator;
import com.drogon.core.Drogon;

/**
Contributors: Nachi
 */
public class TestParser {

	public static void main(String[] args) {
		String str = "Mozilla/5.0 (Linux; Android 4.4.2; SGH-T399N Build/KOT49H) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/30.0.0.0 Mobile Safari/537.36";
		try {
			long start = System.currentTimeMillis();
			for(int i=0;i<1;i++){
				Drogon parser = new Drogon(); 
				Iterator<String> iterator = parser.getProductList(str).iterator();
				while(iterator.hasNext()){
					System.out.println(iterator.next());
				}
			}
			System.out.println("Time taken = "+(System.currentTimeMillis() - start)+"ms");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
