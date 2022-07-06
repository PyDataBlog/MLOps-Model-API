package com.jiangjg.lib.EffectiveJava;

class Insect{
	private int i=9;
	protected int j;
	private int xx=printInit("xx");
	public Insect() {
		System.out.println("i=" + i + ", j=" +j);
		j=39;
	}
	
	private static int x1=printInit("static I1");
	
	static int printInit(String s){
		System.out.println(s);
		return 47;
	}
}

public class Beetle extends Insect {
	
	public Beetle(){
		System.out.println("k=" +k);
		System.out.println("j="+j);
	}
	private int k = printInit("B.k");
	private static int x2=printInit("static 2");
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		System.out.println("bb");
		Beetle beetle = new Beetle();
	}

}
