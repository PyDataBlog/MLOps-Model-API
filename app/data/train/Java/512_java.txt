/**
 * 
 */
package com.coin.arbitrage.huobi.util;

/**
 * @author Frank
 *
 */
public enum DepthType {
	
	STEP0("step0"), 
	STEP1("step1"), 
	STEP2("step2"),
	STEP3("step3"),
	STEP4("step4"),
	STEP5("step5");
	
	private String depth;

	private DepthType(String depth) {
		this.depth = depth;
	}
	
	public String getDepth() {
		return depth;
	}
}
