package edu.neu.leetcode.array;

public class BestTimeToBuyAndSellStockII {
    public static int maxProfit(int[] prices) {
        int res = 0, high = prices.length - 1;
        for (int i = prices.length - 1; i > 0; i--) {
            if (prices[i - 1] < prices[i]) {
                if (i - 1 == 0) {
                    res += prices[high] - prices[i - 1];
                    break;
                } else {
                    continue;
                }
            }
            if (high > i) {
                res += prices[high] - prices[i];
            }
            high = i - 1;
        }
        return res;
    }
}
