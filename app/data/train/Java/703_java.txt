package MultiplyStrings;

/**
 * Created by gzhou on 6/1/15.
 */
public class Solution {
    public static void main(String[] args) {

        System.out.println(multiply("123", "20"));
    }

    public static String multiply(String num1, String num2) {
        String n1 = new StringBuilder(num1).reverse().toString();
        String n2 = new StringBuilder(num2).reverse().toString();
        int[] tmp = new int[n1.length() + n2.length()];
        for (int i = 0; i < n1.length(); i++) {
            int a = n1.charAt(i) - '0';
            for (int j = 0; j < n2.length(); j++) {
                int b = n2.charAt(j) - '0';
                tmp[i + j] += b * a;
            }
        }
        StringBuilder sb = new StringBuilder();
        for (int k = 0; k < tmp.length; k++) {
            int d = tmp[k] % 10;
            int carry = tmp[k] / 10;
            // will insert digit from left most
            sb.insert(0, d);
            if (k < tmp.length - 1) {
                tmp[k + 1] += carry;
            }
        }

        // remove zeros which are created by initialization of 'tmp'
        while(sb.length()>0 && sb.charAt(0)=='0'){
            sb.deleteCharAt(0);
        }

        return sb.length()==0? "0" : sb.toString();
    }
}
