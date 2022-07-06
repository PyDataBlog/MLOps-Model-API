
import java.util.HashMap;
import java.util.Map;

public class LeetCode171 {

	public int titleToNumber(String s) {
        char[] chars = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'};
		Map<Character, Integer> map = new HashMap<Character, Integer>();
		
		for (int i = 1; i <= chars.length; i++) {
			map.put(chars[i-1], i);
		}
	
		int result = 0;
		for (int i = s.length() -1; i >= 0; i--) {
			
			int num1 = map.get(s.charAt(i));
			int pow = (int) Math.pow(26, s.length() -1 -i);
			
			result += num1 * pow;
		}
		
		return result;
    }
	
	public static void main(String[] args) {
		LeetCode171 leetCode171 = new LeetCode171();
		
		System.out.println(leetCode171.titleToNumber("ZY"));
	}
}
