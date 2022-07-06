class Solution {
public:
        string addBinary(string a, string b) {
                string sum = "";
                int carry = 0;
                for (int i = a.size() - 1, j = b.size() - 1; i >= 0 || j >= 0; i--, j--) {
                        int m = (i >= 0 && a[i] == '1');
                        int n = (j >= 0 && b[j] == '1');
                        sum += to_string((m + n + carry) & 0x1); // &0x1 only get the last binary digit
                                                                                           //It's better to avoid pattern string = char + string in loop. Use s[i] to alter string.
                                                                                           //just directly writing the output string and reversing it at last.
                        carry = (m + n + carry) >> 1; // >>1 is /2
                }
                reverse(sum.begin(), sum.end());
                if(carry)  sum = '1' + sum;
                else sum = '0' + sum; //in case of two null input string 
                size_t i=0;
                while(sum[i] == '0' && i < sum.length()-1) i++; 
                sum = sum.substr(i);
                return sum;
        }
};