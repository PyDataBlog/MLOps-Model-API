import java.util.*;
public class Pali {

    public static void main(String args[])
    {
        Scanner sc=new Scanner(System.in);
        String str=sc.next();
        StringBuffer buff=new StringBuffer(str).reverse();
        String str1=buff.toString();
        if(str.isequals(str1))
        {
        System.out.println("Palindrome");
        }
        else
        {
         System.out.println("Not a Palindrome");
        }
        
    }
