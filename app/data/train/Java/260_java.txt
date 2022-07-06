package fPPPrograms;

import java.util.Scanner;

public class LeapYear {

	public static void main(String[] args) {
		
		System.out.println("Enter a year to determine whether it is a leap year or not?");
		
		Scanner yourInput = new Scanner(System.in);
		
		int year = yourInput.nextInt();
		
		//String y = year%400 == 0? (year%4 == 0 ) && (year%100 !=0) ? "Yes" : "Not" : "Not" ;
		String y = ((year%4 == 0) && (year%100 != 0) || (year%400 == 0)) ? "Yes" : "Not";
		
		System.out.println("The Year You Entered is " + y + " a Leap Year");
	}
}
