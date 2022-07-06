//Problem 5.	Angle Unit Converter (Degrees ↔ Radians)
//Write a method to convert from degrees to radians. Write a method to convert from radians to degrees.
// You are given a number n and n queries for conversion. Each conversion query will consist of a
// number + space + measure. Measures are "deg" and "rad". Convert all radians to degrees and all
// degrees to radians. Print the results as n lines, each holding a number + space + measure. Format
// all numbers with 6 digit after the decimal point.

import java.util.ArrayList;
import java.util.DoubleSummaryStatistics;
import java.util.List;
import java.util.Scanner;

public class _5_AngleUnitConverterDegreesRadians {
	public static void main(String[] args) {
		Scanner input = new Scanner(System.in);
		System.out.println("Enter how many calculation you want to make");
		int n = Integer.parseInt(input.nextLine());
		List<String> result = new ArrayList<String>();
		for (int i = 0; i < n; i++) {
			String[] cmd = input.nextLine().split(" ");
			String currentResult;
			if (cmd[1].equals("rad")) {
				currentResult = Math.toDegrees(Double.parseDouble(cmd[0]))
						+ " deg";
			} else {
				currentResult = Math.toDegrees(Double.parseDouble(cmd[0]))
						+ " rad";
			}
			result.add(currentResult);
		}

		System.out.println(String.join("\n", result));

	}
}
