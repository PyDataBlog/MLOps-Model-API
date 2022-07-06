/*****************************************************************************
 * Course: CS 27500
 * Name: Adam Joseph Cook
 * Email: cook5@purduecal.edu
 * Assignment: 1
 *
 * The <CODE>TestDrive</CODE> Java application simulates a car being driven
 * depending on its provided fuel efficiency and fuel amount.
 *
 * @author Adam Joseph Cook
 *     <A HREF="mailto:cook5@purduecal.edu"> (cook5@purduecal.edu) </A>
 *****************************************************************************/

import java.util.Scanner;

public class TestDrive
{
    public static void main( String[] args ) {

        Scanner sc = new Scanner(System.in);
        Car car;

        boolean mainDone = false;
        while (!mainDone) {
            System.out.print("Enter fuel efficiency: ");
            double fuelEfficiency = sc.nextDouble();
            car = new Car(fuelEfficiency);

            if (fuelEfficiency == 0) {
                // The user entered a fuel efficiency of zero, so terminate the
                // program.
                System.exit(0);
            }

            boolean outerDone = false;
            while (!outerDone) {
                System.out.print("Enter amount of fuel: ");
                double fuelAmountToAdd = sc.nextDouble();

                if (fuelAmountToAdd == 0) {
                    // The user entered a zero value for the fuel to add, so
                    // terminate this outer loop.
                    outerDone = true;
                } else {
                    // Add provided fuel to the car.
                    car.addFuel(fuelAmountToAdd);

                    boolean innerDone = false;
                    while (!innerDone) {
                        System.out.print("Enter distance to travel: ");
                        double distanceToTravel = sc.nextDouble();

                        if (distanceToTravel == 0) {
                            // The user entered a zero distance to travel, so
                            // terminate this inner loop.
                            innerDone = true;
                        } else {
                            // Attempt to travel the distance provided with the
                            // car.
                            double distanceTraveled = car.drive(distanceToTravel);
                            System.out.println("   Distance actually " +
                                              "traveled = " + distanceTraveled);
                            System.out.println("   Current fuelLevel = " +
                                    car.getFuelLevel());
                            System.out.println("   Current odometer = " +
                                    car.getOdometer());
                        }
                    }
                }
            }
        }
    }
}