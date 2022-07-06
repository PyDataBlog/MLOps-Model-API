import java.util.Scanner;

/**
 * Created by mvieck on 12/21/16.
 */
public class CarProject {

    Car[] cars = new Car[3];

    public CarProject() {
        cars[0] = new Car("Ford",80, 7);
        cars[1] = new Car("Kia", 70, 6);
        cars[2] = new Car("Lexus", 60, 8);
    }

    public static void main(String[] args) {

        CarProject carProgram = new CarProject();

        Scanner scanner = new Scanner(System.in);
        System.out.print("Enter in a car brand (Ford, Kia, Lexus): ");
        String brand = scanner.nextLine();

        Car car;
        if (carProgram.cars[0].getBrand().equals(brand)) {
            car = carProgram.cars[0];
        } else if (carProgram.cars[1].getBrand().equals(brand)) {
            car = carProgram.cars[1];
        } else if (carProgram.cars[2].getBrand().equals(brand)) {
            car = carProgram.cars[2];
        } else {
            System.out.println("Could not find the brand. Try Ford, Kia or Lexus");
            return;
        }
        System.out.printf("Brand: %s, Speed: %d mph/kmh, Drive: %dth gear\n",car.getBrand(), car.getSpeed(), car.getDrive());
    }
}
