/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package KutzCarRental.WS;

import KutzCarRental.DB.DataBase;
import javax.jws.WebService;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.ejb.Stateless;
import KutzCarRental.Domain.Car;
import KutzCarRental.Domain.Reservation;
import KutzCarRental.Utils.Utils;
import java.text.ParseException;
import java.util.ArrayList;

/**
 *
 * @author Ileana Ontiveros
 */
@WebService(serviceName = "KutzCarRentalWS")
@Stateless()
public class KutzCarRentalWS {
    
    /**
     * Obtener una lista de autos para rentar
     */
    @WebMethod(operationName = "getCars")
    public ArrayList<Car> getCars() {
        ArrayList<Car> cars = DataBase.getDB().getCarsTable();
        return cars;
    }
    
    /**
     * Obtener la lista de reservaciones
     */
    @WebMethod(operationName = "getReservations")
    public ArrayList<Reservation> getReservations() {
        ArrayList<Reservation> reservations = DataBase.getDB().getReservationsTable();
        return reservations;
    }

    /**
     * Reservar un auto
     */
    @WebMethod(operationName = "createReservation")
    public Reservation createReservation(@WebParam(name = "rentalDate") String rentalDate,
        @WebParam(name = "returnDate") String returnDate, 
        @WebParam(name = "idCar") int idCar) throws ParseException {
        
        boolean isBooked = false;
        Reservation reservation = new Reservation();
        try {
            Car car = DataBase.getDB().getCarById(idCar);
            double pricePerDay = car.getPricePerDay();
            int totalDays = new Utils().calculateDays(rentalDate, returnDate);
            double priceTotal = totalDays*pricePerDay;
            reservation = new Reservation(rentalDate, returnDate, car, priceTotal);
            isBooked = DataBase.getDB().addReservation(reservation, idCar);
            return reservation;
        } catch (Exception e){
            return reservation;
        }
        
    }

    @WebMethod(operationName = "getReservation")
    public Reservation getReservation(String reservationId) throws ParseException {
        Reservation reservation = DataBase.getDB().getReservationById(reservationId);
        return reservation;
    }
    
    /**
     * Obtener un auto por id
     */
    @WebMethod(operationName = "findCarById")
    public Car findCarById(@WebParam(name = "idCar") int idCar) {
        Car car = new Car();
        try {
            car = DataBase.getDB().getCarById(idCar);
            return car;
        } catch (Exception e){
            return car;
        } 
    }
        
}
