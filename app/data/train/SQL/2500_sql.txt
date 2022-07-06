-- phpMyAdmin SQL Dump
-- version 4.6.5.2
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1
-- Generation Time: Oct 21, 2017 at 02:52 PM
-- Server version: 10.1.21-MariaDB
-- PHP Version: 5.6.30

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `restaurant`
--

-- --------------------------------------------------------

--
-- Table structure for table `address`
--

CREATE TABLE `address` (
  `id` int(11) NOT NULL,
  `description` varchar(100) DEFAULT NULL,
  `staff_staff_id` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `customer`
--

CREATE TABLE `customer` (
  `idcustomer` int(11) NOT NULL,
  `first_name` varchar(100) DEFAULT NULL,
  `last_name` varchar(100) DEFAULT NULL,
  `date_birth` date DEFAULT NULL,
  `phone` int(11) DEFAULT NULL,
  `email` varchar(100) DEFAULT NULL,
  `is_active` int(11) DEFAULT NULL,
  `licencetype_id` int(11) NOT NULL,
  `address_id` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `invoice_occupancy`
--

CREATE TABLE `invoice_occupancy` (
  `invoice_occu_id` int(11) NOT NULL,
  `service_tax` int(11) DEFAULT NULL,
  `invoice_amount` int(11) DEFAULT NULL,
  `created_by` varchar(100) DEFAULT NULL,
  `created_date` date DEFAULT NULL,
  `invoice_status_invoice_id` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `invoice_status`
--

CREATE TABLE `invoice_status` (
  `invoice_id` int(11) NOT NULL,
  `invoice_description` varchar(100) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `lesson`
--

CREATE TABLE `lesson` (
  `lesson_id` int(11) NOT NULL,
  `lesson_title` varchar(45) DEFAULT NULL,
  `lesson_date` date DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `licencetype`
--

CREATE TABLE `licencetype` (
  `licence_id` int(11) NOT NULL,
  `vehicle_classes` varchar(45) DEFAULT NULL,
  `full_payment` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `request`
--

CREATE TABLE `request` (
  `id` int(11) NOT NULL,
  `service_id` int(11) DEFAULT NULL,
  `status_id` int(11) DEFAULT NULL,
  `created_date` date DEFAULT NULL,
  `created_by` varchar(100) DEFAULT NULL,
  `last_modified_date` date DEFAULT NULL,
  `modified_by` varchar(100) DEFAULT NULL,
  `customer_id` int(11) NOT NULL,
  `request_status_id` int(11) NOT NULL,
  `vehicle_type_id` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `request_status`
--

CREATE TABLE `request_status` (
  `request_id` int(11) NOT NULL,
  `request_desc` varchar(100) DEFAULT NULL,
  `is_active` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `reservation`
--

CREATE TABLE `reservation` (
  `reservation_id` int(11) NOT NULL,
  `created_date` date DEFAULT NULL,
  `created_by` varchar(100) DEFAULT NULL,
  `last_modified_date` date DEFAULT NULL,
  `last_modified_by` varchar(100) DEFAULT NULL,
  `vehicle_vehicle_no` int(11) NOT NULL,
  `request_id` int(11) NOT NULL,
  `invoice_occu_id` int(11) NOT NULL,
  `service_lesson_service_lesson_id` int(11) NOT NULL,
  `staff_staff_id` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `reservation_status`
--

CREATE TABLE `reservation_status` (
  `reservation_status_id` int(11) NOT NULL,
  `resrervation_status_description` varchar(100) DEFAULT NULL,
  `reservation_id` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `staff`
--

CREATE TABLE `staff` (
  `staff_id` int(11) NOT NULL,
  `first_name` varchar(100) DEFAULT NULL,
  `last_name` varchar(100) DEFAULT NULL,
  `email` varchar(45) DEFAULT NULL,
  `mobile` int(11) DEFAULT NULL,
  `is_active` int(11) DEFAULT NULL,
  `lesson_lesson_id` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `vehicle`
--

CREATE TABLE `vehicle` (
  `vehicle_no` int(11) NOT NULL,
  `vehicle_model` varchar(45) DEFAULT NULL,
  `registrated_year` varchar(45) DEFAULT NULL,
  `vehicle_type_vehicle_type_id` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `vehicle_type`
--

CREATE TABLE `vehicle_type` (
  `vehicle_type_id` int(11) NOT NULL,
  `vehicle_type_desc` varchar(100) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Indexes for dumped tables
--

--
-- Indexes for table `address`
--
ALTER TABLE `address`
  ADD PRIMARY KEY (`id`,`staff_staff_id`),
  ADD KEY `fk_address_staff1_idx` (`staff_staff_id`);

--
-- Indexes for table `customer`
--
ALTER TABLE `customer`
  ADD PRIMARY KEY (`idcustomer`,`licencetype_id`,`address_id`),
  ADD KEY `fk_customer_licencetype1_idx` (`licencetype_id`),
  ADD KEY `fk_customer_address1_idx` (`address_id`);

--
-- Indexes for table `invoice_occupancy`
--
ALTER TABLE `invoice_occupancy`
  ADD PRIMARY KEY (`invoice_occu_id`,`invoice_status_invoice_id`),
  ADD KEY `fk_invoice_occupancy_invoice_status1_idx` (`invoice_status_invoice_id`);

--
-- Indexes for table `invoice_status`
--
ALTER TABLE `invoice_status`
  ADD PRIMARY KEY (`invoice_id`);

--
-- Indexes for table `lesson`
--
ALTER TABLE `lesson`
  ADD PRIMARY KEY (`lesson_id`);

--
-- Indexes for table `licencetype`
--
ALTER TABLE `licencetype`
  ADD PRIMARY KEY (`licence_id`);

--
-- Indexes for table `request`
--
ALTER TABLE `request`
  ADD PRIMARY KEY (`id`,`customer_id`,`request_status_id`,`vehicle_type_id`),
  ADD KEY `fk_request_customer1_idx` (`customer_id`),
  ADD KEY `fk_request_request_status1_idx` (`request_status_id`),
  ADD KEY `fk_request_vehicle_type1_idx` (`vehicle_type_id`);

--
-- Indexes for table `request_status`
--
ALTER TABLE `request_status`
  ADD PRIMARY KEY (`request_id`);

--
-- Indexes for table `reservation`
--
ALTER TABLE `reservation`
  ADD PRIMARY KEY (`reservation_id`,`vehicle_vehicle_no`,`request_id`,`invoice_occu_id`,`service_lesson_service_lesson_id`,`staff_staff_id`),
  ADD KEY `fk_reservation_vehicle1_idx` (`vehicle_vehicle_no`),
  ADD KEY `fk_reservation_request1_idx` (`request_id`),
  ADD KEY `fk_reservation_invoice_occupancy1_idx` (`invoice_occu_id`),
  ADD KEY `fk_reservation_service_lesson1_idx` (`service_lesson_service_lesson_id`),
  ADD KEY `fk_reservation_staff1_idx` (`staff_staff_id`);

--
-- Indexes for table `reservation_status`
--
ALTER TABLE `reservation_status`
  ADD PRIMARY KEY (`reservation_status_id`,`reservation_id`),
  ADD KEY `fk_reservation_status_reservation1_idx` (`reservation_id`);

--
-- Indexes for table `staff`
--
ALTER TABLE `staff`
  ADD PRIMARY KEY (`staff_id`,`lesson_lesson_id`),
  ADD KEY `fk_staff_lesson1_idx` (`lesson_lesson_id`);

--
-- Indexes for table `vehicle`
--
ALTER TABLE `vehicle`
  ADD PRIMARY KEY (`vehicle_no`,`vehicle_type_vehicle_type_id`),
  ADD KEY `fk_vehicle_vehicle_type1_idx` (`vehicle_type_vehicle_type_id`);

--
-- Indexes for table `vehicle_type`
--
ALTER TABLE `vehicle_type`
  ADD PRIMARY KEY (`vehicle_type_id`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `address`
--
ALTER TABLE `address`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `customer`
--
ALTER TABLE `customer`
  MODIFY `idcustomer` int(11) NOT NULL AUTO_INCREMENT;
--
-- Constraints for dumped tables
--

--
-- Constraints for table `address`
--
ALTER TABLE `address`
  ADD CONSTRAINT `fk_address_staff1` FOREIGN KEY (`staff_staff_id`) REFERENCES `staff` (`staff_id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `customer`
--
ALTER TABLE `customer`
  ADD CONSTRAINT `fk_customer_address1` FOREIGN KEY (`address_id`) REFERENCES `address` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `fk_customer_licencetype1` FOREIGN KEY (`licencetype_id`) REFERENCES `licencetype` (`licence_id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `invoice_occupancy`
--
ALTER TABLE `invoice_occupancy`
  ADD CONSTRAINT `fk_invoice_occupancy_invoice_status1` FOREIGN KEY (`invoice_status_invoice_id`) REFERENCES `invoice_status` (`invoice_id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `request`
--
ALTER TABLE `request`
  ADD CONSTRAINT `fk_request_customer1` FOREIGN KEY (`customer_id`) REFERENCES `customer` (`idcustomer`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `fk_request_request_status1` FOREIGN KEY (`request_status_id`) REFERENCES `request_status` (`request_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `fk_request_vehicle_type1` FOREIGN KEY (`vehicle_type_id`) REFERENCES `vehicle_type` (`vehicle_type_id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `reservation`
--
ALTER TABLE `reservation`
  ADD CONSTRAINT `fk_reservation_invoice_occupancy1` FOREIGN KEY (`invoice_occu_id`) REFERENCES `invoice_occupancy` (`invoice_occu_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `fk_reservation_request1` FOREIGN KEY (`request_id`) REFERENCES `request` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `fk_reservation_service_lesson1` FOREIGN KEY (`service_lesson_service_lesson_id`) REFERENCES `lesson` (`lesson_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `fk_reservation_staff1` FOREIGN KEY (`staff_staff_id`) REFERENCES `staff` (`staff_id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  ADD CONSTRAINT `fk_reservation_vehicle1` FOREIGN KEY (`vehicle_vehicle_no`) REFERENCES `vehicle` (`vehicle_no`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `reservation_status`
--
ALTER TABLE `reservation_status`
  ADD CONSTRAINT `fk_reservation_status_reservation1` FOREIGN KEY (`reservation_id`) REFERENCES `reservation` (`reservation_id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `staff`
--
ALTER TABLE `staff`
  ADD CONSTRAINT `fk_staff_lesson1` FOREIGN KEY (`lesson_lesson_id`) REFERENCES `lesson` (`lesson_id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Constraints for table `vehicle`
--
ALTER TABLE `vehicle`
  ADD CONSTRAINT `fk_vehicle_vehicle_type1` FOREIGN KEY (`vehicle_type_vehicle_type_id`) REFERENCES `vehicle_type` (`vehicle_type_id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
