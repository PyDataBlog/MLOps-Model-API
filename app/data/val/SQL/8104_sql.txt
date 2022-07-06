-- phpMyAdmin SQL Dump
-- version 4.6.5.2
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1
-- Generation Time: Nov 08, 2017 at 06:41 AM
-- Server version: 10.1.21-MariaDB
-- PHP Version: 5.6.30

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `rvmax`
--

-- --------------------------------------------------------

--
-- Table structure for table `contact`
--

CREATE TABLE `contact` (
  `id` int(255) NOT NULL,
  `firstname` varchar(255) NOT NULL,
  `lastname` varchar(255) NOT NULL,
  `phone` varchar(255) NOT NULL,
  `email` varchar(255) NOT NULL,
  `aboutus` varchar(100) NOT NULL,
  `message` text NOT NULL,
  `created_date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00'
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `contact`
--

INSERT INTO `contact` (`id`, `firstname`, `lastname`, `phone`, `email`, `aboutus`, `message`, `created_date`, `updated_date`) VALUES
(2, 'rajkumar', 'IOJHOI', '1345678945', 'jhfguygthj@gmail.com', 'Other', 'hi djkghkjsdgfkjs', '2017-10-13 03:51:03', '2017-10-13 04:27:44'),
(3, 'sureskumar', 'chinnasamy', '12345674894', 'dfgfdgdfg@gmail.com', 'Mailer', 'sdfgsdhgjgwf', '2017-10-13 03:52:26', '2017-10-13 04:27:23'),
(4, 'mahendran', 'chinnasamy', '12345674894', 'fdgdfgdfgdfg@gmail.com', 'Internet Search', 'sdfggsgsdfg', '2017-10-13 03:52:36', '2017-10-13 04:27:02'),
(5, 'cvncgfghdf', 'b', '1236547896', 'mahesakthi15@gmail.com', 'Internet Search', 'dfgedgdfgdfg', '2017-11-02 02:15:20', '0000-00-00 00:00:00'),
(6, 'mahe', 'wrer', '54546456', 'mahe@gmail.com', 'Newspaper / Magazine Ad', 'dfgedgdfg', '2017-11-02 02:16:15', '0000-00-00 00:00:00'),
(7, 'mahendran', 'c', '1236547896', 'mahe@gmail.com', 'Mailer', 'ya it s good ', '2017-11-02 02:17:24', '0000-00-00 00:00:00'),
(8, 'et5eterert', 'erterter', '6546454', 'mahe@gmail.com', 'Mailer', 'fghrtghgfh', '2017-11-02 02:37:43', '0000-00-00 00:00:00'),
(9, 'mahendran', 'c', '7896452121', 'mahe@gmail.com', 'Newspaper / Magazine Ad', 'sdrfgregergerg', '2017-11-08 00:02:27', '0000-00-00 00:00:00');

--
-- Indexes for dumped tables
--

--
-- Indexes for table `contact`
--
ALTER TABLE `contact`
  ADD PRIMARY KEY (`id`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `contact`
--
ALTER TABLE `contact`
  MODIFY `id` int(255) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=10;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
