-- phpMyAdmin SQL Dump
-- version 3.4.5
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Jul 31, 2012 at 04:47 PM
-- Server version: 5.5.16
-- PHP Version: 5.3.8

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `mappingpattern-adjacencylist`
--

-- --------------------------------------------------------

--
-- Table structure for table `waterareaclassification`
--

CREATE TABLE IF NOT EXISTS `waterareaclassification` (
  `ID` char(5) NOT NULL,
  `CSI_Name` varchar(255) NOT NULL,
  `Parent` char(5) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `waterareaclassification`
--

INSERT INTO `waterareaclassification` (`ID`, `CSI_Name`, `Parent`) VALUES
('20000', 'Water Area', NULL),
('21000', 'Environmental Area', '20000'),
('24020', 'Jurisdiction Area', '20000'),
('22000', 'Fishing Statistical Area', '20000'),
('21001', 'Inland/Marine', '21000'),
('21002', 'Ocean', '21000'),
('21003', 'North/South/Equatorial', '21000'),
('22001', 'FAO Statistical Area', '22000'),
('22002', 'Areal Grid System', '22000');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
