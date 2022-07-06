-- phpMyAdmin SQL Dump
-- version 4.0.9
-- http://www.phpmyadmin.net
--
-- Host: 127.0.0.1
-- Generation Time: May 11, 2014 at 07:52 AM
-- Server version: 5.6.14
-- PHP Version: 5.5.6

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `apdb`
--

-- --------------------------------------------------------

--
-- Table structure for table `bm2`
--

CREATE TABLE IF NOT EXISTS `bm2` (
  `date` int(11) NOT NULL,
  `att` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `bm2`
--

INSERT INTO `bm2` (`date`, `att`) VALUES
(1, 90),
(2, 88),
(3, 0),
(4, 88),
(5, 90),
(6, 92),
(7, 88),
(8, 90),
(9, 86),
(10, 0),
(11, 86),
(12, 0),
(13, 85),
(14, 90),
(15, 92),
(16, 88),
(17, 0),
(18, 86),
(19, 92),
(20, 93),
(21, 90),
(22, 90),
(23, 92),
(24, 0),
(25, 93),
(26, 93),
(27, 93),
(28, 93),
(29, 93),
(30, 0),
(31, 0);

-- --------------------------------------------------------

--
-- Table structure for table `fan`
--

CREATE TABLE IF NOT EXISTS `fan` (
  `date` date NOT NULL,
  `ta` int(11) NOT NULL,
  `fb` text NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `school_name`
--

CREATE TABLE IF NOT EXISTS `school_name` (
  `sid` varchar(5) NOT NULL,
  `sname` varchar(30) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `school_name`
--

INSERT INTO `school_name` (`sid`, `sname`) VALUES
('s1', 'GLPS- NELGULI'),
('', ''),
('s2', 'GLPS SEETHARAMAM PALYA '),
('s3', 'RBANMS - HPS - DICKENSON ROAD '),
('s4', 'GHPS - BASAVANNA NAGARA'),
('s5', 'GLPS - SADANA PALYA');

-- --------------------------------------------------------

--
-- Table structure for table `superv`
--

CREATE TABLE IF NOT EXISTS `superv` (
  `date` date NOT NULL,
  `te` int(11) NOT NULL,
  `fb` text NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `superv2`
--

CREATE TABLE IF NOT EXISTS `superv2` (
  `date` int(11) NOT NULL,
  `te` int(11) NOT NULL,
  `fb` text NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `superv2`
--

INSERT INTO `superv2` (`date`, `te`, `fb`) VALUES
(1, 90, ''),
(2, 84, ''),
(3, 0, ''),
(4, 86, ''),
(5, 91, ''),
(6, 92, ''),
(7, 89, ''),
(8, 88, ''),
(9, 87, ''),
(10, 0, ''),
(11, 88, ''),
(12, 0, ''),
(13, 86, ''),
(14, 92, ''),
(15, 92, ''),
(16, 90, ''),
(17, 0, ''),
(18, 88, ''),
(19, 92, ''),
(20, 94, ''),
(21, 90, ''),
(22, 91, ''),
(23, 92, ''),
(24, 0, ''),
(25, 92, ''),
(26, 92, ''),
(27, 92, ''),
(28, 90, ''),
(29, 92, ''),
(30, 0, ''),
(31, 0, '');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
