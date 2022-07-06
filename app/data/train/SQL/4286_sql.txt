-- phpMyAdmin SQL Dump
-- version 3.4.5
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Nov 08, 2016 at 07:57 AM
-- Server version: 5.5.16
-- PHP Version: 5.3.8

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `db_tausiyah`
--

-- --------------------------------------------------------

--
-- Table structure for table `admin`
--

CREATE TABLE IF NOT EXISTS `admin` (
  `username` varchar(128) NOT NULL,
  `password` varchar(64) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `admin`
--

INSERT INTO `admin` (`username`, `password`) VALUES
('admin', '21232f297a57a5a743894a0e4a801fc3');

-- --------------------------------------------------------

--
-- Table structure for table `ebook`
--

CREATE TABLE IF NOT EXISTS `ebook` (
  `id_ebook` int(11) NOT NULL AUTO_INCREMENT,
  `judul` text NOT NULL,
  `deskripsi` text NOT NULL,
  PRIMARY KEY (`id_ebook`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `jadwal`
--

CREATE TABLE IF NOT EXISTS `jadwal` (
  `id_jadwal` int(11) NOT NULL AUTO_INCREMENT,
  `tanggal` date NOT NULL,
  `judul` text NOT NULL,
  `id_ustadz` int(11) NOT NULL,
  PRIMARY KEY (`id_jadwal`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `materi`
--

CREATE TABLE IF NOT EXISTS `materi` (
  `id_materi` int(11) NOT NULL AUTO_INCREMENT,
  `judul` varchar(128) NOT NULL,
  `id_ustadz` varchar(128) NOT NULL,
  `tanggal` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `isi` text NOT NULL,
  `gambar` text NOT NULL,
  PRIMARY KEY (`id_materi`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=6 ;

--
-- Dumping data for table `materi`
--

INSERT INTO `materi` (`id_materi`, `judul`, `id_ustadz`, `tanggal`, `isi`, `gambar`) VALUES
(1, 'Berbakti Kepada Ortu yang telah Wafat', 'Dr. Aam Amiruddin', '2016-01-11 02:05:18', 'Ustadz , saya merasa selama ini belum berbakti pada kedua orang tua. Padahal mereka sudah meninggal dunia. Bagimana caranya agar saya bisa berbakti kepada mereka? Apa saja amalan yang dapat dilakukan agar saya dapat membalas kebaikan mereka? JAWAB : Diakui, berbakti kepada orang tua merupakan salah satu amal yang paling dicintai Allah. Telah berkata Abdullah bin Mas’ud r.a. Aku berta.....', ''),
(2, 'Tak Tahu Arah Kiblat Ketika Shalat', 'Dr. Aam Amiruddin', '2016-01-01 17:00:00', 'Ketika melakukan perjalanan ke sejumlah negara Eropa, saya menghadapi kesulitan dalam menentukan arah kiblat. Ke arah mana saya harus menghadap dalam keadaan seperti itu? Kalau ternyata arah kiblatnya salah, apakah shalatnya perlu diulangi? Nana, Cimahi, alina….@gmail.com JAWAB : Kiblat adalah titik yang menyatukan arah segenap umat Islam dalam melaksanakan shalat, tetapi titik arah itu sendiri..', ''),
(3, 'Hukum Memberi Ucapan Natal', 'Dr. Aam Amiruddin', '2016-01-01 17:00:00', 'Pak ustadz yang saya hormati, bagaimana hukumnya apabila kita mengucapkan selamat hari raya Natal kepada saudara saya yang memeluk agama Kristen. Apakah kita diperbolehkan menyampaikan ucapan selamat hari raya kepada pemeluk agama lain? Apabila kita juga ingin menyampaikannya melalui pesan singkat seperti SMS atau BBM, bagaimana hukumnya? Mohon penjelasannya. Terima kasih sebelumnya atas jawabannya. (Nunung, Sumedang)', ''),
(4, 'Test2', 'Dr. Aam Amiruddin', '2016-01-04 17:00:00', 'Pak ustadz yang saya hormati, bagaimana hukumnya apabila kita mengucapkan selamat hari raya Natal kepada saudara saya yang memeluk agama Kristen. Apakah kita diperbolehkan menyampaikan ucapan selamat hari raya kepada pemeluk agama lain? Apabila kita juga ingin menyampaikannya melalui pesan singkat seperti SMS atau BBM, bagaimana hukumnya? Mohon penjelasannya. Terima kasih sebelumnya atas jawabannya. (Nunung, Sumedang)', ''),
(5, 'test tanggal lain', 'Ustadz Rizal', '2016-01-17 17:00:00', '', '');

-- --------------------------------------------------------

--
-- Table structure for table `slider`
--

CREATE TABLE IF NOT EXISTS `slider` (
  `id_slider` int(11) NOT NULL AUTO_INCREMENT,
  `gambar` text NOT NULL,
  `status` tinyint(1) NOT NULL,
  PRIMARY KEY (`id_slider`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `ustadz`
--

CREATE TABLE IF NOT EXISTS `ustadz` (
  `id_ustadz` int(11) NOT NULL AUTO_INCREMENT,
  `nama` varchar(255) NOT NULL,
  `profil` text NOT NULL,
  `foto` text NOT NULL,
  PRIMARY KEY (`id_ustadz`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=2 ;

--
-- Dumping data for table `ustadz`
--

INSERT INTO `ustadz` (`id_ustadz`, `nama`, `profil`, `foto`) VALUES
(1, 'Pak Aam', 'beliau adalaha adj ajdhb ahsdb', 'aam.png');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
