-- phpMyAdmin SQL Dump
-- version 4.2.5
-- http://www.phpmyadmin.net
--
-- Host: localhost:8889
-- Generation Time: Apr 30, 2015 at 07:00 AM
-- Server version: 5.5.38
-- PHP Version: 5.5.14

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";

--
-- Database: `db_relief911`
--

-- --------------------------------------------------------

--
-- Table structure for table `donation`
--

DROP TABLE IF EXISTS `donation`;
CREATE TABLE `donation` (
`id` int(11) unsigned NOT NULL,
  `user_id` int(11) unsigned NOT NULL,
  `item` varchar(200) NOT NULL,
  `item_desc` varchar(400) NOT NULL,
  `item_type` enum('general','money','food','clothes','service') NOT NULL DEFAULT 'general',
  `num_items` int(11) unsigned NOT NULL DEFAULT '1',
  `where_to_collect` varchar(300) NOT NULL,
  `phone_number` varchar(120) NOT NULL,
  `media_upload_type` enum('photo','document','pdf','general') NOT NULL DEFAULT 'photo',
  `media_upload_path` varchar(300) NOT NULL,
  `status` varchar(60) NOT NULL,
  `date_added` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=2 ;

-- --------------------------------------------------------

--
-- Table structure for table `donation_distribution`
--

DROP TABLE IF EXISTS `donation_distribution`;
CREATE TABLE `donation_distribution` (
`id` int(11) NOT NULL,
  `donation_id` int(11) NOT NULL,
  `village_id` int(11) NOT NULL,
  `notes` varchar(500) NOT NULL,
  `approved_by_user_id` int(11) NOT NULL,
  `status` varchar(200) NOT NULL,
  `date_added` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `need_relief`
--

DROP TABLE IF EXISTS `need_relief`;
CREATE TABLE `need_relief` (
`id` int(11) NOT NULL,
  `parent_id` int(11) NOT NULL,
  `email_address` varchar(255) NOT NULL,
  `first_name` varchar(255) NOT NULL,
  `middle_name` varchar(255) NOT NULL,
  `last_name` varchar(255) NOT NULL,
  `gender` enum('m','f','u') NOT NULL,
  `age` int(5) NOT NULL,
  `address1` varchar(400) NOT NULL,
  `address2` varchar(400) NOT NULL,
  `tole` varchar(255) NOT NULL,
  `ward_num` int(11) NOT NULL,
  `vdc` varchar(255) NOT NULL,
  `district` varchar(255) NOT NULL,
  `country` varchar(255) NOT NULL,
  `phone_mobile` varchar(100) NOT NULL,
  `phone_home` varchar(100) NOT NULL,
  `notes` varchar(2000) NOT NULL,
  `village_id` int(11) NOT NULL,
  `date_added` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `relief_info`
--

DROP TABLE IF EXISTS `relief_info`;
CREATE TABLE `relief_info` (
`rlf_id` int(11) NOT NULL,
  `head_fname` varchar(100) NOT NULL,
  `head_mname` varchar(100) NOT NULL,
  `head_lname` varchar(100) NOT NULL,
  `gender` varchar(1) NOT NULL,
  `age` int(11) NOT NULL,
  `address 1` varchar(255) NOT NULL,
  `tole` varchar(255) NOT NULL,
  `ward_no` int(11) NOT NULL,
  `vdc` varchar(255) NOT NULL,
  `district` int(11) NOT NULL,
  `country` int(11) NOT NULL,
  `mobile_no` varchar(50) NOT NULL,
  `tel_no` varchar(50) NOT NULL,
  `email` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
CREATE TABLE `user` (
`id` int(11) unsigned NOT NULL,
  `user_type` enum('representative','admin','donator','editor') NOT NULL DEFAULT 'donator',
  `username` varchar(60) NOT NULL,
  `email` varchar(200) NOT NULL,
  `password` varchar(200) NOT NULL,
  `first_name` varchar(200) DEFAULT NULL,
  `second_name` varchar(200) DEFAULT NULL,
  `last_name` varchar(200) DEFAULT NULL,
  `phone` varchar(60) DEFAULT NULL,
  `city` varchar(150) DEFAULT NULL,
  `state` varchar(150) DEFAULT NULL,
  `country` varchar(150) DEFAULT NULL,
  `date_added` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=36 ;

-- --------------------------------------------------------

--
-- Table structure for table `village`
--

DROP TABLE IF EXISTS `village`;
CREATE TABLE `village` (
`id` int(11) unsigned NOT NULL,
  `name` varchar(200) NOT NULL,
  `description` varchar(400) NOT NULL,
  `representative_user_id` int(11) NOT NULL,
  `category` varchar(60) NOT NULL,
  `status` varchar(60) NOT NULL,
  `date_added` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=2 ;

--
-- Indexes for dumped tables
--

--
-- Indexes for table `donation`
--
ALTER TABLE `donation`
 ADD PRIMARY KEY (`id`);

--
-- Indexes for table `donation_distribution`
--
ALTER TABLE `donation_distribution`
 ADD PRIMARY KEY (`id`);

--
-- Indexes for table `need_relief`
--
ALTER TABLE `need_relief`
 ADD PRIMARY KEY (`id`);

--
-- Indexes for table `relief_info`
--
ALTER TABLE `relief_info`
 ADD PRIMARY KEY (`rlf_id`);

--
-- Indexes for table `user`
--
ALTER TABLE `user`
 ADD PRIMARY KEY (`id`), ADD UNIQUE KEY `username` (`username`), ADD UNIQUE KEY `email` (`email`);

--
-- Indexes for table `village`
--
ALTER TABLE `village`
 ADD PRIMARY KEY (`id`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `donation`
--
ALTER TABLE `donation`
MODIFY `id` int(11) unsigned NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=2;
--
-- AUTO_INCREMENT for table `donation_distribution`
--
ALTER TABLE `donation_distribution`
MODIFY `id` int(11) NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `need_relief`
--
ALTER TABLE `need_relief`
MODIFY `id` int(11) NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `relief_info`
--
ALTER TABLE `relief_info`
MODIFY `rlf_id` int(11) NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `user`
--
ALTER TABLE `user`
MODIFY `id` int(11) unsigned NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=2;
--
-- AUTO_INCREMENT for table `village`
--
ALTER TABLE `village`
MODIFY `id` int(11) unsigned NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=2;