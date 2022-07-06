/*
SQLyog Ultimate v11.11 (64 bit)
MySQL - 5.6.17 : Database - camera
*********************************************************************
*/


/*!40101 SET NAMES utf8 */;

/*!40101 SET SQL_MODE=''*/;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;
CREATE DATABASE /*!32312 IF NOT EXISTS*/`camera` /*!40100 DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci */;

USE `camera`;

/*Table structure for table `banner` */

DROP TABLE IF EXISTS `banner`;

CREATE TABLE `banner` (
  `banner_id` int(11) NOT NULL AUTO_INCREMENT,
  `banner_name` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `banner_img` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `banner_url` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `banner_create_day` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`banner_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

/*Data for the table `banner` */

/*Table structure for table `category` */

DROP TABLE IF EXISTS `category`;

CREATE TABLE `category` (
  `cate_id` int(11) NOT NULL AUTO_INCREMENT,
  `cate_name` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`cate_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

/*Data for the table `category` */

/*Table structure for table `download` */

DROP TABLE IF EXISTS `download`;

CREATE TABLE `download` (
  `download_id` int(11) NOT NULL AUTO_INCREMENT,
  `download_name` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `download_content` text COLLATE utf8_unicode_ci,
  `download_file` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `download_create_day` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`download_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

/*Data for the table `download` */

/*Table structure for table `news` */

DROP TABLE IF EXISTS `news`;

CREATE TABLE `news` (
  `new_id` int(11) NOT NULL AUTO_INCREMENT,
  `new_cate_id` int(11) DEFAULT NULL,
  `new_title` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `new_desc` text COLLATE utf8_unicode_ci,
  `new_content` text COLLATE utf8_unicode_ci,
  `new_img` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `new_show` int(11) DEFAULT NULL,
  `new_create_day` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`new_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

/*Data for the table `news` */

/*Table structure for table `product` */

DROP TABLE IF EXISTS `product`;

CREATE TABLE `product` (
  `product_id` int(11) NOT NULL AUTO_INCREMENT,
  `product_cate_id` int(11) DEFAULT NULL,
  `product_name` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `product_desc` text COLLATE utf8_unicode_ci,
  `product_content_1` text COLLATE utf8_unicode_ci,
  `product_content_2` text COLLATE utf8_unicode_ci,
  `product_content_3` text COLLATE utf8_unicode_ci,
  `product_content_4` text COLLATE utf8_unicode_ci,
  `product_img` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `product_price` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `product_note` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `product_show` int(11) DEFAULT NULL,
  PRIMARY KEY (`product_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

/*Data for the table `product` */

/*Table structure for table `static_page` */

DROP TABLE IF EXISTS `static_page`;

CREATE TABLE `static_page` (
  `page_id` int(11) NOT NULL AUTO_INCREMENT,
  `page_name` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `page_content` text COLLATE utf8_unicode_ci,
  `page_create_day` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  `page_img` varchar(5000) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`page_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

/*Data for the table `static_page` */

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
