-- phpMyAdmin SQL Dump
-- version 3.5.5
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Apr 18, 2013 at 04:00 PM
-- Server version: 5.5.30-cll
-- PHP Version: 5.3.17

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `mag1_magrocketdev2`
--

-- --------------------------------------------------------

--
-- Table structure for table `crud_histories`
--

CREATE TABLE IF NOT EXISTS `crud_histories` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `user_id` bigint(20) NOT NULL,
  `user_name` varchar(255) NOT NULL,
  `history_action` varchar(255) DEFAULT NULL,
  `history_date_time` datetime DEFAULT NULL,
  `history_table_name` varchar(255) DEFAULT NULL,
  `history_data` text,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 AUTO_INCREMENT=22 ;

--
-- Dumping data for table `crud_histories`
--

INSERT INTO `crud_histories` (`id`, `user_id`, `user_name`, `history_action`, `history_date_time`, `history_table_name`, `history_data`) VALUES
(1, 1, 'admin', 'update', '2013-04-11 10:35:10', 'crud_groups', '{"group_name":"Super Users","group_description":"","id":"2"}'),
(2, 1, 'admin', 'delete', '2013-04-11 11:54:40', 'SYSTEM_LOG', '{"ID":"828","TYPE":"1","MESSAGE":null,"TIMESTAMP":"2013-04-09 18:10:02"}'),
(3, 1, 'admin', 'update', '2013-04-11 12:49:06', 'crud_users', '{"user_image":"1354699666-image1.png","user_email":"admin@magrocket.com","user_first_name":"MagRocket","user_las_name":"Administrator","user_website":"http:\\/\\/www.magrocket.com","user_aim":"","user_yahoo":"","user_skype":"","user_info":"","id":"1"}'),
(4, 1, 'admin', 'update', '2013-04-11 12:49:58', 'crud_users', '{"user_image":"1365698995-MagRocketLogoSmall1.png","user_email":"admin@magrocket.com","user_first_name":"MagRocket","user_las_name":"Administrator","user_website":"http:\\/\\/www.magrocket.com","user_aim":"","user_yahoo":"","user_skype":"","user_info":"","id":"1"}'),
(5, 1, 'admin', 'update', '2013-04-11 12:58:35', 'crud_users', '{"user_name":"suser","user_password":"7c4a8d09ca3762af61e59520943dc26494f8941b","user_image":"1354699676-image2.png","group_id":"2","user_email":"suser@magrocket.com","user_first_name":"MagRocket","user_las_name":"SuperUser","user_website":"http:\\/\\/www.magrocket.com","user_aim":"","user_yahoo":"","user_skype":"","user_info":"","id":"2"}'),
(6, 1, 'admin', 'update', '2013-04-11 12:59:03', 'crud_users', '{"user_name":"user","user_password":"7c4a8d09ca3762af61e59520943dc26494f8941b","user_image":"1354699685-image3.png","group_id":"3","user_email":"user@magrocket.com","user_first_name":"MagRocket","user_las_name":"User","user_website":"http:\\/\\/www.magrocket.com","user_aim":"","user_yahoo":"","user_skype":"","user_info":"","id":"3"}'),
(7, 1, 'admin', 'update', '2013-04-12 17:40:49', 'PUBLICATION', '{"APP_ID":"com.nin9creative.magrocket","NAME":"MagRocket Sample","ITUNES_SHARED_SECRET":"27b3b6fe881f4e8b89251a4a4f79b758","PARSE_APPLICATION_ID":"CFTto7CKHLA9uvDbJJemIFkTdghZw3RGaVrIcfEY","PARSE_REST_API_KEY":"o1Vq1388iyPYBeqaEUdhMXwuhREHmNIxL2HjcBK1","ID":"6"}'),
(8, 1, 'MagRocketAdmin', 'update', '2013-04-13 00:03:07', 'crud_users', '{"user_image":"","user_email":"admin@magrocket.com","user_first_name":"MagRocket","user_las_name":"Administrator","user_website":"http:\\/\\/www.magrocket.com","user_aim":"","user_yahoo":"","user_skype":"","user_info":"<p>This is the MagRocket Administrator account.&nbsp; Do not tamper with this account.&nbsp; All users of the MagRocket Administration Console should log in with the <strong>suser<\\/strong> or <strong>user<\\/strong> accounts.<\\/p>\\n","id":"1"}'),
(9, 1, 'MagRocketAdmin', 'update', '2013-04-17 10:56:27', 'PUBLICATION', '{"APP_ID":"com.nin9creative.magrocket","NAME":"MagRocket Sample","ITUNES_SHARED_SECRET":"27b3b6fe881f4e8b89251a4a4f79b758","PARSE_APPLICATION_ID":"CFTto7CKHLA9uvDbJJemIFkTdghZw3RGaVrIcfEY","PARSE_REST_API_KEY":"o1Vq1388iyPYBeqaEUdhMXwuhREHmNIxL2HjcBK1","ID":"6"}'),
(10, 1, 'MagRocketAdmin', 'update', '2013-04-17 10:56:39', 'ISSUES', '{"APP_ID":"com.nin9creative.magrocket","NAME":"MagRocket-January","PRODUCT_ID":"com.nin9creative.magrocket.issues.january2013","TITLE":"January 2013","INFO":"Apple''s Man of the Year Issue.","DATE":"2013-01-01 00:00:00","COVER":"http:\\/\\/www.newsstandcontent.com\\/publication\\/com.nin9creative.magrocket\\/BookCoverTemplate@2x.png","URL":"http:\\/\\/www.newsstandcontent.com\\/publication\\/com.nin9creative.magrocket\\/samplemagazine.hpub","ID":"8"}'),
(11, 1, 'MagRocketAdmin', 'update', '2013-04-17 11:06:51', 'PUBLICATION', '{"APP_ID":"com.nin9creative.magrocket","NAME":"MagRocket Sample","DEVELOPMENT_MODE":"1","ITUNES_SHARED_SECRET":"27b3b6fe881f4e8b89251a4a4f79b758","PARSE_APPLICATION_ID":"CFTto7CKHLA9uvDbJJemIFkTdghZw3RGaVrIcfEY","PARSE_REST_API_KEY":"o1Vq1388iyPYBeqaEUdhMXwuhREHmNIxL2HjcBK1","ID":"6"}'),
(12, 1, 'MagRocketAdmin', 'update', '2013-04-17 11:08:05', 'PUBLICATION', '{"APP_ID":"com.nin9creative.magrocket","NAME":"MagRocket Sample","DEVELOPMENT_MODE":"TRUE","ITUNES_SHARED_SECRET":"27b3b6fe881f4e8b89251a4a4f79b758","PARSE_APPLICATION_ID":"CFTto7CKHLA9uvDbJJemIFkTdghZw3RGaVrIcfEY","PARSE_REST_API_KEY":"o1Vq1388iyPYBeqaEUdhMXwuhREHmNIxL2HjcBK1","ID":"6"}'),
(13, 1, 'MagRocketAdmin', 'update', '2013-04-17 11:11:04', 'PUBLICATION', '{"APP_ID":"com.nin9creative.magrocket","NAME":"MagRocket Sample","DEVELOPMENT_MODE":"TRUE","ITUNES_SHARED_SECRET":"27b3b6fe881f4e8b89251a4a4f79b758","PARSE_APPLICATION_ID":"CFTto7CKHLA9uvDbJJemIFkTdghZw3RGaVrIcfEY","PARSE_REST_API_KEY":"o1Vq1388iyPYBeqaEUdhMXwuhREHmNIxL2HjcBK1","ID":"6"}'),
(14, 1, 'MagRocketAdmin', 'update', '2013-04-17 11:11:23', 'PUBLICATION', '{"APP_ID":"com.nin9creative.magrocket","NAME":"MagRocket Sample","DEVELOPMENT_MODE":"FALSE","ITUNES_SHARED_SECRET":"27b3b6fe881f4e8b89251a4a4f79b758","PARSE_APPLICATION_ID":"CFTto7CKHLA9uvDbJJemIFkTdghZw3RGaVrIcfEY","PARSE_REST_API_KEY":"o1Vq1388iyPYBeqaEUdhMXwuhREHmNIxL2HjcBK1","ID":"6"}'),
(15, 1, 'MagRocketAdmin', 'update', '2013-04-17 11:11:34', 'PUBLICATION', '{"APP_ID":"com.nin9creative.magrocket","NAME":"MagRocket Sample","DEVELOPMENT_MODE":"TRUE","ITUNES_SHARED_SECRET":"27b3b6fe881f4e8b89251a4a4f79b758","PARSE_APPLICATION_ID":"CFTto7CKHLA9uvDbJJemIFkTdghZw3RGaVrIcfEY","PARSE_REST_API_KEY":"o1Vq1388iyPYBeqaEUdhMXwuhREHmNIxL2HjcBK1","ID":"6"}'),
(16, 1, 'MagRocketAdmin', 'delete', '2013-04-17 11:14:15', 'APNS_TOKENS', '{"APP_ID":"com.nin9creative.magrocket","USER_ID":"B7340733-CC87-420B-888A-EA24776E276A","APNS_TOKEN":""}'),
(17, 1, 'MagRocketAdmin', 'update', '2013-04-17 11:24:08', 'SUBSCRIPTIONS', '{"APP_ID":"com.nin9creative.magrocket","USER_ID":"B7340733-CC87-420B-888A-EA24776E276A","EFFECTIVE_DATE":"2013-04-07 20:07:21","EXPIRATION_DATE":"2013-04-09 20:37:21","LAST_VALIDATED":"2013-04-17 07:25:49"}'),
(18, 1, 'MagRocketAdmin', 'update', '2013-04-17 17:11:50', 'crud_users', '{"user_image":"","user_email":"admin@magrocket.com","user_first_name":"MagRocket","user_las_name":"Administrator","user_website":"http:\\/\\/www.magrocket.com","user_aim":"","user_yahoo":"","user_skype":"","user_info":"<p>This is the MagRocket Administrator account.&nbsp; Do not tamper with this account.&nbsp; All users of the MagRocket Administration Console should log in with the <strong>suser<\\/strong> or <strong>user<\\/strong> accounts.&nbsp; Using this account may harm or delete senstive data or corrupt the MagRocket installation.<\\/p>\\n","id":"1"}'),
(19, 1, 'MagRocketAdmin', 'delete', '2013-04-17 22:34:18', 'crud_users', '{"id":"3","group_id":"3","user_name":"user","user_password":"7c4a8d09ca3762af61e59520943dc26494f8941b","user_first_name":"MagRocket","user_las_name":"User","user_image":"1354699685-image3.png","user_email":"user@magrocket.com","user_website":"http:\\/\\/www.magrocket.com","user_aim":"","user_yahoo":"","user_skype":"","user_info":"","user_manage_flag":"0"}'),
(20, 1, 'MagRocketAdmin', 'update', '2013-04-17 22:35:21', 'crud_users', '{"user_name":"suser","user_password":"7c4a8d09ca3762af61e59520943dc26494f8941b","user_image":"1354699676-image2.png","group_id":"2","user_email":"user@magrocket.com","user_first_name":"MagRocket","user_las_name":"User","user_website":"http:\\/\\/www.magrocket.com","user_aim":"","user_yahoo":"","user_skype":"","user_info":"","id":"2"}'),
(21, 1, 'MagRocketAdmin', 'delete', '2013-04-17 22:36:22', 'crud_groups', '{"id":"3","group_name":"Users","group_description":null,"group_manage_flag":"0","group_full_controll":"2","group_read":"1","group_read_write":"2","group_read_delete":"2"}');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
