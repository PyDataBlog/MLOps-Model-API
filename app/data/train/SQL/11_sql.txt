/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET NAMES utf8mb4 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;

-- Export de la structure de la base pour chacal
CREATE DATABASE IF NOT EXISTS `chacal` /*!40100 DEFAULT CHARACTER SET utf8 */;
USE `chacal`;

-- Export de la structure de table chacal. items
CREATE TABLE IF NOT EXISTS `items` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `category` varchar(255) NOT NULL DEFAULT '""',
  `desc` text NOT NULL DEFAULT '""',
  `notation` time NOT NULL DEFAULT '00:00:00',
  `positionlat` double DEFAULT NULL,
  `postionlong` double DEFAULT NULL,
  `date` datetime NOT NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- Export de la structure de table chacal. what
CREATE TABLE IF NOT EXISTS `what` (
  `item_id` bigint(20) NOT NULL,
  `what` varchar(255) NOT NULL,
  `how` varchar(255) NOT NULL,
  KEY `FK_what_items` (`item_id`),
  CONSTRAINT `FK_what_items` FOREIGN KEY (`item_id`) REFERENCES `items` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

/*!40101 SET SQL_MODE=IFNULL(@OLD_SQL_MODE, '') */;
/*!40014 SET FOREIGN_KEY_CHECKS=IF(@OLD_FOREIGN_KEY_CHECKS IS NULL, 1, @OLD_FOREIGN_KEY_CHECKS) */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;

