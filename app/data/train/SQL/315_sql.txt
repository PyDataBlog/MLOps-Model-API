# ************************************************************
# Sequel Pro SQL dump
# Version 4541
#
# http://www.sequelpro.com/
# https://github.com/sequelpro/sequelpro
#
# Hôte: 127.0.0.1 (MySQL 5.7.17)
# Base de données: seriestv
# Temps de génération: 2017-09-12 09:39:55 +0000
# ************************************************************


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;


# Affichage de la table episode
# ------------------------------------------------------------

DROP TABLE IF EXISTS `episode`;

CREATE TABLE `episode` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `serie_id` int(11) DEFAULT NULL,
  `saison_id` int(11) DEFAULT NULL,
  `number` int(11) NOT NULL,
  `name` varchar(50) COLLATE utf8_unicode_ci NOT NULL,
  `duration` time DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `IDX_DDAA1CDAD94388BD` (`serie_id`),
  KEY `IDX_DDAA1CDAF965414C` (`saison_id`),
  CONSTRAINT `FK_DDAA1CDAD94388BD` FOREIGN KEY (`serie_id`) REFERENCES `serie` (`id`),
  CONSTRAINT `FK_DDAA1CDAF965414C` FOREIGN KEY (`saison_id`) REFERENCES `saison` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=31 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

LOCK TABLES `episode` WRITE;
/*!40000 ALTER TABLE `episode` DISABLE KEYS */;

INSERT INTO `episode` (`id`, `serie_id`, `saison_id`, `number`, `name`, `duration`)
VALUES
	(1,1,1,1,'L\'hiver vient','01:03:00'),
	(2,1,1,2,'La Route royale','00:56:00'),
	(3,1,1,3,'Lord Snow','00:57:00'),
	(4,1,1,4,'Infirmes, Bâtards et Choses brisées','00:56:00'),
	(5,1,1,5,'Le Loup et le Lion','00:55:00'),
	(6,1,1,6,'Une couronne dorée','00:55:00'),
	(7,1,1,7,'Gagner ou mourir','00:55:00'),
	(8,1,1,8,'Frapper d\'estoc','00:59:00'),
	(9,1,1,9,'Baelor','00:57:00'),
	(10,1,1,10,'De feu et de sang','00:53:00'),
	(11,1,2,1,'Le Nord se souvient','00:53:00'),
	(12,1,2,2,'Les Contrées nocturnes','00:54:00'),
	(13,1,2,3,'Ce qui est mort ne saurait mourir','00:53:00'),
	(14,1,2,4,'Le Jardin des os','00:51:00'),
	(15,1,2,5,'Le Fantôme d\'Harrenhal','00:55:00'),
	(16,1,2,6,'Les Anciens et les Nouveaux Dieux','00:54:00'),
	(17,1,2,7,'Un homme sans honneur','00:56:00'),
	(18,1,2,8,'Le Prince de Winterfell','00:54:00'),
	(19,1,2,9,'La Néra','00:55:00'),
	(20,1,2,10,'Valar Morghulis','01:04:00'),
	(21,1,3,1,'Valar Dohaeris','00:55:00'),
	(22,1,3,2,'Noires ailes, noires nouvelles','00:57:00'),
	(23,1,3,3,'Les Immaculés','00:53:00'),
	(24,1,3,4,'Voici que son tour de garde est fini','00:54:00'),
	(25,1,3,5,'Baisée par le feu','00:58:00'),
	(26,1,3,6,'L\'Ascension','00:53:00'),
	(27,1,3,7,'L\'Ours et la Belle','00:57:00'),
	(28,1,3,8,'Les Puînés','00:56:00'),
	(29,1,3,9,'Les Pluies de Castamere','00:50:00'),
	(30,1,3,10,'Mhysa','01:02:00');

/*!40000 ALTER TABLE `episode` ENABLE KEYS */;
UNLOCK TABLES;


# Affichage de la table people
# ------------------------------------------------------------

DROP TABLE IF EXISTS `people`;

CREATE TABLE `people` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) COLLATE utf8_unicode_ci NOT NULL,
  `birth_date` date NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;



# Affichage de la table people_type
# ------------------------------------------------------------

DROP TABLE IF EXISTS `people_type`;

CREATE TABLE `people_type` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `lib` varchar(50) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `UNIQ_8E179B99A90F3BCC` (`lib`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;



# Affichage de la table saison
# ------------------------------------------------------------

DROP TABLE IF EXISTS `saison`;

CREATE TABLE `saison` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(25) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `UNIQ_C0D0D5865E237E06` (`name`)
) ENGINE=InnoDB AUTO_INCREMENT=21 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

LOCK TABLES `saison` WRITE;
/*!40000 ALTER TABLE `saison` DISABLE KEYS */;

INSERT INTO `saison` (`id`, `name`)
VALUES
	(1,'Saison 01'),
	(2,'Saison 02'),
	(3,'Saison 03'),
	(4,'Saison 04'),
	(5,'Saison 05'),
	(6,'Saison 06'),
	(7,'Saison 07'),
	(8,'Saison 08'),
	(9,'Saison 09'),
	(10,'Saison 10'),
	(11,'Saison 11'),
	(12,'Saison 12'),
	(13,'Saison 13'),
	(14,'Saison 14'),
	(15,'Saison 15'),
	(16,'Saison 16'),
	(17,'Saison 17'),
	(18,'Saison 18'),
	(19,'Saison 19'),
	(20,'Saison 20');

/*!40000 ALTER TABLE `saison` ENABLE KEYS */;
UNLOCK TABLES;


# Affichage de la table serie
# ------------------------------------------------------------

DROP TABLE IF EXISTS `serie`;

CREATE TABLE `serie` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `nom` varchar(50) COLLATE utf8_unicode_ci NOT NULL,
  `date` date NOT NULL,
  `affiche` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `UNIQ_AA3A93346C6E55B5` (`nom`)
) ENGINE=InnoDB AUTO_INCREMENT=21 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

LOCK TABLES `serie` WRITE;
/*!40000 ALTER TABLE `serie` DISABLE KEYS */;

INSERT INTO `serie` (`id`, `nom`, `date`, `affiche`)
VALUES
	(1,'Game of Thrones','2011-04-17','f3145adcdacc9f075ef8e8febcc649b0.jpeg'),
	(10,'Fringe','2008-09-09','31faad7c9d34852298e98e0292943484.jpeg'),
	(11,'Lost','2004-09-22','c25a3645228a59d724aa70ec2c50e5fa.jpeg'),
	(12,'Person of Interest','2011-09-22','880617c603d0bfd1a35ba5d8a3a41454.jpeg'),
	(13,'Forever','2014-09-22','d08880fa77ee405f56994b20720e2cf9.jpeg'),
	(14,'Marvel\'s Agents of SHIELD','2013-09-24','942d65b44acdd04a0b3e2724c34ae69a.jpeg'),
	(15,'Agent Carter','2015-01-06','c5479e58d1d1e56c9e5bcbd98f72a869.jpeg'),
	(16,'Jessica Jones','2015-11-20','340dfac25223dffff0f874a220ec05c9.jpeg'),
	(17,'Daredevil','2015-04-10','afb30667bff6f71cee8bcbd6713e036a.jpeg'),
	(18,'Luke Cage','2016-09-30','ca37c326e23230365aef8129d829a514.jpeg'),
	(19,'The Defenders','2017-08-18','5fa3a7e70ae15fb57ba51023d79e5610.jpeg'),
	(20,'Iron Fist','2017-03-17','eeeea6051cdad6fcb5ab0e296f115116.jpeg');

/*!40000 ALTER TABLE `serie` ENABLE KEYS */;
UNLOCK TABLES;



/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
