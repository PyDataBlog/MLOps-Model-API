-- MySQL dump 10.13  Distrib 5.6.12, for Win64 (x86_64)
--
-- Host: localhost    Database: mada_market
-- ------------------------------------------------------
-- Server version	5.6.12-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `m_advert`
--

DROP TABLE IF EXISTS `m_advert`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `m_advert` (
  `m_advert_id` int(11) NOT NULL AUTO_INCREMENT,
  `m_advert_category_id` int(11) DEFAULT NULL,
  `m_advert_title` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `m_advert_description` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `m_advert_nbrConsultation` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`m_advert_id`),
  KEY `IDX_1E62A6F5380D8BBD` (`m_advert_category_id`),
  CONSTRAINT `FK_1E62A6F5380D8BBD` FOREIGN KEY (`m_advert_category_id`) REFERENCES `m_category` (`m_category_id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `m_advert`
--

LOCK TABLES `m_advert` WRITE;
/*!40000 ALTER TABLE `m_advert` DISABLE KEYS */;
INSERT INTO `m_advert` VALUES (1,1,'IT service','creating webservice',NULL);
/*!40000 ALTER TABLE `m_advert` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `m_category`
--

DROP TABLE IF EXISTS `m_category`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `m_category` (
  `m_category_id` int(11) NOT NULL AUTO_INCREMENT,
  `m_category_name` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`m_category_id`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `m_category`
--

LOCK TABLES `m_category` WRITE;
/*!40000 ALTER TABLE `m_category` DISABLE KEYS */;
INSERT INTO `m_category` VALUES (1,'Vente'),(2,'Service'),(3,'Offre');
/*!40000 ALTER TABLE `m_category` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2017-02-06 15:48:05
