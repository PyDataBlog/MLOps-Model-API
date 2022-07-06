-- MySQL dump 10.13  Distrib 5.5.44, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: dspdb
-- ------------------------------------------------------
-- Server version	5.5.44-0ubuntu0.14.04.1

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
-- Table structure for table `conquest_system`
--

DROP TABLE IF EXISTS `conquest_system`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `conquest_system` (
  `region_id` tinyint(2) NOT NULL DEFAULT '0',
  `region_control` tinyint(2) NOT NULL DEFAULT '0',
  `region_control_prev` tinyint(2) NOT NULL DEFAULT '0',
  `sandoria_influence` int(10) NOT NULL DEFAULT '0',
  `bastok_influence` int(10) NOT NULL DEFAULT '0',
  `windurst_influence` int(10) NOT NULL DEFAULT '0',
  `beastmen_influence` int(10) NOT NULL DEFAULT '0',
  PRIMARY KEY (`region_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `conquest_system`
--

LOCK TABLES `conquest_system` WRITE;
/*!40000 ALTER TABLE `conquest_system` DISABLE KEYS */;
INSERT INTO `conquest_system` VALUES (0,0,1,1933,0,0,3067),(1,1,1,7927,0,0,0),(2,0,1,2365,638,0,1982),(3,1,1,0,35918,0,161),(4,1,0,0,150,0,0),(5,2,0,0,834,4134,23),(6,1,0,0,22002,0,222),(7,1,0,0,4889,0,98),(8,3,0,0,-2147483648,0,8000),(9,1,3,0,478,0,0),(10,1,3,1736,3098,0,140),(11,1,3,0,66,0,9),(12,1,3,0,63,0,0),(13,1,3,0,282,0,18),(14,1,3,0,341,0,0),(15,3,3,0,0,0,0),(16,1,3,0,702,0,467),(17,3,3,0,374,0,4626),(18,3,3,0,936,0,4064);
/*!40000 ALTER TABLE `conquest_system` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2015-09-18 12:28:11
