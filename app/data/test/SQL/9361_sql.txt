CREATE DATABASE  IF NOT EXISTS `drupal8` /*!40100 DEFAULT CHARACTER SET utf8 */;
USE `drupal8`;
-- MySQL dump 10.13  Distrib 5.7.17, for macos10.12 (x86_64)
--
-- Host: localhost    Database: drupal8
-- ------------------------------------------------------
-- Server version	5.6.38

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
-- Table structure for table `D8node__field_body`
--

DROP TABLE IF EXISTS `D8node__field_body`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `D8node__field_body` (
  `bundle` varchar(128) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `langcode` varchar(32) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'The language code for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_body_target_id` int(10) unsigned NOT NULL COMMENT 'The ID of the target entity.',
  `field_body_depth` tinyint(3) unsigned DEFAULT NULL,
  `field_body_options` blob,
  PRIMARY KEY (`entity_id`,`deleted`,`delta`,`langcode`),
  KEY `bundle` (`bundle`),
  KEY `revision_id` (`revision_id`),
  KEY `field_body_target_id` (`field_body_target_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='Data storage for node field field_body.';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `D8node__field_body`
--

LOCK TABLES `D8node__field_body` WRITE;
/*!40000 ALTER TABLE `D8node__field_body` DISABLE KEYS */;
INSERT INTO `D8node__field_body` VALUES ('bricky',0,1,12,'en',0,1,0,'a:2:{s:6:\"layout\";s:13:\"layout_twocol\";s:9:\"css_class\";s:0:\"\";}'),('bricky',0,1,12,'en',1,3,1,'a:2:{s:9:\"view_mode\";s:7:\"default\";s:9:\"css_class\";s:0:\"\";}'),('bricky',0,1,12,'en',2,2,1,'a:2:{s:9:\"view_mode\";s:7:\"default\";s:9:\"css_class\";s:0:\"\";}'),('bricky',0,1,12,'en',3,3,1,'a:2:{s:9:\"view_mode\";s:7:\"default\";s:9:\"css_class\";s:16:\"text-align-right\";}'),('bricky',0,1,12,'en',4,3,1,'a:2:{s:9:\"view_mode\";s:7:\"default\";s:9:\"css_class\";s:0:\"\";}');
/*!40000 ALTER TABLE `D8node__field_body` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2018-07-17  1:34:22
