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
-- Table structure for table `D8paragraph_revision__field_view`
--

DROP TABLE IF EXISTS `D8paragraph_revision__field_view`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `D8paragraph_revision__field_view` (
  `bundle` varchar(128) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `langcode` varchar(32) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'The language code for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_view_target_id` varchar(255) CHARACTER SET ascii NOT NULL COMMENT 'The ID of the target entity.',
  `field_view_display_id` varchar(255) CHARACTER SET ascii DEFAULT NULL COMMENT 'The ID of the display.',
  `field_view_argument` varchar(255) CHARACTER SET ascii DEFAULT NULL COMMENT 'An optional argument.',
  `field_view_title` int(11) DEFAULT NULL COMMENT 'Include title.',
  `field_view_data` longtext COMMENT 'Serialized data.',
  PRIMARY KEY (`entity_id`,`revision_id`,`deleted`,`delta`,`langcode`),
  KEY `bundle` (`bundle`),
  KEY `revision_id` (`revision_id`),
  KEY `field_view_target_id` (`field_view_target_id`),
  KEY `field_view_display_id` (`field_view_display_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='Revision archive storage for paragraph field field_view.';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `D8paragraph_revision__field_view`
--

LOCK TABLES `D8paragraph_revision__field_view` WRITE;
/*!40000 ALTER TABLE `D8paragraph_revision__field_view` DISABLE KEYS */;
INSERT INTO `D8paragraph_revision__field_view` VALUES ('embed_view',0,1,1,'en',0,'basic_page','block_1',NULL,0,NULL),('embed_view',0,1,2,'en',0,'basic_page','block_1',NULL,0,NULL),('embed_view',0,1,3,'en',0,'basic_page','block_1',NULL,0,NULL),('embed_view',0,2,4,'en',0,'basic_page','block_1',NULL,0,NULL),('embed_view',0,2,5,'en',0,'basic_page','block_1',NULL,0,NULL),('embed_view',0,2,7,'en',0,'basic_page','block_1',NULL,0,NULL),('embed_view',0,2,10,'en',0,'basic_page','block_1',NULL,0,NULL),('embed_view',0,3,6,'en',0,'basic_page','block_2',NULL,1,NULL),('embed_view',0,3,8,'en',0,'basic_page','block_2',NULL,1,NULL),('embed_view',0,3,11,'en',0,'basic_page','block_2',NULL,1,NULL);
/*!40000 ALTER TABLE `D8paragraph_revision__field_view` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2018-07-17  1:34:29
