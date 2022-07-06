
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
DROP TABLE IF EXISTS `wp_phtc_leafletmapsmarker_layers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `wp_phtc_leafletmapsmarker_layers` (
  `id` int(6) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `basemap` varchar(25) NOT NULL,
  `layerzoom` int(2) NOT NULL,
  `mapwidth` int(4) NOT NULL,
  `mapwidthunit` varchar(2) NOT NULL,
  `mapheight` int(4) NOT NULL,
  `panel` tinyint(1) NOT NULL,
  `layerviewlat` decimal(9,6) NOT NULL,
  `layerviewlon` decimal(9,6) NOT NULL,
  `createdby` varchar(60) NOT NULL,
  `createdon` datetime NOT NULL,
  `updatedby` varchar(60) DEFAULT NULL,
  `updatedon` datetime DEFAULT NULL,
  `controlbox` int(1) NOT NULL,
  `overlays_custom` int(1) NOT NULL,
  `overlays_custom2` int(1) NOT NULL,
  `overlays_custom3` int(1) NOT NULL,
  `overlays_custom4` int(1) NOT NULL,
  `wms` tinyint(1) NOT NULL,
  `wms2` tinyint(1) NOT NULL,
  `wms3` tinyint(1) NOT NULL,
  `wms4` tinyint(1) NOT NULL,
  `wms5` tinyint(1) NOT NULL,
  `wms6` tinyint(1) NOT NULL,
  `wms7` tinyint(1) NOT NULL,
  `wms8` tinyint(1) NOT NULL,
  `wms9` tinyint(1) NOT NULL,
  `wms10` tinyint(1) NOT NULL,
  `listmarkers` tinyint(1) NOT NULL,
  `multi_layer_map` tinyint(1) NOT NULL,
  `multi_layer_map_list` varchar(4000) DEFAULT NULL,
  `address` varchar(255) NOT NULL,
  `clustering` tinyint(1) unsigned NOT NULL,
  `gpx_url` varchar(2083) NOT NULL,
  `gpx_panel` tinyint(1) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

LOCK TABLES `wp_phtc_leafletmapsmarker_layers` WRITE;
/*!40000 ALTER TABLE `wp_phtc_leafletmapsmarker_layers` DISABLE KEYS */;
INSERT INTO `wp_phtc_leafletmapsmarker_layers` VALUES (0,'markers not assigned to a layer','osm_mapnik',11,640,'px',480,0,0.000000,0.000000,'','0000-00-00 00:00:00',NULL,NULL,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL,'',0,'',0);
/*!40000 ALTER TABLE `wp_phtc_leafletmapsmarker_layers` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

