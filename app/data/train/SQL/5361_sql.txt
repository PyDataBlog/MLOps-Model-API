DROP TABLE IF EXISTS `item`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item` (
  `item_ID` int(11) NOT NULL AUTO_INCREMENT,
  `itemName` varchar(15) DEFAULT NULL,
  `price` int(11) DEFAULT NULL,
  `currency` varchar(10) DEFAULT NULL,
  PRIMARY KEY (`item_ID`)
) 
LOCK TABLES `item` WRITE;
/*!40000 ALTER TABLE `item` DISABLE KEYS */;
INSERT INTO `item` VALUES (1,'Watch',2000,'PKR'),(2,'MobilePhone',17000,'PRK'),(3,'AC',20000,'PKR'),(4,'TV',20000,'PKR'),(5,'Generator',4567,'PKR'),(6,'Refrigrator',569876,'PKR'),(7,'Oven',1123,'PKR'),(8,'EnergiSaver',5432,'PKR'),(9,'Juicer',876,'PKR'),(10,'Laptop',23333,'PKR'),(11,'Monitor',76666,'PKR'),(12,'CPU',990000,'PKR'),(13,'KeyBoard',7890,'PKR'),(14,'Mouse',567890,'PKR'),(15,'Fan',5430,'PKR'),(16,'Air Cooler',5430,'PKR'),(17,'TubeLight',345670,'PKR'),(18,'Bulb',4560,'PKR'),(19,'Charger',560,'PKR'),(20,'UPS',450,'PKR');
