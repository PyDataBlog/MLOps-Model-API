-- MySQL dump 10.13  Distrib 5.7.19, for Linux (x86_64)
--
-- Host: localhost    Database: javavirtual
-- ------------------------------------------------------
-- Server version	5.7.19-0ubuntu0.16.04.1

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
-- Table structure for table `message`
--

DROP TABLE IF EXISTS `message`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `message` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `stu_id` varchar(255) DEFAULT NULL COMMENT '留言学生id',
  `teac_id` varchar(255) DEFAULT NULL COMMENT '被留言教师id',
  `type` varchar(255) NOT NULL DEFAULT 'to_tec' COMMENT '类型（to_tec 或 to_stu）',
  `content` text COMMENT '内容',
  `uploadTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '留言时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=80 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `message`
--

LOCK TABLES `message` WRITE;
/*!40000 ALTER TABLE `message` DISABLE KEYS */;
INSERT INTO `message` VALUES (71,'22','3','to_tec','吴老师，请问实现接口和继承父类有什么区别？','2016-05-06 14:39:10'),(72,'23','4','to_tec','张老师，请问一个类可以实现多个接口吗？','2016-05-06 14:40:18'),(73,'22','3','to_stu','首先实现接口和类继承都是提供多态的机制。\n一个类可以实现多个接口，但只能继承一个类','2016-05-06 14:48:50'),(74,'22','3','to_stu','123','2016-05-29 05:23:18'),(75,'22','3','to_stu','没什么区别','2016-09-22 17:40:09'),(76,'27','3','to_tec','qwq','2016-08-23 03:51:56'),(77,'27','3','to_tec','','2016-08-23 03:52:07'),(78,'27','4','to_tec','qwq','2016-08-23 03:52:36'),(79,'28','3','to_tec','吴老师，我装环境的时候把前面的删了，┭┮﹏┭┮ 会不会有什么影响啊，能不能解决？','2016-09-17 12:06:51');
/*!40000 ALTER TABLE `message` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2020-08-09 17:37:19
