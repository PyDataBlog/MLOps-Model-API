-- MySQL dump 10.13  Distrib 5.7.12, for Win64 (x86_64)
--
-- Host: localhost    Database: love_academy
-- ------------------------------------------------------
-- Server version	5.7.16-log

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
-- Table structure for table `admin`
--

DROP TABLE IF EXISTS `admin`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `admin` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(100) DEFAULT NULL,
  `last_name` varchar(100) DEFAULT NULL,
  `mail` varchar(100) DEFAULT NULL,
  `gender` varchar(100) DEFAULT NULL,
  `password` varchar(100) DEFAULT NULL,
  `city` varchar(100) DEFAULT NULL,
  `country` varchar(100) DEFAULT NULL,
  `phone` varchar(25) DEFAULT NULL,
  `birthday` date DEFAULT NULL,
  `skype` varchar(50) DEFAULT NULL,
  `status` int(11) DEFAULT NULL,
  `avatar` varchar(100) DEFAULT NULL,
  `language_idlanguage` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_admin_language1_idx` (`language_idlanguage`),
  CONSTRAINT `fk_admin_language1` FOREIGN KEY (`language_idlanguage`) REFERENCES `language` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `admin`
--

LOCK TABLES `admin` WRITE;
/*!40000 ALTER TABLE `admin` DISABLE KEYS */;
INSERT INTO `admin` VALUES (1,'dfkuro','dfkuro','dfkuro@dfkuro.com',NULL,'a67446a5836bcf8c2a683b47295c11f0dcb640d0',NULL,NULL,NULL,NULL,'dfkuro',1,'default.jpg',1);
/*!40000 ALTER TABLE `admin` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `blog`
--

DROP TABLE IF EXISTS `blog`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `blog` (
  `idblog` int(11) NOT NULL,
  `type` varchar(45) DEFAULT NULL,
  `name` varchar(45) DEFAULT NULL,
  `date` varchar(45) DEFAULT NULL,
  `short_description` varchar(45) DEFAULT NULL,
  `description` varchar(45) DEFAULT NULL,
  `tags` varchar(45) DEFAULT NULL,
  `image` varchar(45) DEFAULT NULL,
  `Teacher_idTeacher` int(11) NOT NULL,
  `category_idcategory` int(11) NOT NULL,
  `admin_idAdmin` int(11) NOT NULL,
  PRIMARY KEY (`idblog`),
  KEY `fk_blog_Teacher1_idx` (`Teacher_idTeacher`),
  KEY `fk_blog_category1_idx` (`category_idcategory`),
  KEY `fk_blog_admin1_idx` (`admin_idAdmin`),
  CONSTRAINT `fk_blog_Teacher1` FOREIGN KEY (`Teacher_idTeacher`) REFERENCES `teacher` (`idTeacher`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_blog_admin1` FOREIGN KEY (`admin_idAdmin`) REFERENCES `admin` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_blog_category1` FOREIGN KEY (`category_idcategory`) REFERENCES `category` (`idcategory`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `blog`
--

LOCK TABLES `blog` WRITE;
/*!40000 ALTER TABLE `blog` DISABLE KEYS */;
/*!40000 ALTER TABLE `blog` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `category`
--

DROP TABLE IF EXISTS `category`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `category` (
  `idcategory` int(11) NOT NULL,
  `name` varchar(45) DEFAULT NULL,
  `status` varchar(45) DEFAULT NULL,
  PRIMARY KEY (`idcategory`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `category`
--

LOCK TABLES `category` WRITE;
/*!40000 ALTER TABLE `category` DISABLE KEYS */;
/*!40000 ALTER TABLE `category` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `download`
--

DROP TABLE IF EXISTS `download`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `download` (
  `iddownload` int(11) NOT NULL,
  `name` varchar(45) DEFAULT NULL,
  `type_archive` varchar(45) DEFAULT NULL,
  `date_upload` varchar(45) DEFAULT NULL,
  `archive` varchar(45) DEFAULT NULL,
  `teacher_idTeacher` int(11) NOT NULL,
  `category_idcategory` int(11) NOT NULL,
  PRIMARY KEY (`iddownload`),
  KEY `fk_download_teacher1_idx` (`teacher_idTeacher`),
  KEY `fk_download_category1_idx` (`category_idcategory`),
  CONSTRAINT `fk_download_category1` FOREIGN KEY (`category_idcategory`) REFERENCES `category` (`idcategory`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_download_teacher1` FOREIGN KEY (`teacher_idTeacher`) REFERENCES `teacher` (`idTeacher`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `download`
--

LOCK TABLES `download` WRITE;
/*!40000 ALTER TABLE `download` DISABLE KEYS */;
/*!40000 ALTER TABLE `download` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `experience`
--

DROP TABLE IF EXISTS `experience`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `experience` (
  `idExperience` int(11) NOT NULL AUTO_INCREMENT,
  `type` varchar(45) DEFAULT NULL,
  `name` varchar(45) DEFAULT NULL,
  `start_date` varchar(45) DEFAULT NULL,
  `finish_date` varchar(45) DEFAULT NULL,
  `location` varchar(45) DEFAULT NULL,
  `session_time` varchar(45) DEFAULT NULL,
  `total_quota` varchar(45) DEFAULT NULL,
  `avalaible_quota` varchar(45) DEFAULT NULL,
  `description` varchar(45) DEFAULT NULL,
  `image` varchar(45) DEFAULT NULL,
  `Teacher_idTeacher` int(11) NOT NULL,
  PRIMARY KEY (`idExperience`),
  KEY `fk_experience_Teacher1_idx` (`Teacher_idTeacher`),
  CONSTRAINT `fk_experience_Teacher1` FOREIGN KEY (`Teacher_idTeacher`) REFERENCES `teacher` (`idTeacher`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `experience`
--

LOCK TABLES `experience` WRITE;
/*!40000 ALTER TABLE `experience` DISABLE KEYS */;
/*!40000 ALTER TABLE `experience` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `interest`
--

DROP TABLE IF EXISTS `interest`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `interest` (
  `idinterest` int(11) NOT NULL,
  `experience_idExperience` int(11) NOT NULL,
  `session_idSession` int(11) NOT NULL,
  `blog_idblog` int(11) NOT NULL,
  `video_idvideo` int(11) NOT NULL,
  PRIMARY KEY (`idinterest`),
  KEY `fk_interest_experience1_idx` (`experience_idExperience`),
  KEY `fk_interest_session1_idx` (`session_idSession`),
  KEY `fk_interest_blog1_idx` (`blog_idblog`),
  KEY `fk_interest_video1_idx` (`video_idvideo`),
  CONSTRAINT `fk_interest_blog1` FOREIGN KEY (`blog_idblog`) REFERENCES `blog` (`idblog`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_interest_experience1` FOREIGN KEY (`experience_idExperience`) REFERENCES `experience` (`idExperience`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_interest_session1` FOREIGN KEY (`session_idSession`) REFERENCES `session` (`idSession`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_interest_video1` FOREIGN KEY (`video_idvideo`) REFERENCES `video` (`idvideo`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `interest`
--

LOCK TABLES `interest` WRITE;
/*!40000 ALTER TABLE `interest` DISABLE KEYS */;
/*!40000 ALTER TABLE `interest` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `language`
--

DROP TABLE IF EXISTS `language`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `language` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(45) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `language`
--

LOCK TABLES `language` WRITE;
/*!40000 ALTER TABLE `language` DISABLE KEYS */;
INSERT INTO `language` VALUES (1,'English');
/*!40000 ALTER TABLE `language` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `newsletter`
--

DROP TABLE IF EXISTS `newsletter`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `newsletter` (
  `idNewsletter` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(45) NOT NULL,
  `last_name` varchar(45) NOT NULL,
  `mail` varchar(45) DEFAULT NULL,
  `birthday` varchar(45) DEFAULT NULL,
  `gender` varchar(45) DEFAULT NULL,
  `city` varchar(45) DEFAULT NULL,
  `country` varchar(45) DEFAULT NULL,
  `status` varchar(45) DEFAULT NULL,
  PRIMARY KEY (`idNewsletter`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `newsletter`
--

LOCK TABLES `newsletter` WRITE;
/*!40000 ALTER TABLE `newsletter` DISABLE KEYS */;
/*!40000 ALTER TABLE `newsletter` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `pay`
--

DROP TABLE IF EXISTS `pay`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `pay` (
  `idpay` int(11) NOT NULL,
  `mount` varchar(45) DEFAULT NULL,
  `status` varchar(45) DEFAULT NULL,
  ` payment_method` varchar(45) DEFAULT NULL,
  PRIMARY KEY (`idpay`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `pay`
--

LOCK TABLES `pay` WRITE;
/*!40000 ALTER TABLE `pay` DISABLE KEYS */;
/*!40000 ALTER TABLE `pay` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `session`
--

DROP TABLE IF EXISTS `session`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `session` (
  `idSession` int(11) NOT NULL AUTO_INCREMENT,
  `type` varchar(45) DEFAULT NULL,
  `name` varchar(45) DEFAULT NULL,
  `start_date` varchar(45) DEFAULT NULL,
  `finish_date` varchar(45) DEFAULT NULL,
  `location` varchar(45) DEFAULT NULL,
  `session_time` varchar(45) DEFAULT NULL,
  `total_quota` varchar(45) DEFAULT NULL,
  `avalaible_quota` varchar(45) DEFAULT NULL,
  `description` varchar(45) DEFAULT NULL,
  `image` varchar(45) DEFAULT NULL,
  `Teacher_idTeacher` int(11) NOT NULL,
  PRIMARY KEY (`idSession`),
  KEY `fk_session_Teacher1_idx` (`Teacher_idTeacher`),
  CONSTRAINT `fk_session_Teacher1` FOREIGN KEY (`Teacher_idTeacher`) REFERENCES `teacher` (`idTeacher`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `session`
--

LOCK TABLES `session` WRITE;
/*!40000 ALTER TABLE `session` DISABLE KEYS */;
/*!40000 ALTER TABLE `session` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `teacher`
--

DROP TABLE IF EXISTS `teacher`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `teacher` (
  `idTeacher` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(45) DEFAULT NULL,
  `last_name` varchar(45) DEFAULT NULL,
  `mail` varchar(45) DEFAULT NULL,
  `gender` varchar(45) DEFAULT NULL,
  `pass` varchar(18) DEFAULT NULL,
  `city` varchar(45) DEFAULT NULL,
  `country` varchar(45) DEFAULT NULL,
  `phone` varchar(45) DEFAULT NULL,
  `birthday` date DEFAULT NULL,
  `hour_birthday` time DEFAULT NULL,
  `skype` varchar(45) DEFAULT NULL,
  `description` varchar(45) DEFAULT NULL,
  `status` binary(1) DEFAULT NULL,
  `avatar` varchar(45) DEFAULT NULL,
  `language_idlanguage` int(11) NOT NULL,
  PRIMARY KEY (`idTeacher`),
  KEY `fk_teacher_language1_idx` (`language_idlanguage`),
  CONSTRAINT `fk_teacher_language1` FOREIGN KEY (`language_idlanguage`) REFERENCES `language` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `teacher`
--

LOCK TABLES `teacher` WRITE;
/*!40000 ALTER TABLE `teacher` DISABLE KEYS */;
/*!40000 ALTER TABLE `teacher` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(45) NOT NULL,
  `last_name` varchar(45) NOT NULL,
  `mail` varchar(45) DEFAULT NULL,
  `gender` varchar(45) DEFAULT NULL,
  `pass` varchar(18) DEFAULT NULL,
  `city` varchar(45) DEFAULT NULL,
  `country` varchar(45) DEFAULT NULL,
  `phone` varchar(45) DEFAULT NULL,
  `Birthday` date DEFAULT NULL,
  `hour_birth` time DEFAULT NULL,
  `skype` varchar(45) DEFAULT NULL,
  `description` varchar(1000) DEFAULT NULL,
  `status` binary(1) DEFAULT NULL,
  `avatar` varchar(45) DEFAULT NULL,
  `language_idlanguage` int(11) NOT NULL,
  `interest_idinterest` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_user_language1_idx` (`language_idlanguage`),
  KEY `fk_user_interest1_idx` (`interest_idinterest`),
  CONSTRAINT `fk_user_interest1` FOREIGN KEY (`interest_idinterest`) REFERENCES `interest` (`idinterest`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_user_language1` FOREIGN KEY (`language_idlanguage`) REFERENCES `language` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
/*!40000 ALTER TABLE `user` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `video`
--

DROP TABLE IF EXISTS `video`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `video` (
  `idvideo` int(11) NOT NULL,
  `type` varchar(45) DEFAULT NULL,
  `name` varchar(45) DEFAULT NULL,
  `date` varchar(45) DEFAULT NULL,
  `short_description` varchar(45) DEFAULT NULL,
  `description` varchar(45) DEFAULT NULL,
  `tags` varchar(45) DEFAULT NULL,
  `Teacher_idTeacher` int(11) NOT NULL,
  `category_idcategory` int(11) NOT NULL,
  PRIMARY KEY (`idvideo`),
  KEY `fk_video_Teacher_idx` (`Teacher_idTeacher`),
  KEY `fk_video_category1_idx` (`category_idcategory`),
  CONSTRAINT `fk_video_Teacher` FOREIGN KEY (`Teacher_idTeacher`) REFERENCES `teacher` (`idTeacher`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fk_video_category1` FOREIGN KEY (`category_idcategory`) REFERENCES `category` (`idcategory`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `video`
--

LOCK TABLES `video` WRITE;
/*!40000 ALTER TABLE `video` DISABLE KEYS */;
/*!40000 ALTER TABLE `video` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2017-01-11 12:03:08
