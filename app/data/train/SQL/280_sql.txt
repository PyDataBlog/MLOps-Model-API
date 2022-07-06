-- MySQL dump 10.13  Distrib 5.7.14, for Win64 (x86_64)
--
-- Host: localhost    Database: administrador
-- ------------------------------------------------------
-- Server version	5.7.14

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
-- Table structure for table `activos`
--

DROP TABLE IF EXISTS `activos`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `activos` (
  `id_activo` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(255) NOT NULL,
  `descripcion` varchar(255) NOT NULL,
  PRIMARY KEY (`id_activo`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `activos`
--

LOCK TABLES `activos` WRITE;
/*!40000 ALTER TABLE `activos` DISABLE KEYS */;
/*!40000 ALTER TABLE `activos` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `activos_solicitudes`
--

DROP TABLE IF EXISTS `activos_solicitudes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `activos_solicitudes` (
  `id_solicitud` int(11) NOT NULL AUTO_INCREMENT,
  `id_activo` int(11) NOT NULL,
  `id_usuario` int(11) NOT NULL,
  `id_estado` int(11) NOT NULL,
  `entrada` timestamp NOT NULL,
  `salida` timestamp NULL DEFAULT NULL,
  PRIMARY KEY (`id_solicitud`),
  KEY `id_activo` (`id_activo`),
  KEY `id_usuario` (`id_usuario`),
  KEY `id_estado` (`id_estado`),
  CONSTRAINT `activos_solicitudes_ibfk_1` FOREIGN KEY (`id_activo`) REFERENCES `activos` (`id_activo`),
  CONSTRAINT `activos_solicitudes_ibfk_2` FOREIGN KEY (`id_usuario`) REFERENCES `usuarios` (`id_usuario`),
  CONSTRAINT `activos_solicitudes_ibfk_3` FOREIGN KEY (`id_estado`) REFERENCES `estados` (`id_estado`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `activos_solicitudes`
--

LOCK TABLES `activos_solicitudes` WRITE;
/*!40000 ALTER TABLE `activos_solicitudes` DISABLE KEYS */;
/*!40000 ALTER TABLE `activos_solicitudes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `archivos`
--

DROP TABLE IF EXISTS `archivos`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `archivos` (
  `id_archivo` int(11) NOT NULL AUTO_INCREMENT,
  `fecha` timestamp NOT NULL,
  `id_archivo_tipo` int(11) NOT NULL,
  `id_especialidad` int(11) NOT NULL,
  `id_usuario` int(11) NOT NULL,
  `id_tarea` int(11) NOT NULL,
  PRIMARY KEY (`id_archivo`),
  KEY `id_archivo_tipo` (`id_archivo_tipo`),
  KEY `id_especialidad` (`id_especialidad`),
  KEY `id_usuario` (`id_usuario`),
  KEY `id_tarea` (`id_tarea`),
  CONSTRAINT `archivos_ibfk_1` FOREIGN KEY (`id_archivo_tipo`) REFERENCES `archivos_tipo` (`id_archivo_tipo`),
  CONSTRAINT `archivos_ibfk_2` FOREIGN KEY (`id_especialidad`) REFERENCES `especialidad` (`id_especialidad`),
  CONSTRAINT `archivos_ibfk_3` FOREIGN KEY (`id_usuario`) REFERENCES `usuarios` (`id_usuario`),
  CONSTRAINT `archivos_ibfk_4` FOREIGN KEY (`id_tarea`) REFERENCES `tareas` (`id_tarea`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `archivos`
--

LOCK TABLES `archivos` WRITE;
/*!40000 ALTER TABLE `archivos` DISABLE KEYS */;
/*!40000 ALTER TABLE `archivos` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `archivos_tipo`
--

DROP TABLE IF EXISTS `archivos_tipo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `archivos_tipo` (
  `id_archivo_tipo` int(11) NOT NULL AUTO_INCREMENT,
  `extension` varchar(10) NOT NULL,
  `descripcion` varchar(255) NOT NULL,
  PRIMARY KEY (`id_archivo_tipo`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `archivos_tipo`
--

LOCK TABLES `archivos_tipo` WRITE;
/*!40000 ALTER TABLE `archivos_tipo` DISABLE KEYS */;
/*!40000 ALTER TABLE `archivos_tipo` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `especialidad`
--

DROP TABLE IF EXISTS `especialidad`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `especialidad` (
  `id_especialidad` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(255) NOT NULL,
  PRIMARY KEY (`id_especialidad`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `especialidad`
--

LOCK TABLES `especialidad` WRITE;
/*!40000 ALTER TABLE `especialidad` DISABLE KEYS */;
INSERT INTO `especialidad` VALUES (1,'contabilidad'),(3,'Secretariado Bilingue'),(4,'Informatica');
/*!40000 ALTER TABLE `especialidad` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `estados`
--

DROP TABLE IF EXISTS `estados`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `estados` (
  `id_estado` int(11) NOT NULL AUTO_INCREMENT,
  `estado` varchar(255) NOT NULL,
  PRIMARY KEY (`id_estado`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `estados`
--

LOCK TABLES `estados` WRITE;
/*!40000 ALTER TABLE `estados` DISABLE KEYS */;
/*!40000 ALTER TABLE `estados` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `memos`
--

DROP TABLE IF EXISTS `memos`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `memos` (
  `id_memo` int(11) NOT NULL AUTO_INCREMENT,
  `fecha_creacion` timestamp NOT NULL,
  `titulo` varchar(255) DEFAULT NULL,
  `descripcion` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id_memo`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `memos`
--

LOCK TABLES `memos` WRITE;
/*!40000 ALTER TABLE `memos` DISABLE KEYS */;
/*!40000 ALTER TABLE `memos` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tareas`
--

DROP TABLE IF EXISTS `tareas`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tareas` (
  `id_tarea` int(11) NOT NULL AUTO_INCREMENT,
  `descripcion` varchar(255) NOT NULL,
  `fecha_creacion` timestamp NOT NULL,
  `fecha_culminacion` timestamp NULL DEFAULT NULL,
  `id_usuario_creador` int(11) NOT NULL,
  `id_usuario_asignado` int(11) NOT NULL,
  `id_estado` int(11) NOT NULL,
  PRIMARY KEY (`id_tarea`),
  KEY `id_usuario_creador` (`id_usuario_creador`),
  KEY `id_usuario_asignado` (`id_usuario_asignado`),
  KEY `id_estado` (`id_estado`),
  CONSTRAINT `tareas_ibfk_1` FOREIGN KEY (`id_usuario_creador`) REFERENCES `usuarios` (`id_usuario`),
  CONSTRAINT `tareas_ibfk_2` FOREIGN KEY (`id_usuario_asignado`) REFERENCES `usuarios` (`id_usuario`),
  CONSTRAINT `tareas_ibfk_3` FOREIGN KEY (`id_estado`) REFERENCES `estados` (`id_estado`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tareas`
--

LOCK TABLES `tareas` WRITE;
/*!40000 ALTER TABLE `tareas` DISABLE KEYS */;
/*!40000 ALTER TABLE `tareas` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `usuarios`
--

DROP TABLE IF EXISTS `usuarios`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `usuarios` (
  `id_usuario` int(11) NOT NULL AUTO_INCREMENT,
  `id_usuario_tipo` int(11) NOT NULL,
  `id_especialidad` int(11) NOT NULL,
  `nombre` varchar(50) NOT NULL,
  `apellido` varchar(50) NOT NULL,
  `email` varchar(50) NOT NULL,
  `telefono` varchar(9) NOT NULL,
  `password` varchar(15) NOT NULL,
  PRIMARY KEY (`id_usuario`),
  KEY `id_usuario_tipo` (`id_usuario_tipo`),
  KEY `id_especialidad` (`id_especialidad`),
  CONSTRAINT `usuarios_ibfk_1` FOREIGN KEY (`id_usuario_tipo`) REFERENCES `usuarios_tipo` (`id_usuario_tipo`),
  CONSTRAINT `usuarios_ibfk_2` FOREIGN KEY (`id_especialidad`) REFERENCES `especialidad` (`id_especialidad`)
) ENGINE=InnoDB AUTO_INCREMENT=23 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `usuarios`
--

LOCK TABLES `usuarios` WRITE;
/*!40000 ALTER TABLE `usuarios` DISABLE KEYS */;
INSERT INTO `usuarios` VALUES (5,1,1,'Adelaida','Chavarria','lalychavac@yahoo.com','8818-2823','233'),(7,1,1,'adolfo','rojas','safito@dot.com','2425-0930','233'),(22,1,1,'Julian','Rojas','julrochava@gmail.com','8818-2823','ee313');
/*!40000 ALTER TABLE `usuarios` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `usuarios_memos`
--

DROP TABLE IF EXISTS `usuarios_memos`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `usuarios_memos` (
  `id_usuarios_memos` int(11) NOT NULL AUTO_INCREMENT,
  `id_memo` int(11) NOT NULL,
  `id_usuario` int(11) NOT NULL,
  `fecha_recibido` timestamp NULL DEFAULT NULL,
  PRIMARY KEY (`id_usuarios_memos`),
  KEY `id_memo` (`id_memo`),
  KEY `id_usuario` (`id_usuario`),
  CONSTRAINT `usuarios_memos_ibfk_1` FOREIGN KEY (`id_memo`) REFERENCES `memos` (`id_memo`),
  CONSTRAINT `usuarios_memos_ibfk_2` FOREIGN KEY (`id_usuario`) REFERENCES `usuarios` (`id_usuario`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `usuarios_memos`
--

LOCK TABLES `usuarios_memos` WRITE;
/*!40000 ALTER TABLE `usuarios_memos` DISABLE KEYS */;
/*!40000 ALTER TABLE `usuarios_memos` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `usuarios_tipo`
--

DROP TABLE IF EXISTS `usuarios_tipo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `usuarios_tipo` (
  `id_usuario_tipo` int(11) NOT NULL AUTO_INCREMENT,
  `rol` varchar(255) NOT NULL,
  PRIMARY KEY (`id_usuario_tipo`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `usuarios_tipo`
--

LOCK TABLES `usuarios_tipo` WRITE;
/*!40000 ALTER TABLE `usuarios_tipo` DISABLE KEYS */;
INSERT INTO `usuarios_tipo` VALUES (1,'admin'),(2,'profesor');
/*!40000 ALTER TABLE `usuarios_tipo` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2017-05-14  0:28:48
