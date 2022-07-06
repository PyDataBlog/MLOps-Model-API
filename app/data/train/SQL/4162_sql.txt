# ************************************************************
# Sequel Pro SQL dump
# Version 4135
#
# http://www.sequelpro.com/
# http://code.google.com/p/sequel-pro/
#
# Host: 127.0.0.1 (MySQL 5.5.42)
# Database: flexy
# Generation Time: 2015-11-16 11:24:20 +0000
# ************************************************************


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

# Dump of database flexy
# ------------------------------------------------------------

DROP DATABASE IF EXISTS `flexy`;

CREATE DATABASE `flexy`;

USE flexy;

# Dump of table faixa_ceps
# ------------------------------------------------------------

DROP TABLE IF EXISTS `faixa_ceps`;

CREATE TABLE `faixa_ceps` (
  `id_fc` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `faixa_cep_ini` int(11) NOT NULL,
  `faixa_cep_fim` int(11) NOT NULL,
  `localidade_faixa_cep` varchar(200) COLLATE utf8_unicode_ci DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` timestamp NULL DEFAULT NULL,
  PRIMARY KEY (`id_fc`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

LOCK TABLES `faixa_ceps` WRITE;
/*!40000 ALTER TABLE `faixa_ceps` DISABLE KEYS */;

INSERT INTO `faixa_ceps` (`id_fc`, `faixa_cep_ini`, `faixa_cep_fim`, `localidade_faixa_cep`, `created_at`, `updated_at`)
VALUES
	(3,88000001,88099999,'Florianópolis',NULL,NULL),
	(4,90000000,99999999,'Porto Alegre',NULL,NULL),
	(5,88340001,88349999,'Camboriú',NULL,NULL),
	(6,89200001,89239999,'Joinville',NULL,NULL),
	(7,80000001,82999999,'Curitiba','2015-11-16 04:13:27','2015-11-16 04:13:27');

/*!40000 ALTER TABLE `faixa_ceps` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table faixa_pesos
# ------------------------------------------------------------

DROP TABLE IF EXISTS `faixa_pesos`;

CREATE TABLE `faixa_pesos` (
  `id_fp` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `faixa_peso_ini` decimal(6,3) unsigned NOT NULL,
  `faixa_peso_fim` decimal(6,3) unsigned NOT NULL,
  `updated_at` timestamp NULL DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT NULL,
  PRIMARY KEY (`id_fp`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

LOCK TABLES `faixa_pesos` WRITE;
/*!40000 ALTER TABLE `faixa_pesos` DISABLE KEYS */;

INSERT INTO `faixa_pesos` (`id_fp`, `faixa_peso_ini`, `faixa_peso_fim`, `updated_at`, `created_at`)
VALUES
	(3,0.001,5.000,NULL,NULL),
	(4,5.001,10.000,NULL,NULL),
	(5,10.001,17.500,'2015-11-16 04:59:56','2015-11-16 04:59:56');

/*!40000 ALTER TABLE `faixa_pesos` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table migrations
# ------------------------------------------------------------

DROP TABLE IF EXISTS `migrations`;

CREATE TABLE `migrations` (
  `migration` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `batch` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

LOCK TABLES `migrations` WRITE;
/*!40000 ALTER TABLE `migrations` DISABLE KEYS */;

INSERT INTO `migrations` (`migration`, `batch`)
VALUES
	('2014_10_12_000000_create_users_table',1),
	('2014_10_12_100000_create_password_resets_table',1),
	('2015_11_10_200021_create_transportadoras_table',1),
	('2015_11_10_200057_create_faixa_ceps_table',1),
	('2015_11_10_200114_create_faixa_pesos_table',1),
	('2015_11_10_200210_create_valor_fc_fp_tps_table',1);

/*!40000 ALTER TABLE `migrations` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table transportadoras
# ------------------------------------------------------------

DROP TABLE IF EXISTS `transportadoras`;

CREATE TABLE `transportadoras` (
  `id_transportadora` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `nome_fantasia` varchar(200) COLLATE utf8_unicode_ci NOT NULL,
  `razao_social` varchar(600) COLLATE utf8_unicode_ci NOT NULL,
  `cnpj` varchar(13) COLLATE utf8_unicode_ci NOT NULL,
  `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` timestamp NULL DEFAULT NULL,
  `status_transportadora` tinyint(1) DEFAULT '1' COMMENT '0=inativo,1=ativo',
  PRIMARY KEY (`id_transportadora`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

LOCK TABLES `transportadoras` WRITE;
/*!40000 ALTER TABLE `transportadoras` DISABLE KEYS */;

INSERT INTO `transportadoras` (`id_transportadora`, `nome_fantasia`, `razao_social`, `cnpj`, `created_at`, `updated_at`, `status_transportadora`)
VALUES
	(4,'Dimi Transportes','Dimi Antoni ME','01394534078','0000-00-00 00:00:00','0000-00-00 00:00:00',1),
	(5,'Ingrid Transportes','Ingrid Transpordadora LTDA','01394534079','2015-11-15 14:35:34','0000-00-00 00:00:00',1),
	(6,'Transportadora inativa Teste','Transportadora inativa Teste','01394534080','2015-11-15 17:00:30','0000-00-00 00:00:00',0),
	(10,'grhgrehrehtrhrt','egrerhgrehrehtrhrt','1234344242424','2015-11-16 02:42:31','2015-11-16 02:42:31',1),
	(11,'gewrg43hrehrte','gerhrehrehe','1234533432532','2015-11-16 02:46:59','2015-11-16 02:46:59',1);

/*!40000 ALTER TABLE `transportadoras` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table valor_fc_fp_tps
# ------------------------------------------------------------

DROP TABLE IF EXISTS `valor_fc_fp_tps`;

CREATE TABLE `valor_fc_fp_tps` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `valor` decimal(5,2) NOT NULL,
  `faixa_cep_id` int(10) unsigned NOT NULL,
  `transportadora_id` int(10) unsigned NOT NULL,
  `faixa_peso_id` int(10) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `valor_fc_fp_tps_faixa_cep_id_foreign` (`faixa_cep_id`),
  KEY `valor_fc_fp_tps_transportadora_id_foreign` (`transportadora_id`),
  KEY `valor_fc_fp_tps_faixa_peso_id_foreign` (`faixa_peso_id`),
  CONSTRAINT `valor_fc_fp_tps_faixa_cep_id_foreign` FOREIGN KEY (`faixa_cep_id`) REFERENCES `faixa_ceps` (`id_fc`) ON DELETE CASCADE,
  CONSTRAINT `valor_fc_fp_tps_faixa_peso_id_foreign` FOREIGN KEY (`faixa_peso_id`) REFERENCES `faixa_pesos` (`id_fp`) ON DELETE CASCADE,
  CONSTRAINT `valor_fc_fp_tps_transportadora_id_foreign` FOREIGN KEY (`transportadora_id`) REFERENCES `transportadoras` (`id_transportadora`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

LOCK TABLES `valor_fc_fp_tps` WRITE;
/*!40000 ALTER TABLE `valor_fc_fp_tps` DISABLE KEYS */;

INSERT INTO `valor_fc_fp_tps` (`id`, `valor`, `faixa_cep_id`, `transportadora_id`, `faixa_peso_id`)
VALUES
	(7,50.00,3,4,3),
	(9,52.00,3,5,3),
	(10,76.00,3,4,4);

/*!40000 ALTER TABLE `valor_fc_fp_tps` ENABLE KEYS */;
UNLOCK TABLES;



/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
