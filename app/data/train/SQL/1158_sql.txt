-- --------------------------------------------------------
-- Host:                         127.0.0.1
-- VersiÃ³n del servidor:         10.2.6-MariaDB - mariadb.org binary distribution
-- SO del servidor:              Win64
-- HeidiSQL VersiÃ³n:             9.4.0.5125
-- --------------------------------------------------------

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET NAMES utf8 */;
/*!50503 SET NAMES utf8mb4 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;


-- Volcando estructura de base de datos para bdraiz
DROP DATABASE IF EXISTS `bdraiz`;
CREATE DATABASE IF NOT EXISTS `bdraiz` /*!40100 DEFAULT CHARACTER SET utf8 COLLATE utf8_spanish_ci */;
USE `bdraiz`;

-- Volcando estructura para tabla bdraiz.aprobacion
DROP TABLE IF EXISTS `aprobacion`;
CREATE TABLE IF NOT EXISTS `aprobacion` (
  `CODIGO_APROBACION` int(11) NOT NULL AUTO_INCREMENT,
  `CODIGO_AUTORIZACION` int(11) NOT NULL,
  `CODIGO_USUARIO` int(11) DEFAULT NULL,
  `COMENTARIO` varchar(50) COLLATE utf8_spanish_ci DEFAULT NULL,
  `FECHA_APROBACION` datetime DEFAULT NULL,
  PRIMARY KEY (`CODIGO_APROBACION`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.aprobacion: ~0 rows (aproximadamente)
/*!40000 ALTER TABLE `aprobacion` DISABLE KEYS */;
/*!40000 ALTER TABLE `aprobacion` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.cts
DROP TABLE IF EXISTS `cts`;
CREATE TABLE IF NOT EXISTS `cts` (
  `CODIGO_CTS` int(11) NOT NULL AUTO_INCREMENT,
  `CODIGO_EMPLEADO` int(11) NOT NULL,
  `CODIGO_EMPRESA` int(11) NOT NULL,
  `FECHA_INGRESO` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  `FECHA_MODIFICA` varchar(50) COLLATE utf8_spanish_ci DEFAULT NULL,
  `TASA` double NOT NULL,
  `ESTADO` char(1) COLLATE utf8_spanish_ci NOT NULL,
  `CONDICION` char(1) COLLATE utf8_spanish_ci NOT NULL,
  `SALDO` double NOT NULL,
  `CODIGO_USUARIO` int(11) DEFAULT NULL,
  PRIMARY KEY (`CODIGO_CTS`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.cts: ~2 rows (aproximadamente)
/*!40000 ALTER TABLE `cts` DISABLE KEYS */;
INSERT INTO `cts` (`CODIGO_CTS`, `CODIGO_EMPLEADO`, `CODIGO_EMPRESA`, `FECHA_INGRESO`, `FECHA_MODIFICA`, `TASA`, `ESTADO`, `CONDICION`, `SALDO`, `CODIGO_USUARIO`) VALUES
	(1, 1, 3, '25/07/2017 11:22', '25/07/2017 14:55', 2.1, '0', 'T', 1200, 1),
	(4, 3, 5, '25/07/2017 15:09', '', 3, '0', 'N', 0, 2);
/*!40000 ALTER TABLE `cts` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.empleado
DROP TABLE IF EXISTS `empleado`;
CREATE TABLE IF NOT EXISTS `empleado` (
  `CODIGO_EMPLEADO` int(11) NOT NULL AUTO_INCREMENT,
  `CODIGO_PAIS` int(11) NOT NULL,
  `CODIGO_TIPDOC` int(11) NOT NULL,
  `NRO_DOC` varchar(11) COLLATE utf8_spanish_ci NOT NULL,
  `APELLIDO_PATERNO` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  `APELLIDO_MATERNO` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  `NOMBRES` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  `FECHA_NACIMIENTO` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  `DOMICILIO` varchar(100) COLLATE utf8_spanish_ci NOT NULL,
  `EMAIL` varchar(50) COLLATE utf8_spanish_ci DEFAULT NULL,
  PRIMARY KEY (`CODIGO_EMPLEADO`)
) ENGINE=InnoDB AUTO_INCREMENT=11 DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.empleado: ~10 rows (aproximadamente)
/*!40000 ALTER TABLE `empleado` DISABLE KEYS */;
INSERT INTO `empleado` (`CODIGO_EMPLEADO`, `CODIGO_PAIS`, `CODIGO_TIPDOC`, `NRO_DOC`, `APELLIDO_PATERNO`, `APELLIDO_MATERNO`, `NOMBRES`, `FECHA_NACIMIENTO`, `DOMICILIO`, `EMAIL`) VALUES
	(1, 1, 1, '4423154', 'PEREZ', 'PADILLA', 'PEPITO', '12/04/1980', 'CALLE SIN NUMERO MZ O LT 15', 'PEPITO@GMAIL.COM'),
	(2, 1, 1, '41414548', 'CASTILLON', 'SIUCE', 'RAUL EUGENIO', '16/05/1985', 'JR ANTOFAGASTA 2174', 'RAUL@GMAIL.COM'),
	(3, 1, 1, '85858545', 'GOMEZ', 'MANSILLA', 'GLORIA VERONI', '31/10/1981', 'JR BELLO HORIZONTE 1699', 'GLORIA@GMAIL.COM'),
	(4, 1, 1, '96523541', 'MONTES', 'SANCHEZ', 'OSCAR TEODOSI', '02/12/1986', 'CL JORGE CHAVEZ 379', 'OSCAR@GMAIL.COM'),
	(5, 1, 1, '75412500', 'ORTIZ', 'OLIVOS', 'AMALIA PATRICIA', '05/07/1982', 'UR UR PERUAV LIMA 3839', 'PATRICIA@GMAIL.COM'),
	(6, 1, 1, '65415985', 'MURGUIA', 'PINO DE BARRENA', 'DORA', '08/07/1975', 'AV SUCRE 1173 Int: 35', 'DORA@GMAIL.COM'),
	(7, 1, 1, '85200012', 'DIAZ', 'YENGLE', 'LUCILA VIOLETA', '16/06/1984', 'CL EUSEBIO GALVEZ 174', 'VIOLETA@GMAIL.COM'),
	(8, 1, 0, '1231221212', 'RAMIREZ', 'TANDAZO', 'IVAN', '07/07/2017', 'AV METROPOLITANA', 'IVAN@IVAN.COM'),
	(9, 1, 0, '44235854', 'HUARCAYA', 'HINOSTROZA', 'JONATHAN', '04/07/2017', 'SAN MIGUEL', 'JONATHAN@JONATHAN'),
	(10, 2, 1, '1515151515', 'FGHJGFKL', 'DGHJFGKGLHKJ', 'GTUYJTYU', '27/06/2017', 'DGJKJDFG', 'KHGJKHGJHG');
/*!40000 ALTER TABLE `empleado` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.empresa
DROP TABLE IF EXISTS `empresa`;
CREATE TABLE IF NOT EXISTS `empresa` (
  `CODIGO_EMPRESA` int(11) NOT NULL AUTO_INCREMENT,
  `RUC` varchar(20) COLLATE utf8_spanish_ci NOT NULL,
  `DESCRIPCION` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  `DIRECCION` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  PRIMARY KEY (`CODIGO_EMPRESA`)
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.empresa: ~8 rows (aproximadamente)
/*!40000 ALTER TABLE `empresa` DISABLE KEYS */;
INSERT INTO `empresa` (`CODIGO_EMPRESA`, `RUC`, `DESCRIPCION`, `DIRECCION`) VALUES
	(1, '15452548541', 'TELEFONICA', 'CL VALDEZ, FULGENCIO 505 Piso: 2'),
	(2, '65254857485', 'CIBERTEC', 'UR UR ARCO IRISPJ SENDA DORADA 0 Int: 303'),
	(3, '96352154012', 'GLORIA', 'AV BOLIVIA 1091 Piso: 2 Int: 202'),
	(4, '90120152410', 'METRO', 'CL LA TORRE BALTAZAR 470 Int: 11'),
	(5, '80352102541', 'SAGA FALABELLA', 'JR JR REBECA OQUENDO 409 CD UNICO  706  '),
	(6, '21023254857', 'COBRA PERU', 'UR UR ORBEACL ECHENIQUE 306 Piso: 10 Int: 1001'),
	(7, '78987456548', 'lari construcciones', 'ate vitarte'),
	(8, '45645645612', 'IPHONE', 'EN ESTADOS UNIDOS');
/*!40000 ALTER TABLE `empresa` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.empresa_empleado
DROP TABLE IF EXISTS `empresa_empleado`;
CREATE TABLE IF NOT EXISTS `empresa_empleado` (
  `IDEMPLEADO_EMPLEADOR` int(11) NOT NULL AUTO_INCREMENT,
  `CODIGO_EMPRESA` int(11) NOT NULL,
  `CODIGO_EMPLEADO` int(11) NOT NULL,
  PRIMARY KEY (`IDEMPLEADO_EMPLEADOR`)
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.empresa_empleado: ~5 rows (aproximadamente)
/*!40000 ALTER TABLE `empresa_empleado` DISABLE KEYS */;
INSERT INTO `empresa_empleado` (`IDEMPLEADO_EMPLEADOR`, `CODIGO_EMPRESA`, `CODIGO_EMPLEADO`) VALUES
	(1, 6, 10),
	(2, 1, 10),
	(3, 3, 10),
	(4, 3, 1),
	(5, 5, 3);
/*!40000 ALTER TABLE `empresa_empleado` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.excepcion_tasa
DROP TABLE IF EXISTS `excepcion_tasa`;
CREATE TABLE IF NOT EXISTS `excepcion_tasa` (
  `CODIGO_EXCEPCION` int(11) NOT NULL AUTO_INCREMENT,
  `CODIGO_PERFIL` int(11) DEFAULT NULL,
  `TASA_MINIMA` double DEFAULT NULL,
  PRIMARY KEY (`CODIGO_EXCEPCION`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.excepcion_tasa: ~0 rows (aproximadamente)
/*!40000 ALTER TABLE `excepcion_tasa` DISABLE KEYS */;
/*!40000 ALTER TABLE `excepcion_tasa` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.pais
DROP TABLE IF EXISTS `pais`;
CREATE TABLE IF NOT EXISTS `pais` (
  `CODIGO_PAIS` int(11) NOT NULL AUTO_INCREMENT,
  `DESCRIPCION` varchar(50) COLLATE utf8_spanish_ci DEFAULT NULL,
  PRIMARY KEY (`CODIGO_PAIS`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.pais: ~4 rows (aproximadamente)
/*!40000 ALTER TABLE `pais` DISABLE KEYS */;
INSERT INTO `pais` (`CODIGO_PAIS`, `DESCRIPCION`) VALUES
	(1, 'PERU'),
	(2, 'ECUADOR'),
	(3, 'BRASIL'),
	(4, 'ARGENTINA');
/*!40000 ALTER TABLE `pais` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.perfil
DROP TABLE IF EXISTS `perfil`;
CREATE TABLE IF NOT EXISTS `perfil` (
  `CODIGO_PERFIL` int(11) NOT NULL AUTO_INCREMENT,
  `DESCRIPCION` varchar(50) COLLATE utf8_spanish_ci DEFAULT NULL,
  PRIMARY KEY (`CODIGO_PERFIL`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.perfil: ~2 rows (aproximadamente)
/*!40000 ALTER TABLE `perfil` DISABLE KEYS */;
INSERT INTO `perfil` (`CODIGO_PERFIL`, `DESCRIPCION`) VALUES
	(1, 'FSO GERENTE DE FINANZAS'),
	(2, 'EJECUTIVO DE PLATAFORMA');
/*!40000 ALTER TABLE `perfil` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.tipo_autorizacion
DROP TABLE IF EXISTS `tipo_autorizacion`;
CREATE TABLE IF NOT EXISTS `tipo_autorizacion` (
  `CODIGO_AUTORIZACION` int(11) NOT NULL AUTO_INCREMENT,
  `DESCRIPCION` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  PRIMARY KEY (`CODIGO_AUTORIZACION`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.tipo_autorizacion: ~2 rows (aproximadamente)
/*!40000 ALTER TABLE `tipo_autorizacion` DISABLE KEYS */;
INSERT INTO `tipo_autorizacion` (`CODIGO_AUTORIZACION`, `DESCRIPCION`) VALUES
	(1, 'APROBACION'),
	(2, 'EXCEPCION');
/*!40000 ALTER TABLE `tipo_autorizacion` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.tipo_documento
DROP TABLE IF EXISTS `tipo_documento`;
CREATE TABLE IF NOT EXISTS `tipo_documento` (
  `CODIGO_TIPODOCUMENTO` int(11) NOT NULL AUTO_INCREMENT,
  `DESCRIPCION` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  PRIMARY KEY (`CODIGO_TIPODOCUMENTO`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.tipo_documento: ~3 rows (aproximadamente)
/*!40000 ALTER TABLE `tipo_documento` DISABLE KEYS */;
INSERT INTO `tipo_documento` (`CODIGO_TIPODOCUMENTO`, `DESCRIPCION`) VALUES
	(1, 'DNI'),
	(2, 'RUC'),
	(3, 'CARNET EXTRANJERIA');
/*!40000 ALTER TABLE `tipo_documento` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.usuario
DROP TABLE IF EXISTS `usuario`;
CREATE TABLE IF NOT EXISTS `usuario` (
  `CODIGO_USUARIO` int(11) NOT NULL AUTO_INCREMENT,
  `NOMBRE_USUARIO` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  `FECHA_INGRESO` datetime NOT NULL,
  PRIMARY KEY (`CODIGO_USUARIO`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.usuario: ~2 rows (aproximadamente)
/*!40000 ALTER TABLE `usuario` DISABLE KEYS */;
INSERT INTO `usuario` (`CODIGO_USUARIO`, `NOMBRE_USUARIO`, `FECHA_INGRESO`) VALUES
	(1, 'Huarcaya Jonathan', '2001-01-15 08:00:00'),
	(2, 'Olivera Adaia', '2005-02-18 08:00:00');
/*!40000 ALTER TABLE `usuario` ENABLE KEYS */;

-- Volcando estructura para tabla bdraiz.usuario_perfil
DROP TABLE IF EXISTS `usuario_perfil`;
CREATE TABLE IF NOT EXISTS `usuario_perfil` (
  `CODIGO_USUARIO` int(11) NOT NULL,
  `CODIGO_PERFIL` int(11) NOT NULL,
  `FECHA_VTO` varchar(50) COLLATE utf8_spanish_ci NOT NULL,
  PRIMARY KEY (`CODIGO_USUARIO`),
  KEY `CODIGO_PERFIL` (`CODIGO_PERFIL`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_spanish_ci;

-- Volcando datos para la tabla bdraiz.usuario_perfil: ~2 rows (aproximadamente)
/*!40000 ALTER TABLE `usuario_perfil` DISABLE KEYS */;
INSERT INTO `usuario_perfil` (`CODIGO_USUARIO`, `CODIGO_PERFIL`, `FECHA_VTO`) VALUES
	(1, 2, '31/12/2018 20:00'),
	(2, 1, '31/12/2018 20:00');
/*!40000 ALTER TABLE `usuario_perfil` ENABLE KEYS */;

/*!40101 SET SQL_MODE=IFNULL(@OLD_SQL_MODE, '') */;
/*!40014 SET FOREIGN_KEY_CHECKS=IF(@OLD_FOREIGN_KEY_CHECKS IS NULL, 1, @OLD_FOREIGN_KEY_CHECKS) */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
