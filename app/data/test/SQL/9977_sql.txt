-- phpMyAdmin SQL Dump
-- version 4.0.10.7
-- http://www.phpmyadmin.net
--
-- Servidor: localhost
-- Tiempo de generación: 23-09-2015 a las 15:21:12
-- Versión del servidor: 5.5.45-cll-lve
-- Versión de PHP: 5.4.31

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Base de datos: `jaimeira_control-requerimientos`
--

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `carpeta`
--

CREATE TABLE IF NOT EXISTS `carpeta` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(100) COLLATE utf8_unicode_ci NOT NULL,
  `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `usuario_id` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=8 ;

--
-- Volcado de datos para la tabla `carpeta`
--

INSERT INTO `carpeta` (`id`, `nombre`, `created`, `usuario_id`) VALUES
(2, 'Nueva Carpeta', '2015-09-22 15:31:54', 1),
(3, 'Ingenieros', '2015-09-22 16:35:26', 1),
(4, 'Los 4 primeros', '2015-09-23 14:20:56', 1),
(5, 'Hello', '2015-09-23 15:40:44', 3),
(6, 'Honda', '2015-09-23 15:40:58', 3),
(7, 'IT', '2015-09-23 19:08:59', 3);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `carpeta_data`
--

CREATE TABLE IF NOT EXISTS `carpeta_data` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `carpeta_id` int(11) NOT NULL,
  `data_id` int(11) NOT NULL,
  `objeto_json` text COLLATE utf8_unicode_ci,
  `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=32 ;

--
-- Volcado de datos para la tabla `carpeta_data`
--

INSERT INTO `carpeta_data` (`id`, `carpeta_id`, `data_id`, `objeto_json`, `created`) VALUES
(9, 2, 1, '{"id":"1","nombre_carpeta":"Nueva Carpeta","nombre":"Luis Garza","carrera":"Ingenier\\u00eda","pais_estado":"Mexico-Jalisco"}', '2015-09-22 15:31:54'),
(10, 2, 2, '{"id":"2","nombre_carpeta":"Nueva Carpeta","nombre":"Jorge Verfara","carrera":"Licenciatura","pais_estado":"Estados Unidos-DF"}', '2015-09-22 15:31:54'),
(11, 2, 3, '{"id":"3","nombre_carpeta":"Nueva Carpeta","nombre":"Arturo Vasconselos","carrera":"Abogado","pais_estado":"Espa\\u00f1a-morelos"}', '2015-09-22 15:31:54'),
(12, 2, 4, '{"id":"4","nombre_carpeta":"Nueva Carpeta","nombre":"Alejandro Magno","carrera":"Aquirquitecto","pais_estado":"Colombia-Nuevo Leon"}', '2015-09-22 15:31:54'),
(13, 3, 1, '{"id":"1","nombre_carpeta":"Ingenieros","nombre":"Luis Garza \\n                                                     Ingenier\\u00eda \\n                                                     \\u00a0 \\u00a0 \\u00a0 \\u00a0 Mexico-Jalisco","carrera":"Ingenier\\u00eda","pais_estado":"Mexico-Jalisco"}', '2015-09-22 16:35:26'),
(14, 3, 5, '{"id":"5","nombre_carpeta":"Ingenieros","nombre":"Marco Jaimes \\n                                                     Ingenier\\u00eda \\n                                                     \\u00a0 \\u00a0 \\u00a0 \\u00a0 Australia-Colima","carrera":"Ingenier\\u00eda","pais_estado":"Australia-Colima"}', '2015-09-22 16:35:26'),
(15, 3, 9, '{"id":"9","nombre_carpeta":"Ingenieros","nombre":"Marco Aguirre \\n                                                     Ingenier\\u00eda \\n                                                     \\u00a0 \\u00a0 \\u00a0 \\u00a0 Argentina-Queretaro","carrera":"Ingenier\\u00eda","pais_estado":"argentina-queretaro"}', '2015-09-22 16:35:26'),
(16, 3, 13, '{"id":"13","nombre_carpeta":"Ingenieros","nombre":"Hugo Moran \\n                                                     Ingenier\\u00eda \\n                                                     \\u00a0 \\u00a0 \\u00a0 \\u00a0 Italia-Quintana roo","carrera":"Ingenier\\u00eda","pais_estado":"italia-quintana roo"}', '2015-09-22 16:35:26'),
(17, 3, 17, '{"id":"17","nombre_carpeta":"Ingenieros","nombre":"Ana Ramirez \\n                                                     Ingenier\\u00eda \\n                                                     \\u00a0 \\u00a0 \\u00a0 \\u00a0 Uruguay-Durango","carrera":"Ingenier\\u00eda","pais_estado":"uruguay-durango"}', '2015-09-22 16:35:26'),
(18, 3, 21, '{"id":"21","nombre_carpeta":"Ingenieros","nombre":"Anastacia Bustamante \\n                                                     Ingenier\\u00eda \\n                                                     \\u00a0 \\u00a0 \\u00a0 \\u00a0 Inglaterra-Tlaxcala","carrera":"Ingenier\\u00eda","pais_estado":"inglaterra-tlaxcala"}', '2015-09-22 16:35:26'),
(19, 3, 25, '{"id":"25","nombre_carpeta":"Ingenieros","nombre":"Guadalupe Ramos \\n                                                     Ingenier\\u00eda \\n                                                     \\u00a0 \\u00a0 \\u00a0 \\u00a0 Rusia-Chihuahua","carrera":"Ingenier\\u00eda","pais_estado":"rusia-chihuahua"}', '2015-09-22 16:35:26'),
(20, 3, 29, '{"id":"29","nombre_carpeta":"Ingenieros","nombre":"Samantha Osmos \\n                                                     Ingenier\\u00eda \\n                                                     \\u00a0 \\u00a0 \\u00a0 \\u00a0 Mexico-San luis potosi","carrera":"Ingenier\\u00eda","pais_estado":"mexico-san luis potosi"}', '2015-09-22 16:35:26'),
(21, 4, 1, '{"id":"1","nombre_carpeta":"Los 4 primeros","nombre":"Luis Garza \\n                    Gerente de medios digitales \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 Mexico-Jalisco","carrera":"Gerente de medios digitales","pais_estado":"Mexico-Jalisco"}', '2015-09-23 14:20:56'),
(22, 4, 2, '{"id":"2","nombre_carpeta":"Los 4 primeros","nombre":"Jorge Verfara \\n                    Coordinador de impuestos \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 Estados Unidos-DF","carrera":"Coordinador de impuestos","pais_estado":"Estados Unidos-DF"}', '2015-09-23 14:20:56'),
(23, 4, 3, '{"id":"3","nombre_carpeta":"Los 4 primeros","nombre":"Arturo Vasconselos \\n                    Gerente de nomina \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 Espa\\u00f1a-Morelos","carrera":"gerente de nomina","pais_estado":"Espa\\u00f1a-morelos"}', '2015-09-23 14:20:56'),
(24, 4, 4, '{"id":"4","nombre_carpeta":"Los 4 primeros","nombre":"Alejandro Magno \\n                    Director de recursos humanos \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 Colombia-Nuevo Leon","carrera":"director de recursos humanos","pais_estado":"Colombia-Nuevo Leon"}', '2015-09-23 14:20:56'),
(25, 5, 5, '{"id":"5","nombre_carpeta":"Hello","nombre":"Marco Jaimes \\n                    Director de IT \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 Australia-Colima","carrera":"Ingenier\\u00eda","pais_estado":"Australia-Colima"}', '2015-09-23 15:40:44'),
(26, 5, 6, '{"id":"6","nombre_carpeta":"Hello","nombre":"David Arteaga \\n                    Gerente de ventas \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 Nueva zelanda-Veracruz","carrera":"Licenciatura","pais_estado":"Nueva zelanda-veracruz"}', '2015-09-23 15:40:44'),
(27, 5, 7, '{"id":"7","nombre_carpeta":"Hello","nombre":"Gerardo Somas \\n                    Ejecutivo comercial \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 China-Puebla","carrera":"Maestr\\u00eda","pais_estado":"china-puebla"}', '2015-09-23 15:40:44'),
(28, 6, 1, '{"id":"1","nombre_carpeta":"Honda","nombre":"Luis Garza \\n                    Gerente de medios digitales \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 Mexico-Jalisco","carrera":"Gerente de medios digitales","pais_estado":"Mexico-Jalisco"}', '2015-09-23 15:40:58'),
(29, 6, 2, '{"id":"2","nombre_carpeta":"Honda","nombre":"Jorge Verfara \\n                    Coordinador de impuestos \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 Estados Unidos-DF","carrera":"Coordinador de impuestos","pais_estado":"Estados Unidos-DF"}', '2015-09-23 15:40:58'),
(30, 7, 1, '{"id":"1","nombre_carpeta":"IT","nombre":"Luis Garza \\n\\n                    Gerente de medios digitales \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 Mexico-Jalisco","carrera":"Gerente de medios digitales","pais_estado":"Mexico-Jalisco"}', '2015-09-23 19:08:59'),
(31, 7, 12, '{"id":"12","nombre_carpeta":"IT","nombre":"Luis Barrientos \\n\\n                    IT \\n                    \\u00a0 \\u00a0 \\u00a0 \\u00a0 Francia-Tabasco","carrera":"IT","pais_estado":"francia-tabasco"}', '2015-09-23 19:08:59');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `cliente`
--

CREATE TABLE IF NOT EXISTS `cliente` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(50) NOT NULL,
  `apellido` varchar(50) NOT NULL,
  `email` varchar(200) NOT NULL,
  `fecha` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `email` (`email`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=3 ;

--
-- Volcado de datos para la tabla `cliente`
--

INSERT INTO `cliente` (`id`, `nombre`, `apellido`, `email`, `fecha`) VALUES
(1, 'Pedro', 'Mondragon', 'pedro@gmail.com', '2015-03-27 00:19:37'),
(2, 'Jaime', 'Irazabal', 'jaimeirazabal1@gmail.com', '2015-03-27 00:26:44');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `data`
--

CREATE TABLE IF NOT EXISTS `data` (
  `id` int(2) NOT NULL AUTO_INCREMENT,
  `Nombre` varchar(9) DEFAULT NULL,
  `Nombre_2` varchar(9) DEFAULT NULL,
  `Apellido_Paterno` varchar(11) DEFAULT NULL,
  `Apellido_Materno` varchar(11) DEFAULT NULL,
  `sexo` varchar(10) DEFAULT NULL,
  `sueldo` varchar(50) NOT NULL,
  `dia_de_nacimiento` int(2) DEFAULT NULL,
  `mes_de_nacimiento` int(2) DEFAULT NULL,
  `ano_de_nacimiento` int(2) DEFAULT NULL,
  `calle` varchar(11) DEFAULT NULL,
  `numero` varchar(5) DEFAULT NULL,
  `numero_int` varchar(3) DEFAULT NULL,
  `colonia` varchar(18) DEFAULT NULL,
  `estado` varchar(19) DEFAULT NULL,
  `CP` int(5) DEFAULT NULL,
  `Pais` varchar(14) DEFAULT NULL,
  `Ultimo_puesto` varchar(28) DEFAULT NULL,
  `descripción` varchar(34) DEFAULT NULL,
  `fecha_de_ingreso` varchar(10) DEFAULT NULL,
  `fecha_de_salida` varchar(10) DEFAULT NULL,
  `Puesto_1` varchar(28) DEFAULT NULL,
  `descripción_puesto_1` varchar(34) DEFAULT NULL,
  `fecha_de_ingreso_puesto_1` varchar(10) DEFAULT NULL,
  `fecha_de_salida_puesto_1` varchar(10) DEFAULT NULL,
  `Idioma_1` varchar(10) DEFAULT NULL,
  `porcentaje_de_idioma_1` varchar(7) DEFAULT NULL,
  `Idioma_2` varchar(7) DEFAULT NULL,
  `porcentaje_de_idioma_2` varchar(3) DEFAULT NULL,
  `Idioma_3` varchar(8) DEFAULT NULL,
  `porcentaje_de_idioma_3` varchar(2) DEFAULT NULL,
  `Institucion_o_universidad` varchar(5) DEFAULT NULL,
  `Carrera_o_profesion` varchar(13) DEFAULT NULL,
  `ano_de_ingreso_universidad` int(4) DEFAULT NULL,
  `ano_de_finalización_universidad` int(4) DEFAULT NULL,
  `Curso_1` varchar(16) DEFAULT NULL,
  `Curso_2` varchar(16) DEFAULT NULL,
  `Curso_3` varchar(15) DEFAULT NULL,
  `Habilidades` varchar(34) DEFAULT NULL,
  `titulo_de_CV` varchar(10) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=31 ;

--
-- Volcado de datos para la tabla `data`
--

INSERT INTO `data` (`id`, `Nombre`, `Nombre_2`, `Apellido_Paterno`, `Apellido_Materno`, `sexo`, `sueldo`, `dia_de_nacimiento`, `mes_de_nacimiento`, `ano_de_nacimiento`, `calle`, `numero`, `numero_int`, `colonia`, `estado`, `CP`, `Pais`, `Ultimo_puesto`, `descripción`, `fecha_de_ingreso`, `fecha_de_salida`, `Puesto_1`, `descripción_puesto_1`, `fecha_de_ingreso_puesto_1`, `fecha_de_salida_puesto_1`, `Idioma_1`, `porcentaje_de_idioma_1`, `Idioma_2`, `porcentaje_de_idioma_2`, `Idioma_3`, `porcentaje_de_idioma_3`, `Institucion_o_universidad`, `Carrera_o_profesion`, `ano_de_ingreso_universidad`, `ano_de_finalización_universidad`, `Curso_1`, `Curso_2`, `Curso_3`, `Habilidades`, `titulo_de_CV`) VALUES
(1, 'Luis', 'Felipe', 'Garza', 'Hernandez', 'Hombre', '5111', 21, 11, 82, 'Zapata', '456', 'Y78', 'San agustin', 'Jalisco', 54050, 'Mexico', 'Gerente de medios digitales', 'Desarrollar campañas de marketing', '15/11/2012', '15/12/2013', 'gerente de ventas', 'Desarrollar', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '100', 'Chino', '', 'ITESM', 'Ingeniería', 1995, 2001, 'Office', 'Contable', '', 'Autoconocimiento', ''),
(2, 'Jorge', 'David', 'Verfara', 'Ramirez', 'Hombre', '15000', 1, 12, 81, 'insurgentes', '42', '', 'prado', 'DF', 45325, 'Estados Unidos', 'Coordinador de impuestos', 'Desarrollar', '16/11/2012', '16/12/2013', 'ejecutivo comercial', 'mi futuro', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'ITAM', 'Licenciatura', 1996, 2002, 'SAP', 'IT', 'Cocina', 'Empatía', ''),
(3, 'arturo', '', 'vasconselos', 'Santaana', 'Hombre', '35000', 4, 10, 79, 'reforma', '325', '', 'polanco', 'morelos', 32455, 'España', 'gerente de nomina', 'mi futuro', '17/11/2012', '17/12/2013', 'marketing', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '75', '', '50', 'UNAM', 'Abogado', 1997, 2003, 'ORACLE', 'Desarrollo', 'Taller creativo', 'Comunicación asertiva', ''),
(4, 'alejandro', 'arturo', 'Magno', 'jonas', 'Hombre', '45000', 3, 8, 78, 'juarez', '252', '', 'del valle', 'Nuevo Leon', 34532, 'Colombia', 'director de recursos humanos', 'compensar mis estudios', '18/11/2012', '18/12/2013', 'comunicaciones', 'generar valor', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '75', '', '50', 'UIA', 'Aquirquitecto', 1998, 2004, 'Six Sigma', 'Diseño gráfico', '', 'Relaciones Interpersonales', ''),
(5, 'marco', 'antonio', 'Jaimes', 'martinez', 'Hombre', '55000', 2, 3, 56, 'hidalgo', '2423', '', 'narvarte', 'Colima', 23423, 'Australia', 'director de IT', 'generar valor', '19/11/2012', '19/12/2013', 'secretaria', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'inglés', 'Materna', '', '', '', '', 'UP', 'Ingeniería', 1999, 2005, 'Inglés', '', '', 'Toma de decisiones', ''),
(6, 'david', 'jorge', 'Arteaga', 'soloasteaga', 'Hombre', '75000', 3, 5, 67, 'circuito', '5', '2', 'perisur', 'veracruz', 54387, 'Nueva zelanda', 'gerente de ventas', 'Desarrollar campañas de marketing', '20/11/2012', '20/12/2013', 'operativos', 'Desarrollar', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'UAM', 'Licenciatura', 2000, 2006, '', '', '', 'manejo de problemas y conflictos', ''),
(7, 'gerardo', 'Emilio', 'Somas', 'Ruiz', 'Hombre', '5000', 5, 6, 56, 'viaducto', '25', '', 'perinorte', 'puebla', 65421, 'china', 'ejecutivo comercial', 'Desarrollar', '21/11/2012', '21/12/2013', 'IT', 'mi futuro', '15/12/2011', '15/07/2010', 'Chino', 'Materna', 'Inglés', '50', 'Francés', '35', 'IPN', 'Maestría', 2001, 2007, 'Office', '', '', 'Pensamiento Creativo', ''),
(8, 'emmanuel', '', 'Juno', 'arteaga', 'Hombre', '3000', 6, 7, 46, 'porfirio', '4', '435', 'del valle', 'zacatecas', 12365, 'japón', 'marketing', 'mi futuro', '22/11/2012', '22/12/2013', 'TI', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Japonés', 'Materna', 'Inglés', '50', '', '', 'ITESM', 'Doctorado', 2002, 2008, 'SAP', 'Contable', '', 'Pensamiento Crítico', ''),
(9, 'Marco', 'Antonio', 'aguirre', 'pacheco', 'Hombre', '4500', 7, 1, 75, 'emiliano', '34', '', 'dorado', 'queretaro', 65434, 'argentina', 'comunicaciones', 'compensar mis estudios', '23/11/2012', '23/12/2013', 'IT', 'generar valor', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'ITAM', 'Ingeniería', 2003, 2009, 'ORACLE', 'IT', '', 'Manejo de emociones y sentimientos', ''),
(10, 'Antonio', '', 'Cortes', 'solis', 'Hombre', '5500', 8, 2, 90, 'calle 6', '32453', '4', 'napoles', 'baja california', 23454, 'brasil', 'secretaria', 'generar valor', '24/11/2012', '24/12/2013', 'TI', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'Portugués', 'Materna', '', '', '', '', 'UNAM', 'Licenciatura', 2004, 2010, 'Six Sigma', 'Desarrollo', 'Cocina', 'Manejo de tensiones y estrés', ''),
(11, 'Fredy', '', 'Rueda', 'allende', 'Hombre', '7600', 9, 3, 89, 'ciervos', '53', '', 'narvarte', 'baja california sur', 34555, 'chile', 'operativos', 'Desarrollar campañas de marketing', '25/11/2012', '25/12/2013', 'Gerente de medios digitales', 'Desarrollar', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'UIA', 'Maestría', 2005, 2011, 'Inglés', 'Taller creativo', '', 'Autoconocimiento', ''),
(12, 'Luis', '', 'Barrientos', 'hidalgo', 'Hombre', '8900', 12, 4, 87, 'calle 8', '534', '54', 'roma', 'tabasco', 34555, 'francia', 'IT', 'Desarrollar', '26/11/2012', '26/12/2013', 'Coordinador de impuestos', 'mi futuro', '15/12/2011', '15/07/2010', 'Francés', 'Materna', 'Inglés', '20', '', '20', 'UP', 'Doctorado', 2006, 2012, 'Diseño gráfico', '', '', 'Empatía', ''),
(13, 'Hugo', 'Ristedoc', 'Moran', 'pacheco', 'Hombre', '10000', 13, 5, 76, 'somasca', '43', '', 'roma sur', 'quintana roo', 76555, 'italia', 'TI', 'mi futuro', '27/11/2012', '27/12/2013', 'gerente de nomina', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Italiano', 'Materna', '', '', '', '', 'UAM', 'Ingeniería', 2007, 2013, 'Office', '', '', 'Comunicación asertiva', ''),
(14, 'Fernando', 'octavio', 'avalos', 'Rueda', 'Hombre', '11000', 25, 6, 75, 'calle 10', '', '', 'roma norte', 'chiapas', 23444, 'alemania', 'IT', 'compensar mis estudios', '28/11/2012', '28/12/2013', 'director de recursos humanos', 'generar valor', '15/12/2011', '15/07/2010', 'alemán', 'Materna', '', '', '', '', 'IPN', 'Licenciatura', 2008, 2014, 'SAP', '', '', 'Relaciones Interpersonales', ''),
(15, 'Carlos', '', 'pineda', 'Hernandez', 'Hombre', '13500', 31, 1, 85, 'barandale', '6586', '', 'Jeronimo', 'guerrero', 32434, 'egipto', 'TI', 'generar valor', '29/11/2012', '29/12/2013', 'director de IT', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'Árabe', 'Materna', '', '', '', '', 'ITESM', 'Maestría', 1990, 1991, 'ORACLE', 'Contable', '', 'Toma de decisiones', ''),
(16, 'maria', '', 'calderon', 'gomez', 'Mujer', '35500', 16, 11, 56, 'calle 12', '67', '8', 'santa fe', 'aguascalientes', 64546, 'perú', 'Relaciones públicas', 'Desarrollar campañas de marketing', '30/11/2012', '30/12/2013', 'gerente de ventas', 'Desarrollar', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'ITAM', 'Doctorado', 1991, 1992, 'Six Sigma', 'IT', '', 'manejo de problemas y conflictos', ''),
(17, 'ana', 'maria', 'ramirez', 'garza', 'Mujer', '25000', 15, 11, 67, 'encino', '86', '', 'polanco', 'durango', 45745, 'uruguay', 'Jefe de planeación digital', 'Desarrollar', '01/12/2012', '31/12/2013', 'ejecutivo comercial', 'mi futuro', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '35', 'Francés', '15', 'UNAM', 'Ingeniería', 1992, 1993, 'Inglés', 'Desarrollo', '', 'Pensamiento Creativo', ''),
(18, 'davina', '', 'pacheco', 'serralde', 'Mujer', '30000', 14, 10, 68, 'calle 14', '867', '', 'anzures', 'tamaulipas', 45748, 'paraguay', 'Diseño gráfico', 'mi futuro', '02/12/2012', '01/01/2014', 'marketing', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '100', '', '', 'UIA', 'Licenciatura', 1993, 1994, 'Cocina', '', '', 'Pensamiento Crítico', ''),
(19, 'sandra', '', 'lebrija', 'huerta', 'Mujer', '27250', 28, 12, 68, 'caoba', '', '', 'sta monica', 'morelos', 76575, 'bolivia', 'actuario', 'compensar mis estudios', '03/12/2012', '02/01/2014', 'comunicaciones', 'generar valor', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'UP', 'Maestría', 1994, 1995, 'Office', 'Taller creativo', '', 'Manejo de emociones y sentimientos', ''),
(20, 'gabriela', 'sofia', 'sanchez', 'segura', 'Mujer', '15000', 2, 12, 79, 'calle 16', '', '78', 'calacoaya', 'Guanajuato', 23423, 'portugal', 'Director General', 'generar valor', '04/12/2012', '03/01/2014', 'secretaria', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'Portugués', 'Materna', '', '', '', '', 'UAM', 'Doctorado', 1995, 1996, 'SAP', 'Diseño gráfico', '', 'Manejo de tensiones y estrés', ''),
(21, 'anastacia', '', 'bustamante', 'villamour', 'Mujer', '55000', 5, 4, 72, 'cacao', '76', '', 'jardines', 'tlaxcala', 23421, 'inglaterra', 'Director de Ventas', 'Desarrollar campañas de marketing', '05/12/2012', '04/01/2014', 'operativos', 'Desarrollar', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'IPN', 'Ingeniería', 1996, 1997, 'Six Sigma', '', '', 'Autoconocimiento', ''),
(22, 'mariana', '', 'lopez', 'navarro', 'Mujer', '75000', 27, 5, 74, 'calle 18', '6', '', 'coyotepec', 'mexico', 23444, 'UK', 'analista de marketing', 'Desarrollar', '06/12/2012', '05/01/2014', 'IT', 'mi futuro', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'ITESM', 'Licenciatura', 1997, 1998, 'Inglés', 'Contable', '', 'Empatía', ''),
(23, 'Pamela', '', 'Gomez', 'tejerina', 'Mujer', '5000', 26, 6, 74, 'limones', '7', '8', 'jazmines', 'michoacan', 65464, 'holanda', 'analista de ventas', 'mi futuro', '07/12/2012', '06/01/2014', 'TI', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'ITAM', 'Maestría', 1998, 1999, 'IT', '', '', 'Comunicación asertiva', ''),
(24, 'isela', '', 'Pérez', 'petrone', 'Mujer', '3000', 6, 7, 75, 'calle 20', '86', '7', 'riviera', 'campeche', 75655, 'sudafrica', 'analista de negocios', 'compensar mis estudios', '08/12/2012', '07/01/2014', 'IT', 'generar valor', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'UNAM', 'Doctorado', 1999, 2000, 'Office', 'Desarrollo', '', 'Relaciones Interpersonales', ''),
(25, 'guadalupe', 'magdalena', 'Ramos', 'caraveo', 'Mujer', '4500', 9, 9, 76, 'fresas', '678', '867', 'maya', 'chihuahua', 65756, 'rusia', 'analista de competencia', 'generar valor', '09/12/2012', '08/01/2014', 'TI', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'Ruso', 'Materna', '', '', '', '', 'UIA', 'Ingeniería', 2000, 2001, 'SAP', '', '', 'Toma de decisiones', ''),
(26, 'Ortencia', '', 'Ramirez', 'lopez', 'Mujer', '5500', 25, 7, 68, 'calle 22', '6', '9', 'contreras', 'hidalgo', 98777, 'Mexico', 'analista de nómina', 'Desarrollar campañas de marketing', '10/12/2012', '09/01/2014', 'Relaciones públicas', 'Desarrollar', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'UP', 'Licenciatura', 2001, 2002, 'Six Sigma', 'Cocina', '', 'manejo de problemas y conflictos', ''),
(27, 'jazmin', '', 'Olmos', 'juarez', 'Mujer', '7600', 2, 3, 68, 'linares', '867', '86', 'doctores', 'Sonora', 23444, 'mexico', 'reclutamiento y selección', 'Desarrollar', '11/12/2012', '10/01/2014', 'Jefe de planeación digital', 'mi futuro', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '70', 'Chino', '50', 'UAM', 'Maestría', 2002, 2003, 'Inglés', 'Taller creativo', '', 'Pensamiento Creativo', ''),
(28, 'Oyuki', '', 'Turribiates', 'sevilla', 'Mujer', '8900', 25, 1, 98, 'calle 24', '25', '5', 'irrigacación', 'sinaloa', 22254, 'Mexico', 'reclutamiento y selección', 'mi futuro', '12/12/2012', '11/01/2014', 'Gerente de medios digitales', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '65', '', '', 'IPN', 'Doctorado', 2003, 2004, 'Diseño gráfico', '', '', 'Pensamiento Crítico', ''),
(29, 'Samantha', 'deyanira', 'Osmos', 'mancera', 'Mujer', '55000', 3, 7, 92, 'cosmos', '25', '34', 'aeropuerto central', 'san luis potosi', 43333, 'mexico', 'reclutamiento y selección', 'compensar mis estudios', '13/12/2012', '12/01/2014', 'Coordinador de impuestos', 'generar valor', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'ITESM', 'Ingeniería', 2004, 2005, 'Office', 'Contable', '', 'Manejo de emociones y sentimientos', ''),
(30, 'Berenice', '', 'Mota', 'garcia', 'Mujer', '75000', 25, 2, 29, 'venados', '453', '', 'colmena', 'oaxaca', 11111, 'mexico', 'reclutamiento y selección', 'generar valor', '14/12/2012', '13/01/2014', 'gerente de nomina', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '90', 'Chino', '50', 'ITAM', 'Licenciatura', 2005, 2006, 'SAP', 'IT', '', 'Manejo de tensiones y estrés', '');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `filtro`
--

CREATE TABLE IF NOT EXISTS `filtro` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `filtro` text COLLATE utf8_unicode_ci NOT NULL,
  `usuario_id` int(11) NOT NULL,
  `nombre_filtro` varchar(100) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `pedido`
--

CREATE TABLE IF NOT EXISTS `pedido` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `cliente_id` int(11) NOT NULL,
  `fecha_comienzo` datetime NOT NULL,
  `fecha_termino` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=3 ;

--
-- Volcado de datos para la tabla `pedido`
--

INSERT INTO `pedido` (`id`, `cliente_id`, `fecha_comienzo`, `fecha_termino`) VALUES
(1, 1, '2015-03-26 08:46:00', '2015-03-26 08:46:00'),
(2, 2, '2015-03-26 08:46:00', NULL);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `requerimiento`
--

CREATE TABLE IF NOT EXISTS `requerimiento` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `pedido_id` int(11) NOT NULL,
  `descripcion` text NOT NULL,
  `hecho` tinyint(1) DEFAULT NULL,
  `precio` varchar(50) DEFAULT NULL,
  `fecha` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=13 ;

--
-- Volcado de datos para la tabla `requerimiento`
--

INSERT INTO `requerimiento` (`id`, `pedido_id`, `descripcion`, `hecho`, `precio`, `fecha`) VALUES
(6, 1, 'Piotreck 19/05/15:\n\nEn ventas/modulo de ventas/ventas -> nuevo -> \nen la búsqueda de productos que salga el nombre del producto y la cantidad en stock\n(cabe destacar que cada producto se divide por empresa y almacen, por lo que es notable la busqueda en esas tablas) \n\nPRECIO 40$\n\nCREO QUE YA ESTA HECHO', 1, '40$', '2015-05-20 04:30:00'),
(7, 1, 'Piotreck 19/05/15:\n\ncolocar fecha de vencimiento en el reporte "CONSECUTIVO DE REMISIONES" ubicado en reportes/modulo ventas/ reporte documentos\nparece que algunas veces hay que calcular la fecha de vencimiento con los dias de plazo que se le asigna...\n\nPRECIO: 50$\n\n CREO QUE YA ESTA HECHO', 1, '50$', '2015-05-20 04:30:00'),
(8, 1, 'al mater una cadena en el campo de producto, el autocomplete no me arroja la cadena que ingrese ya sea por nombre o codigo\r\n-------------\r\nQue me arroje la cadena ya sea por nombre o codigo en el autocomplete\r\n', 1, '30$', '2015-06-10 12:28:06'),
(9, 1, 'Al cargar la seccion el select por default es COTIZACION\r\n---------------------------\r\nEl select por default sea remision, asi como los documentos que se cargan al tener el respectivo select (30)', 1, '30$', '2015-06-10 12:28:34'),
(10, 1, 'Al ingresar todos los datos cliente y productos, despues al darle guardar y seleccionar una forma de pago me salga la opcion de imprimir el documento\r\n--------------------------------------------------------------------------\r\nNecesito poder imprimir el documento que se genero. Actualmente se genera ese documento desde Ventas/ventas/ y seleccionas cotizacion, remision o factura y le das click al boton de pdf y ya hace todo el proceso para imprimir, pues necesito eso despues de la opcion de FORMA DE PAGO, en ticket ya esta, pero solo la impresión del ticket y necesito tambien la version completa del documento\r\n', 1, '50$', '2015-06-10 12:29:11'),
(11, 1, 'en el ventas/ventas cuando se agregar una cantidad de productos y se elimina uno, los descuentos en la tabla de productos, se colocan en cero despues de eliminar algun producto.', 1, '30$', '2015-06-10 12:30:26'),
(12, 1, 'acomoda las peticiones ajax cada 5segundos', NULL, '50$', '2015-06-10 12:30:42');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `role`
--

CREATE TABLE IF NOT EXISTS `role` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role` varchar(100) COLLATE utf8_unicode_ci NOT NULL,
  `descripcion` text COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=2 ;

--
-- Volcado de datos para la tabla `role`
--

INSERT INTO `role` (`id`, `role`, `descripcion`) VALUES
(1, 'Administrador', 'administra y tiene acceso a todo');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `TABLE 4`
--

CREATE TABLE IF NOT EXISTS `TABLE 4` (
  `id` int(2) DEFAULT NULL,
  `Nombre` varchar(9) DEFAULT NULL,
  `Nombre_2` varchar(9) DEFAULT NULL,
  `Apellido_Paterno` varchar(11) DEFAULT NULL,
  `Apellido_Materno` varchar(11) DEFAULT NULL,
  `dia_de_nacimiento` int(2) DEFAULT NULL,
  `mes_de_nacimiento` int(2) DEFAULT NULL,
  `ano_de_nacimiento` int(2) DEFAULT NULL,
  `calle` varchar(11) DEFAULT NULL,
  `numero` varchar(5) DEFAULT NULL,
  `numero_int` varchar(3) DEFAULT NULL,
  `colonia` varchar(18) DEFAULT NULL,
  `estado` varchar(19) DEFAULT NULL,
  `CP` int(5) DEFAULT NULL,
  `Pais` varchar(14) DEFAULT NULL,
  `Ultimo_puesto` varchar(28) DEFAULT NULL,
  `descripción` varchar(34) DEFAULT NULL,
  `fecha_de_ingreso` varchar(10) DEFAULT NULL,
  `fecha_de_salida` varchar(10) DEFAULT NULL,
  `Puesto_1` varchar(28) DEFAULT NULL,
  `descripción_puesto_1` varchar(34) DEFAULT NULL,
  `fecha_de_ingreso_puesto_1` varchar(10) DEFAULT NULL,
  `fecha_de_salida_puesto_1` varchar(10) DEFAULT NULL,
  `Idioma_1` varchar(10) DEFAULT NULL,
  `porcentaje_de_idioma_1` varchar(7) DEFAULT NULL,
  `Idioma_2` varchar(7) DEFAULT NULL,
  `porcentaje_de_idioma_2` varchar(3) DEFAULT NULL,
  `Idioma_3` varchar(8) DEFAULT NULL,
  `porcentaje_de_idioma_3` varchar(2) DEFAULT NULL,
  `Institucion_o_universidad` varchar(5) DEFAULT NULL,
  `Carrera_o_profesion` varchar(13) DEFAULT NULL,
  `ano_de_ingreso_universidad` int(4) DEFAULT NULL,
  `ano_de_finalización_universidad` int(4) DEFAULT NULL,
  `Curso_1` varchar(16) DEFAULT NULL,
  `Curso_2` varchar(16) DEFAULT NULL,
  `Curso_3` varchar(15) DEFAULT NULL,
  `Habilidades` varchar(34) DEFAULT NULL,
  `titulo_de_CV` varchar(10) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Volcado de datos para la tabla `TABLE 4`
--

INSERT INTO `TABLE 4` (`id`, `Nombre`, `Nombre_2`, `Apellido_Paterno`, `Apellido_Materno`, `dia_de_nacimiento`, `mes_de_nacimiento`, `ano_de_nacimiento`, `calle`, `numero`, `numero_int`, `colonia`, `estado`, `CP`, `Pais`, `Ultimo_puesto`, `descripción`, `fecha_de_ingreso`, `fecha_de_salida`, `Puesto_1`, `descripción_puesto_1`, `fecha_de_ingreso_puesto_1`, `fecha_de_salida_puesto_1`, `Idioma_1`, `porcentaje_de_idioma_1`, `Idioma_2`, `porcentaje_de_idioma_2`, `Idioma_3`, `porcentaje_de_idioma_3`, `Institucion_o_universidad`, `Carrera_o_profesion`, `ano_de_ingreso_universidad`, `ano_de_finalización_universidad`, `Curso_1`, `Curso_2`, `Curso_3`, `Habilidades`, `titulo_de_CV`) VALUES
(1, 'Luis', 'Felipe', 'Garza', 'Hernandez', 21, 11, 82, 'Zapata', '456', 'Y78', 'San agustin', 'Jalisco', 54050, 'Mexico', 'Gerente de medios digitales', 'Desarrollar campañas de marketing', '15/11/2012', '15/12/2013', 'gerente de ventas', 'Desarrollar', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '100', 'Chino', '', 'ITESM', 'Ingeniería', 1995, 2001, 'Office', 'Contable', '', 'Autoconocimiento', ''),
(2, 'Jorge', 'David', 'Verfara', 'Ramirez', 1, 12, 81, 'insurgentes', '42', '', 'prado', 'DF', 45325, 'Estados Unidos', 'Coordinador de impuestos', 'Desarrollar', '16/11/2012', '16/12/2013', 'ejecutivo comercial', 'mi futuro', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'ITAM', 'Licenciatura', 1996, 2002, 'SAP', 'IT', 'Cocina', 'Empatía', ''),
(3, 'arturo', '', 'vasconselos', 'Santaana', 4, 10, 79, 'reforma', '325', '', 'polanco', 'morelos', 32455, 'España', 'gerente de nomina', 'mi futuro', '17/11/2012', '17/12/2013', 'marketing', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '75', '', '50', 'UNAM', 'Abogado', 1997, 2003, 'ORACLE', 'Desarrollo', 'Taller creativo', 'Comunicación asertiva', ''),
(4, 'alejandro', 'arturo', 'Magno', 'jonas', 3, 8, 78, 'juarez', '252', '', 'del valle', 'Nuevo Leon', 34532, 'Colombia', 'director de recursos humanos', 'compensar mis estudios', '18/11/2012', '18/12/2013', 'comunicaciones', 'generar valor', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '75', '', '50', 'UIA', 'Aquirquitecto', 1998, 2004, 'Six Sigma', 'Diseño gráfico', '', 'Relaciones Interpersonales', ''),
(5, 'marco', 'antonio', 'Jaimes', 'martinez', 2, 3, 56, 'hidalgo', '2423', '', 'narvarte', 'Colima', 23423, 'Australia', 'director de IT', 'generar valor', '19/11/2012', '19/12/2013', 'secretaria', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'inglés', 'Materna', '', '', '', '', 'UP', 'Ingeniería', 1999, 2005, 'Inglés', '', '', 'Toma de decisiones', ''),
(6, 'david', 'jorge', 'Arteaga', 'soloasteaga', 3, 5, 67, 'circuito', '5', '2', 'perisur', 'veracruz', 54387, 'Nueva zelanda', 'gerente de ventas', 'Desarrollar campañas de marketing', '20/11/2012', '20/12/2013', 'operativos', 'Desarrollar', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'UAM', 'Licenciatura', 2000, 2006, '', '', '', 'manejo de problemas y conflictos', ''),
(7, 'gerardo', 'Emilio', 'Somas', 'Ruiz', 5, 6, 56, 'viaducto', '25', '', 'perinorte', 'puebla', 65421, 'china', 'ejecutivo comercial', 'Desarrollar', '21/11/2012', '21/12/2013', 'IT', 'mi futuro', '15/12/2011', '15/07/2010', 'Chino', 'Materna', 'Inglés', '50', 'Francés', '35', 'IPN', 'Maestría', 2001, 2007, 'Office', '', '', 'Pensamiento Creativo', ''),
(8, 'emmanuel', '', 'Juno', 'arteaga', 6, 7, 46, 'porfirio', '4', '435', 'del valle', 'zacatecas', 12365, 'japón', 'marketing', 'mi futuro', '22/11/2012', '22/12/2013', 'TI', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Japonés', 'Materna', 'Inglés', '50', '', '', 'ITESM', 'Doctorado', 2002, 2008, 'SAP', 'Contable', '', 'Pensamiento Crítico', ''),
(9, 'Marco', 'Antonio', 'aguirre', 'pacheco', 7, 1, 75, 'emiliano', '34', '', 'dorado', 'queretaro', 65434, 'argentina', 'comunicaciones', 'compensar mis estudios', '23/11/2012', '23/12/2013', 'IT', 'generar valor', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'ITAM', 'Ingeniería', 2003, 2009, 'ORACLE', 'IT', '', 'Manejo de emociones y sentimientos', ''),
(10, 'Antonio', '', 'Cortes', 'solis', 8, 2, 90, 'calle 6', '32453', '4', 'napoles', 'baja california', 23454, 'brasil', 'secretaria', 'generar valor', '24/11/2012', '24/12/2013', 'TI', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'Portugués', 'Materna', '', '', '', '', 'UNAM', 'Licenciatura', 2004, 2010, 'Six Sigma', 'Desarrollo', 'Cocina', 'Manejo de tensiones y estrés', ''),
(11, 'Fredy', '', 'Rueda', 'allende', 9, 3, 89, 'ciervos', '53', '', 'narvarte', 'baja california sur', 34555, 'chile', 'operativos', 'Desarrollar campañas de marketing', '25/11/2012', '25/12/2013', 'Gerente de medios digitales', 'Desarrollar', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'UIA', 'Maestría', 2005, 2011, 'Inglés', 'Taller creativo', '', 'Autoconocimiento', ''),
(12, 'Luis', '', 'Barrientos', 'hidalgo', 12, 4, 87, 'calle 8', '534', '54', 'roma', 'tabasco', 34555, 'francia', 'IT', 'Desarrollar', '26/11/2012', '26/12/2013', 'Coordinador de impuestos', 'mi futuro', '15/12/2011', '15/07/2010', 'Francés', 'Materna', 'Inglés', '20', '', '20', 'UP', 'Doctorado', 2006, 2012, 'Diseño gráfico', '', '', 'Empatía', ''),
(13, 'Hugo', 'Ristedoc', 'Moran', 'pacheco', 13, 5, 76, 'somasca', '43', '', 'roma sur', 'quintana roo', 76555, 'italia', 'TI', 'mi futuro', '27/11/2012', '27/12/2013', 'gerente de nomina', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Italiano', 'Materna', '', '', '', '', 'UAM', 'Ingeniería', 2007, 2013, 'Office', '', '', 'Comunicación asertiva', ''),
(14, 'Fernando', 'octavio', 'avalos', 'Rueda', 25, 6, 75, 'calle 10', '', '', 'roma norte', 'chiapas', 23444, 'alemania', 'IT', 'compensar mis estudios', '28/11/2012', '28/12/2013', 'director de recursos humanos', 'generar valor', '15/12/2011', '15/07/2010', 'alemán', 'Materna', '', '', '', '', 'IPN', 'Licenciatura', 2008, 2014, 'SAP', '', '', 'Relaciones Interpersonales', ''),
(15, 'Carlos', '', 'pineda', 'Hernandez', 31, 1, 85, 'barandale', '6586', '', 'Jeronimo', 'guerrero', 32434, 'egipto', 'TI', 'generar valor', '29/11/2012', '29/12/2013', 'director de IT', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'Árabe', 'Materna', '', '', '', '', 'ITESM', 'Maestría', 1990, 1991, 'ORACLE', 'Contable', '', 'Toma de decisiones', ''),
(16, 'maria', '', 'calderon', 'gomez', 16, 11, 56, 'calle 12', '67', '8', 'santa fe', 'aguascalientes', 64546, 'perú', 'Relaciones públicas', 'Desarrollar campañas de marketing', '30/11/2012', '30/12/2013', 'gerente de ventas', 'Desarrollar', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'ITAM', 'Doctorado', 1991, 1992, 'Six Sigma', 'IT', '', 'manejo de problemas y conflictos', ''),
(17, 'ana', 'maria', 'ramirez', 'garza', 15, 11, 67, 'encino', '86', '', 'polanco', 'durango', 45745, 'uruguay', 'Jefe de planeación digital', 'Desarrollar', '01/12/2012', '31/12/2013', 'ejecutivo comercial', 'mi futuro', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '35', 'Francés', '15', 'UNAM', 'Ingeniería', 1992, 1993, 'Inglés', 'Desarrollo', '', 'Pensamiento Creativo', ''),
(18, 'davina', '', 'pacheco', 'serralde', 14, 10, 68, 'calle 14', '867', '', 'anzures', 'tamaulipas', 45748, 'paraguay', 'Diseño gráfico', 'mi futuro', '02/12/2012', '01/01/2014', 'marketing', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '100', '', '', 'UIA', 'Licenciatura', 1993, 1994, 'Cocina', '', '', 'Pensamiento Crítico', ''),
(19, 'sandra', '', 'lebrija', 'huerta', 28, 12, 68, 'caoba', '', '', 'sta monica', 'morelos', 76575, 'bolivia', 'actuario', 'compensar mis estudios', '03/12/2012', '02/01/2014', 'comunicaciones', 'generar valor', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'UP', 'Maestría', 1994, 1995, 'Office', 'Taller creativo', '', 'Manejo de emociones y sentimientos', ''),
(20, 'gabriela', 'sofia', 'sanchez', 'segura', 2, 12, 79, 'calle 16', '', '78', 'calacoaya', 'Guanajuato', 23423, 'portugal', 'Director General', 'generar valor', '04/12/2012', '03/01/2014', 'secretaria', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'Portugués', 'Materna', '', '', '', '', 'UAM', 'Doctorado', 1995, 1996, 'SAP', 'Diseño gráfico', '', 'Manejo de tensiones y estrés', ''),
(21, 'anastacia', '', 'bustamante', 'villamour', 5, 4, 72, 'cacao', '76', '', 'jardines', 'tlaxcala', 23421, 'inglaterra', 'Director de Ventas', 'Desarrollar campañas de marketing', '05/12/2012', '04/01/2014', 'operativos', 'Desarrollar', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'IPN', 'Ingeniería', 1996, 1997, 'Six Sigma', '', '', 'Autoconocimiento', ''),
(22, 'mariana', '', 'lopez', 'navarro', 27, 5, 74, 'calle 18', '6', '', 'coyotepec', 'mexico', 23444, 'UK', 'analista de marketing', 'Desarrollar', '06/12/2012', '05/01/2014', 'IT', 'mi futuro', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'ITESM', 'Licenciatura', 1997, 1998, 'Inglés', 'Contable', '', 'Empatía', ''),
(23, 'Pamela', '', 'Gomez', 'tejerina', 26, 6, 74, 'limones', '7', '8', 'jazmines', 'michoacan', 65464, 'holanda', 'analista de ventas', 'mi futuro', '07/12/2012', '06/01/2014', 'TI', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'ITAM', 'Maestría', 1998, 1999, 'IT', '', '', 'Comunicación asertiva', ''),
(24, 'isela', '', 'Pérez', 'petrone', 6, 7, 75, 'calle 20', '86', '7', 'riviera', 'campeche', 75655, 'sudafrica', 'analista de negocios', 'compensar mis estudios', '08/12/2012', '07/01/2014', 'IT', 'generar valor', '15/12/2011', '15/07/2010', 'Inglés', 'Materna', '', '', '', '', 'UNAM', 'Doctorado', 1999, 2000, 'Office', 'Desarrollo', '', 'Relaciones Interpersonales', ''),
(25, 'guadalupe', 'magdalena', 'Ramos', 'caraveo', 9, 9, 76, 'fresas', '678', '867', 'maya', 'chihuahua', 65756, 'rusia', 'analista de competencia', 'generar valor', '09/12/2012', '08/01/2014', 'TI', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'Ruso', 'Materna', '', '', '', '', 'UIA', 'Ingeniería', 2000, 2001, 'SAP', '', '', 'Toma de decisiones', ''),
(26, 'Ortencia', '', 'Ramirez', 'lopez', 25, 7, 68, 'calle 22', '6', '9', 'contreras', 'hidalgo', 98777, 'Mexico', 'analista de nómina', 'Desarrollar campañas de marketing', '10/12/2012', '09/01/2014', 'Relaciones públicas', 'Desarrollar', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'UP', 'Licenciatura', 2001, 2002, 'Six Sigma', 'Cocina', '', 'manejo de problemas y conflictos', ''),
(27, 'jazmin', '', 'Olmos', 'juarez', 2, 3, 68, 'linares', '867', '86', 'doctores', 'Sonora', 23444, 'mexico', 'reclutamiento y selección', 'Desarrollar', '11/12/2012', '10/01/2014', 'Jefe de planeación digital', 'mi futuro', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '70', 'Chino', '50', 'UAM', 'Maestría', 2002, 2003, 'Inglés', 'Taller creativo', '', 'Pensamiento Creativo', ''),
(28, 'Oyuki', '', 'Turribiates', 'sevilla', 25, 1, 98, 'calle 24', '25', '5', 'irrigacación', 'sinaloa', 22254, 'Mexico', 'reclutamiento y selección', 'mi futuro', '12/12/2012', '11/01/2014', 'Gerente de medios digitales', 'compensar mis estudios', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '65', '', '', 'IPN', 'Doctorado', 2003, 2004, 'Diseño gráfico', '', '', 'Pensamiento Crítico', ''),
(29, 'Samantha', 'deyanira', 'Osmos', 'mancera', 3, 7, 92, 'cosmos', '25', '34', 'aeropuerto central', 'san luis potosi', 43333, 'mexico', 'reclutamiento y selección', 'compensar mis estudios', '13/12/2012', '12/01/2014', 'Coordinador de impuestos', 'generar valor', '15/12/2011', '15/07/2010', 'Español', 'Materna', '', '', '', '', 'ITESM', 'Ingeniería', 2004, 2005, 'Office', 'Contable', '', 'Manejo de emociones y sentimientos', ''),
(30, 'Berenice', '', 'Mota', 'garcia', 25, 2, 29, 'venados', '453', '', 'colmena', 'oaxaca', 11111, 'mexico', 'reclutamiento y selección', 'generar valor', '14/12/2012', '13/01/2014', 'gerente de nomina', 'Desarrollar campañas de marketing', '15/12/2011', '15/07/2010', 'Español', 'Materna', 'Inglés', '90', 'Chino', '50', 'ITAM', 'Licenciatura', 2005, 2006, 'SAP', 'IT', '', 'Manejo de tensiones y estrés', '');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `usuario`
--

CREATE TABLE IF NOT EXISTS `usuario` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `login` varchar(100) COLLATE utf8_unicode_ci NOT NULL,
  `password` varchar(200) COLLATE utf8_unicode_ci NOT NULL,
  `role_id` int(11) DEFAULT NULL,
  `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=4 ;

--
-- Volcado de datos para la tabla `usuario`
--

INSERT INTO `usuario` (`id`, `login`, `password`, `role_id`, `created`) VALUES
(1, 'jaimeirazabal1@gmail.com', '7d3ff5e583a1727c07bd911d427b514b', 1, '2015-09-21 14:02:06'),
(2, 'pepe@gmail.com', '926e27eecdbc7a18858b3798ba99bddd', NULL, '2015-09-21 14:46:13'),
(3, 'luisgarza', 'fa83478f3b0ddee54e978e5e4b52e00b', NULL, '2015-09-23 17:01:44');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;