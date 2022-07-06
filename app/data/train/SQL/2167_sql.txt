-- phpMyAdmin SQL Dump
-- version 4.5.1
-- http://www.phpmyadmin.net
--
-- Servidor: 127.0.0.1
-- Tiempo de generación: 28-11-2016 a las 20:52:11
-- Versión del servidor: 10.1.16-MariaDB
-- Versión de PHP: 5.5.38

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Base de datos: `yii2advanced`
--

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `auth_assignment`
--

CREATE TABLE `auth_assignment` (
  `item_name` varchar(64) COLLATE utf8_unicode_ci NOT NULL,
  `user_id` varchar(64) COLLATE utf8_unicode_ci NOT NULL,
  `created_at` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `auth_assignment`
--

INSERT INTO `auth_assignment` (`item_name`, `user_id`, `created_at`) VALUES
('admin', '1', 1478400017),
('autor', '1', 1478400017),
('autor', '2', 1478400038),
('autor', '3', 1478400052),
('autor', '6', 1478477158),
('delete', '1', 1478400017),
('delete', '2', 1478400038),
('delete', '3', 1478400052),
('update', '1', 1478400017),
('update', '2', 1478400038),
('update', '3', 1478400052);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `auth_item`
--

CREATE TABLE `auth_item` (
  `name` varchar(64) COLLATE utf8_unicode_ci NOT NULL,
  `type` int(11) NOT NULL,
  `description` text COLLATE utf8_unicode_ci,
  `rule_name` varchar(64) COLLATE utf8_unicode_ci DEFAULT NULL,
  `data` text COLLATE utf8_unicode_ci,
  `created_at` int(11) DEFAULT NULL,
  `updated_at` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `auth_item`
--

INSERT INTO `auth_item` (`name`, `type`, `description`, `rule_name`, `data`, `created_at`, `updated_at`) VALUES
('admin', 1, 'administrador', NULL, NULL, 1478399905, 1478399905),
('autor', 1, 'autor', NULL, NULL, 1478399916, 1478399916),
('delete', 2, 'Eliminar Entradas', NULL, NULL, 1478399985, 1478399985),
('update', 2, 'Actualizar entradas', NULL, NULL, 1478399971, 1478399971);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `auth_item_child`
--

CREATE TABLE `auth_item_child` (
  `parent` varchar(64) COLLATE utf8_unicode_ci NOT NULL,
  `child` varchar(64) COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `auth_rule`
--

CREATE TABLE `auth_rule` (
  `name` varchar(64) COLLATE utf8_unicode_ci NOT NULL,
  `data` text COLLATE utf8_unicode_ci,
  `created_at` int(11) DEFAULT NULL,
  `updated_at` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `blog_cms`
--

CREATE TABLE `blog_cms` (
  `Id` int(11) NOT NULL,
  `Titulo` varchar(50) NOT NULL,
  `Contenido` text NOT NULL,
  `Publicar` tinyint(1) NOT NULL,
  `Autor` varchar(50) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Volcado de datos para la tabla `blog_cms`
--

INSERT INTO `blog_cms` (`Id`, `Titulo`, `Contenido`, `Publicar`, `Autor`) VALUES
(17, 'SOFTWARE QA', '<div class="card-content">\r\n<p align="justify">Este documento describe una familia de herramientas que no s&oacute;lo es compatible desarrollo de software, sino que tambi&eacute;n asegura la calidad de cada software producto de los requisitos definici&oacute;n al sistema integrado..</p>\r\n<p align="justify"><a href="https://www.computer.org/csdl/trans/ts/1985/09/01702108.pdf" target="_blank">Leer M&aacute;s</a></p>\r\n</div>', 1, ''),
(18, 'TESTING', '<div class="card-content">\r\n<p align="justify">El presente trabajo describe el proceso realizado y resultados obtenidos al estudiar la documentaci&oacute;n t&eacute;cnica de los desarrollos de software (DS) del grupo de investigaci&oacute;n en sistemas y tecnolog&iacute;as de la informaci&oacute;n (STI), grupo adscrito a la UIS.</p>\r\n<p align="justify"><a title="Leer M&aacute;s" href="http://dspace.ucuenca.edu.ec/bitstream/123456789/23835/1/2015_TIC.EC_24.pdf" target="_blank">Leer M&aacute;s</a></p>\r\n</div>', 1, ''),
(19, 'SELENIUM', '<div class="card-content">\r\n<p align="justify">El selenio es una herramienta para la creaci&oacute;n y ejecuci&oacute;n de pruebas web automatizados y es una buena opci&oacute;n para proyectos &aacute;giles en el que se puede utilizar para la creaci&oacute;n de pruebas de aceptaci&oacute;n correspondientes a las historias de usuario de la aplicaci&oacute;n web.</p>\r\n<p align="justify"><a title="Leer M&agrave;s" href="http://dl.acm.org/citation.cfm?id=1297927&amp;dl=ACM&amp;coll=DL&amp;CFID=691175237&amp;CFTOKEN=71190589" target="_blank">Leer M&aacute;s</a></p>\r\n</div>', 1, ''),
(20, 'MVC', '<div class="card-content">\r\n<p align="justify">Este art&iacute;culo presenta un sistema flexible de la miner&iacute;a construida sobre una la arquitectura de varios niveles. La arquitectura del sistema es dise&ntilde;ada en el patr&oacute;n de dise&ntilde;o Modelo-Vista-Controlador.</p>\r\n<p align="justify"><a title="Leer M&aacute;s" href="https://www.computer.org/csdl/proceedings/hicss/2005/2268/03/22680073c.pdf" target="_blank">Leer M&aacute;s</a></p>\r\n</div>', 1, ''),
(21, 'HTML5', '<div class="card-content">\r\n<p align="justify">Se presenta un modelo que permite integrar directamente los nodos X3D en contenido HTML5 DOM. Este modelo trata de cumplir la promesa de la especificaci&oacute;n de HTML 5, que hace referencia a X3D.</p>\r\n<p align="justify"><a title="Leer M&aacute;s" href="http://dl.acm.org/citation.cfm?id=1559784&amp;dl=ACM&amp;coll=DL&amp;CFID=691175237&amp;CFTOKEN=71190589" target="_blank">Leer M&aacute;s</a></p>\r\n</div>', 1, ''),
(22, 'hola', '<p>skdasd</p>', 1, 'wendy');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `migration`
--

CREATE TABLE `migration` (
  `version` varchar(180) NOT NULL,
  `apply_time` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Volcado de datos para la tabla `migration`
--

INSERT INTO `migration` (`version`, `apply_time`) VALUES
('m000000_000000_base', 1478385738),
('m140506_102106_rbac_init', 1478385944);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `profile`
--

CREATE TABLE `profile` (
  `user_id` int(11) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `public_email` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `gravatar_email` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `gravatar_id` varchar(32) COLLATE utf8_unicode_ci DEFAULT NULL,
  `location` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `website` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `bio` text COLLATE utf8_unicode_ci,
  `timezone` varchar(40) COLLATE utf8_unicode_ci DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `profile`
--

INSERT INTO `profile` (`user_id`, `name`, `public_email`, `gravatar_email`, `gravatar_id`, `location`, `website`, `bio`, `timezone`) VALUES
(1, 'Wendy Paola Estupiñan', 'wendy.paola.estupinan@gmail.com', 'wendy.paola.estupinan@gmail.com', '767630de44edbefadda0d2357b62e738', 'Esmeraldas - Atacames', 'http://www.mundogya.com/', '', 'Pacific/Apia'),
(2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
(7, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `social_account`
--

CREATE TABLE `social_account` (
  `id` int(11) NOT NULL,
  `user_id` int(11) DEFAULT NULL,
  `provider` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `client_id` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `data` text COLLATE utf8_unicode_ci,
  `code` varchar(32) COLLATE utf8_unicode_ci DEFAULT NULL,
  `created_at` int(11) DEFAULT NULL,
  `email` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `username` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `token`
--

CREATE TABLE `token` (
  `user_id` int(11) NOT NULL,
  `code` varchar(32) COLLATE utf8_unicode_ci NOT NULL,
  `created_at` int(11) NOT NULL,
  `type` smallint(6) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `token`
--

INSERT INTO `token` (`user_id`, `code`, `created_at`, `type`) VALUES
(1, '4Ciq_YWlyYEZs9BHlcL32pvQBXk7nwdB', 1478301133, 0),
(2, '8sN_eLHwxsC9pyYR-lgHPHdYah9KN8xn', 1478364095, 0),
(7, 'pHrZzrS7yDGclrzwNc8_wc1vCxFlnhN7', 1478537057, 0);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `user`
--

CREATE TABLE `user` (
  `id` int(11) NOT NULL,
  `username` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `email` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `password_hash` varchar(60) COLLATE utf8_unicode_ci NOT NULL,
  `auth_key` varchar(32) COLLATE utf8_unicode_ci NOT NULL,
  `confirmed_at` int(11) DEFAULT NULL,
  `unconfirmed_email` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `blocked_at` int(11) DEFAULT NULL,
  `registration_ip` varchar(45) COLLATE utf8_unicode_ci DEFAULT NULL,
  `created_at` int(11) NOT NULL,
  `updated_at` int(11) NOT NULL,
  `flags` int(11) NOT NULL DEFAULT '0'
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `user`
--

INSERT INTO `user` (`id`, `username`, `email`, `password_hash`, `auth_key`, `confirmed_at`, `unconfirmed_email`, `blocked_at`, `registration_ip`, `created_at`, `updated_at`, `flags`) VALUES
(1, 'wendy', 'wendytunegrita@gmail.com', '$2y$10$XMm6bZR3QCf1pJBfMr3Eu.aF9U1CyE/CDcIYWJqI/sKcghYW9iHEO', 'o41qPiKEY1aPOwcPnqr8RGpbRwPF_RNN', NULL, NULL, NULL, '::1', 1478300769, 1478300769, 0),
(2, 'paola', 'wendy.paola.estupinan@gmail.com', '$2y$12$Wd3vpmpqhGMH8Zpfm8lR.OfObj2at.wCKwQV7NVHLtP7WCd2rGEuC', 'YVRo4uR7IKGKBfv4lYgnsV5v_bSdjsUe', NULL, NULL, NULL, '::1', 1478364095, 1478364095, 0),
(7, 'marc', 'wendy@hotmail.com', '$2y$12$f33.2e5QYtiKJF1DP23mFu2G1M6xVxCUYAn3qi5Sga8eF3z1Ci9UG', '2C_yQfrH2qEOaUiubfwMIdA0UveVEK0y', NULL, NULL, NULL, '::1', 1478537057, 1478537057, 0);

--
-- Índices para tablas volcadas
--

--
-- Indices de la tabla `auth_assignment`
--
ALTER TABLE `auth_assignment`
  ADD PRIMARY KEY (`item_name`,`user_id`);

--
-- Indices de la tabla `auth_item`
--
ALTER TABLE `auth_item`
  ADD PRIMARY KEY (`name`),
  ADD KEY `rule_name` (`rule_name`),
  ADD KEY `idx-auth_item-type` (`type`);

--
-- Indices de la tabla `auth_item_child`
--
ALTER TABLE `auth_item_child`
  ADD PRIMARY KEY (`parent`,`child`),
  ADD KEY `child` (`child`);

--
-- Indices de la tabla `auth_rule`
--
ALTER TABLE `auth_rule`
  ADD PRIMARY KEY (`name`);

--
-- Indices de la tabla `blog_cms`
--
ALTER TABLE `blog_cms`
  ADD PRIMARY KEY (`Id`),
  ADD UNIQUE KEY `Titulo` (`Titulo`);

--
-- Indices de la tabla `migration`
--
ALTER TABLE `migration`
  ADD PRIMARY KEY (`version`);

--
-- Indices de la tabla `profile`
--
ALTER TABLE `profile`
  ADD PRIMARY KEY (`user_id`);

--
-- Indices de la tabla `social_account`
--
ALTER TABLE `social_account`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `account_unique` (`provider`,`client_id`),
  ADD UNIQUE KEY `account_unique_code` (`code`),
  ADD KEY `fk_user_account` (`user_id`);

--
-- Indices de la tabla `token`
--
ALTER TABLE `token`
  ADD UNIQUE KEY `token_unique` (`user_id`,`code`,`type`);

--
-- Indices de la tabla `user`
--
ALTER TABLE `user`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `user_unique_email` (`email`),
  ADD UNIQUE KEY `user_unique_username` (`username`);

--
-- AUTO_INCREMENT de las tablas volcadas
--

--
-- AUTO_INCREMENT de la tabla `blog_cms`
--
ALTER TABLE `blog_cms`
  MODIFY `Id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=23;
--
-- AUTO_INCREMENT de la tabla `social_account`
--
ALTER TABLE `social_account`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT de la tabla `user`
--
ALTER TABLE `user`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=8;
--
-- Restricciones para tablas volcadas
--

--
-- Filtros para la tabla `auth_assignment`
--
ALTER TABLE `auth_assignment`
  ADD CONSTRAINT `auth_assignment_ibfk_1` FOREIGN KEY (`item_name`) REFERENCES `auth_item` (`name`) ON DELETE CASCADE ON UPDATE CASCADE;

--
-- Filtros para la tabla `auth_item`
--
ALTER TABLE `auth_item`
  ADD CONSTRAINT `auth_item_ibfk_1` FOREIGN KEY (`rule_name`) REFERENCES `auth_rule` (`name`) ON DELETE SET NULL ON UPDATE CASCADE;

--
-- Filtros para la tabla `auth_item_child`
--
ALTER TABLE `auth_item_child`
  ADD CONSTRAINT `auth_item_child_ibfk_1` FOREIGN KEY (`parent`) REFERENCES `auth_item` (`name`) ON DELETE CASCADE ON UPDATE CASCADE,
  ADD CONSTRAINT `auth_item_child_ibfk_2` FOREIGN KEY (`child`) REFERENCES `auth_item` (`name`) ON DELETE CASCADE ON UPDATE CASCADE;

--
-- Filtros para la tabla `profile`
--
ALTER TABLE `profile`
  ADD CONSTRAINT `fk_user_profile` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE;

--
-- Filtros para la tabla `social_account`
--
ALTER TABLE `social_account`
  ADD CONSTRAINT `fk_user_account` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE;

--
-- Filtros para la tabla `token`
--
ALTER TABLE `token`
  ADD CONSTRAINT `fk_user_token` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
