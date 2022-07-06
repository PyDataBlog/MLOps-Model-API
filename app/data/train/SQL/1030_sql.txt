-- phpMyAdmin SQL Dump
-- version 4.5.1
-- http://www.phpmyadmin.net
--
-- Servidor: 127.0.0.1
-- Tiempo de generación: 03-03-2017 a las 14:12:19
-- Versión del servidor: 10.1.19-MariaDB
-- Versión de PHP: 5.6.28

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Base de datos: `agencia`
--

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `actor`
--

CREATE TABLE `actor` (
  `id` int(11) NOT NULL,
  `id_actor` int(11) DEFAULT NULL,
  `NIF` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `Nom` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `Cognom1` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `Cognom2` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `Sexe` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `Foto` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `id_obra` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `actor`
--

INSERT INTO `actor` (`id`, `id_actor`, `NIF`, `Nom`, `Cognom1`, `Cognom2`, `Sexe`, `Foto`, `id_obra`) VALUES
(1, 1, '91589551D', 'José Luis', 'Calvo', 'Cuesta', 'male', 'actor01.jpeg', 9),
(2, 2, '66874983E', 'Ignacio', 'Mata', 'Cuervo', 'male', 'actor02.jpeg', 10),
(3, 1, '71576353P', 'Ivonne', 'Alonso', 'Villa', 'female', 'actriu02.jpeg', 11),
(4, 2, '90903310L', 'Eva', 'Fernández', 'Grube', 'female', 'actriu01.jpeg', 8),
(5, 3, '91589551A', 'Santiago', 'Borràs', 'Hofman', 'male', 'actor03.jpeg', 11),
(6, 1, '26560076E', 'Paco', 'Ramos', 'Cuesta', 'male', 'actor04.jpeg', 11),
(7, 2, '21695156C', 'martha', 'Alfaro', 'Céspedes', 'female', 'actriu03.jpeg', 9),
(8, 3, '86870514C', 'Julia', 'Lora', 'Mateu', 'female', 'actriu04.jpeg', 8);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `agencia`
--

CREATE TABLE `agencia` (
  `id` int(11) NOT NULL,
  `CIF` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `Nom` varchar(255) COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `director`
--

CREATE TABLE `director` (
  `id` int(11) NOT NULL,
  `NIF` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `Nom` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `Cognom1` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `Cognom2` varchar(255) COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `director`
--

INSERT INTO `director` (`id`, `NIF`, `Nom`, `Cognom1`, `Cognom2`) VALUES
(1, '21695156B', 'Francisco', 'Carbonell', 'Allende'),
(2, '26560076E', 'Juan Carlos', 'Martínez', 'Hofman'),
(3, '38066899J', 'Artur', 'Fernández', 'Santos'),
(4, '38066899J', 'Carles', 'Alonso', 'Sanchez'),
(7, '91589551A', 'Elvira', 'Taveras', 'Cuesta');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `obra`
--

CREATE TABLE `obra` (
  `id` int(11) NOT NULL,
  `id_agencia` int(11) DEFAULT NULL,
  `id_obra` int(11) DEFAULT NULL,
  `Nom` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `Descripcio` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `DataInici` date NOT NULL,
  `DataFi` date NOT NULL,
  `id_director` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `obra`
--

INSERT INTO `obra` (`id`, `id_agencia`, `id_obra`, `Nom`, `Descripcio`, `DataInici`, `DataFi`, `id_director`) VALUES
(8, NULL, 1, 'Romeo i julieta', 'Romance', '2016-12-12', '2017-02-23', 2),
(9, NULL, 3, 'Cats', 'drama', '2016-11-15', '2017-02-15', 1),
(10, NULL, 1, 'La Celestina', 'comedia', '2017-02-21', '2017-03-30', 7),
(11, NULL, 2, 'King Kong', 'clasico', '2016-11-08', '2017-04-13', 3);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `tipo_actor`
--

CREATE TABLE `tipo_actor` (
  `id` int(11) NOT NULL,
  `Papel` varchar(255) COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `tipo_actor`
--

INSERT INTO `tipo_actor` (`id`, `Papel`) VALUES
(1, 'Principal'),
(2, 'Secundari'),
(3, 'De repartiment');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `tipo_obra`
--

CREATE TABLE `tipo_obra` (
  `id` int(11) NOT NULL,
  `Descripcio` varchar(255) COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `tipo_obra`
--

INSERT INTO `tipo_obra` (`id`, `Descripcio`) VALUES
(1, 'Teatre'),
(2, 'Cinema'),
(3, 'dança');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `usuario`
--

CREATE TABLE `usuario` (
  `id` int(11) NOT NULL,
  `nombre` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `password` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `rol` varchar(255) COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Volcado de datos para la tabla `usuario`
--

INSERT INTO `usuario` (`id`, `nombre`, `password`, `rol`) VALUES
(1, 'admin', '123', 'Administrador');

--
-- Índices para tablas volcadas
--

--
-- Indices de la tabla `actor`
--
ALTER TABLE `actor`
  ADD PRIMARY KEY (`id`),
  ADD KEY `IDX_447556F9AAF017C9` (`id_actor`),
  ADD KEY `IDX_447556F9A5CE71BF` (`id_obra`);

--
-- Indices de la tabla `agencia`
--
ALTER TABLE `agencia`
  ADD PRIMARY KEY (`id`);

--
-- Indices de la tabla `director`
--
ALTER TABLE `director`
  ADD PRIMARY KEY (`id`);

--
-- Indices de la tabla `obra`
--
ALTER TABLE `obra`
  ADD PRIMARY KEY (`id`),
  ADD KEY `IDX_2EEE6DBD35F182E9` (`id_agencia`),
  ADD KEY `IDX_2EEE6DBDA5CE71BF` (`id_obra`),
  ADD KEY `IDX_2EEE6DBD4E4B3F65` (`id_director`);

--
-- Indices de la tabla `tipo_actor`
--
ALTER TABLE `tipo_actor`
  ADD PRIMARY KEY (`id`);

--
-- Indices de la tabla `tipo_obra`
--
ALTER TABLE `tipo_obra`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `UNIQ_24C1BAF811D3633A` (`id`);

--
-- Indices de la tabla `usuario`
--
ALTER TABLE `usuario`
  ADD PRIMARY KEY (`id`);

--
-- AUTO_INCREMENT de las tablas volcadas
--

--
-- AUTO_INCREMENT de la tabla `actor`
--
ALTER TABLE `actor`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=9;
--
-- AUTO_INCREMENT de la tabla `agencia`
--
ALTER TABLE `agencia`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT de la tabla `director`
--
ALTER TABLE `director`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=8;
--
-- AUTO_INCREMENT de la tabla `obra`
--
ALTER TABLE `obra`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=12;
--
-- AUTO_INCREMENT de la tabla `tipo_actor`
--
ALTER TABLE `tipo_actor`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=4;
--
-- AUTO_INCREMENT de la tabla `tipo_obra`
--
ALTER TABLE `tipo_obra`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=4;
--
-- AUTO_INCREMENT de la tabla `usuario`
--
ALTER TABLE `usuario`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=2;
--
-- Restricciones para tablas volcadas
--

--
-- Filtros para la tabla `actor`
--
ALTER TABLE `actor`
  ADD CONSTRAINT `FK_447556F9A5CE71BF` FOREIGN KEY (`id_obra`) REFERENCES `obra` (`id`),
  ADD CONSTRAINT `FK_447556F9AAF017C9` FOREIGN KEY (`id_actor`) REFERENCES `tipo_actor` (`id`);

--
-- Filtros para la tabla `obra`
--
ALTER TABLE `obra`
  ADD CONSTRAINT `FK_2EEE6DBD35F182E9` FOREIGN KEY (`id_agencia`) REFERENCES `agencia` (`id`),
  ADD CONSTRAINT `FK_2EEE6DBD4E4B3F65` FOREIGN KEY (`id_director`) REFERENCES `director` (`id`),
  ADD CONSTRAINT `FK_2EEE6DBDA5CE71BF` FOREIGN KEY (`id_obra`) REFERENCES `tipo_obra` (`id`);

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
