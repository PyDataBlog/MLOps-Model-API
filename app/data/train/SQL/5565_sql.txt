-- phpMyAdmin SQL Dump
-- version 4.5.1
-- http://www.phpmyadmin.net
--
-- Host: 127.0.0.1
-- Erstellungszeit: 19. Nov 2016 um 16:40
-- Server-Version: 10.1.16-MariaDB
-- PHP-Version: 5.6.24

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Datenbank: `festiva`
--

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `warenkörbe`
--

CREATE TABLE `warenkörbe` (
  `id` int(3) NOT NULL,
  `benutzer_id` int(3) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Daten für Tabelle `warenkörbe`
--

INSERT INTO `warenkörbe` (`id`, `benutzer_id`) VALUES
(28, 1),
(29, 3),
(30, 5),
(31, 9),
(32, 35),
(33, 36),
(34, 37),
(35, 38),
(36, 39),
(37, 40);

--
-- Indizes der exportierten Tabellen
--

--
-- Indizes für die Tabelle `warenkörbe`
--
ALTER TABLE `warenkörbe`
  ADD PRIMARY KEY (`id`),
  ADD KEY `benutzer_id` (`benutzer_id`);

--
-- AUTO_INCREMENT für exportierte Tabellen
--

--
-- AUTO_INCREMENT für Tabelle `warenkörbe`
--
ALTER TABLE `warenkörbe`
  MODIFY `id` int(3) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=38;
--
-- Constraints der exportierten Tabellen
--

--
-- Constraints der Tabelle `warenkörbe`
--
ALTER TABLE `warenkörbe`
  ADD CONSTRAINT `warenkörbe_ibfk_1` FOREIGN KEY (`benutzer_id`) REFERENCES `benutzer` (`id`);

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
