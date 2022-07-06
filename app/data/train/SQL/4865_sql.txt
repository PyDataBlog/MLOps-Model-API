-- phpMyAdmin SQL Dump
-- version 4.1.12
-- http://www.phpmyadmin.net
--
-- Host: 127.0.0.1
-- Generation Time: 27 Lut 2015, 14:53
-- Server version: 5.6.16
-- PHP Version: 5.5.11

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `symfony`
--

-- --------------------------------------------------------

--
-- Struktura tabeli dla tabeli `fos_user`
--

CREATE TABLE IF NOT EXISTS `fos_user` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `username` varchar(120) COLLATE utf8_polish_ci DEFAULT NULL,
  `username_canonical` varchar(50) COLLATE utf8_polish_ci DEFAULT NULL,
  `email` varchar(120) COLLATE utf8_polish_ci DEFAULT NULL,
  `email_canonical` varchar(50) COLLATE utf8_polish_ci DEFAULT NULL,
  `enabled` tinyint(1) DEFAULT NULL,
  `salt` varchar(255) COLLATE utf8_polish_ci DEFAULT NULL,
  `password` varchar(255) COLLATE utf8_polish_ci DEFAULT NULL,
  `plain_password` varchar(255) COLLATE utf8_polish_ci DEFAULT NULL,
  `last_login` datetime DEFAULT NULL,
  `confirmation_token` varchar(255) COLLATE utf8_polish_ci DEFAULT NULL,
  `password_requested_at` datetime DEFAULT NULL,
  `groups` varchar(255) COLLATE utf8_polish_ci DEFAULT NULL,
  `locked` tinyint(1) DEFAULT NULL,
  `expired` tinyint(1) DEFAULT NULL,
  `expires_at` datetime DEFAULT NULL,
  `roles` varchar(255) COLLATE utf8_polish_ci DEFAULT NULL,
  `credentials_expired` tinyint(1) DEFAULT NULL,
  `credentials_expire_at` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_polish_ci AUTO_INCREMENT=4 ;

--
-- Zrzut danych tabeli `fos_user`
--

INSERT INTO `fos_user` (`id`, `username`, `username_canonical`, `email`, `email_canonical`, `enabled`, `salt`, `password`, `plain_password`, `last_login`, `confirmation_token`, `password_requested_at`, `groups`, `locked`, `expired`, `expires_at`, `roles`, `credentials_expired`, `credentials_expire_at`) VALUES
(2, 'wacek', 'wacek', 'wacek@adrespoczty.pl', 'wacek@adrespoczty.pl', 1, '81gqlr0jv5s04wowo844kks484csgc8', 'HClTmZgsM/tM/5knTRREC5CbZMKnbf/wvZT3Zg4Sv9QChGVXNR2NnkWYdztWpqSLqqaySrLo76HNWM8o49Nr2w==', NULL, '2015-02-26 21:22:03', NULL, NULL, NULL, 0, 0, NULL, 'a:0:{}', 0, NULL),
(3, 'jola', 'jola', 'jola@adrespoczty.pl', 'jola@adrespoczty.pl', 1, '2vyhkxl8z0oww4o0cwgkggkck84ks8s', 'He1O6Rlkuzw/uM0h/Hw3/L0q6nEJy79M9S7qLvTY0gKL+BLFdDL7R49TF4uVV4uF/GJ0325wZYs7V9DCq3P8Yg==', NULL, '2015-02-27 14:53:08', NULL, NULL, NULL, 0, 0, NULL, 'a:0:{}', 0, NULL);

-- --------------------------------------------------------

--
-- Struktura tabeli dla tabeli `koszyk`
--

CREATE TABLE IF NOT EXISTS `koszyk` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `tytul` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `cena` int(11) NOT NULL,
  `autor` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `data_utworzenia` datetime NOT NULL,
  `movie_id` int(11) NOT NULL,
  `done` tinyint(1) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_polish_ci AUTO_INCREMENT=8 ;

--
-- Zrzut danych tabeli `koszyk`
--

INSERT INTO `koszyk` (`id`, `tytul`, `cena`, `autor`, `data_utworzenia`, `movie_id`, `done`) VALUES
(1, 'In Time', 59, 'wacek', '2015-02-25 15:28:46', 1, 0),
(2, 'Transporter', 89, 'wacek', '2015-02-26 14:15:47', 6, 0),
(4, 'Leon', 89, 'wacek', '2015-02-26 18:19:19', 4, 0),
(6, 'The Shawshank Redemption', 89, 'jola', '2015-02-26 19:41:27', 3, 0),
(7, 'Behind enemy lines', 59, 'jola', '2015-02-26 19:43:30', 2, 1);

-- --------------------------------------------------------

--
-- Struktura tabeli dla tabeli `movie`
--

CREATE TABLE IF NOT EXISTS `movie` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `value` int(11) NOT NULL,
  `img` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `gerne` int(11) NOT NULL,
  `source` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `opis` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `aktorzy` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `data` date NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_polish_ci AUTO_INCREMENT=9 ;

--
-- Zrzut danych tabeli `movie`
--

INSERT INTO `movie` (`id`, `name`, `value`, `img`, `gerne`, `source`, `opis`, `aktorzy`, `data`) VALUES
(1, 'In Time', 59, 'in-time-2011-SF.jpg', 7, 'http://thevideos.tv/embed-00exmaeuep3k-728x410.html', 'W społeczeństwie przyszłości walutą jest czas. Najbogatsi, czyli młodzi, zdrowi i piękni mogą żyć tysiące lat, podczas gdy biedota budzi się co rano, mając do dyspozycji kilkanaście godzin.', 'Justin Timberlake, Amanda Seyfried, Cillian Murphy', '2011-10-26'),
(2, 'Behind enemy lines', 59, 'behind-enemy-lines-2001-Action.jpg', 1, 'http://thevideos.tv/embed-gdrcsz7ynu8d-728x410.html', 'Wojna jest bliska końca, rozpoczyna się natomiast walka o przetrwanie. Chris Burnett (Owen Wilson) zostaje zestrzelony podczas rutynowej misji rozpoznawczej nad terytorium bośniackim. Cudem uchodzi z życiem, jego śladem ruszają okrutny oficer tajnej polic', 'Gene Hackman, Owen Wilson', '2001-11-17'),
(3, 'The Shawshank Redemption', 89, 'the-shawshank-redemption-1994.jpg', 3, 'http://thevideos.tv/embed-ryqpka7wslv9-728x410.html', 'Adaptacja opowiadania Stephena Kinga. Historia niesłusznie skazanego na dożywocie bankiera, który musi przeżyć w brutalnym świecie rządzonym przez strażników i współwięźniów.', 'Tim Robbins, Morgan Freeman, Bob Gunton', '1994-09-10'),
(4, 'Leon', 89, 'leon-1994.jpg', 5, 'http://thevideos.tv/embed-z01ej23llo0f-728x410.html', 'Płatny morderca ratuje dwunastoletnią dziewczynkę, której rodzina została zabita przez skorumpowanych policjantów.', 'Jean Reno, Gary Oldman, Natalie Portman', '1994-09-14'),
(5, 'Donnie Brasco', 59, 'donnie-brasco-1997.jpg', 2, 'Film oparty na faktach. Agent FBI pod pseudonimem Donnie Brasco wnika w środowisko mafijne, by rozpracować je od środka.', 'Film oparty na faktach. Agent FBI pod pseudonimem Donnie Brasco wnika w środowisko mafijne, by rozpracować je od środka.', 'Al Pacino, Johnny Depp, Michael Madsen', '1997-02-28'),
(6, 'Transporter', 89, 'the-transporter-2002.jpg', 1, 'http://thevideos.tv/embed-ee73ea3u6g5c-728x410.html', 'Frank Martin pracuje jako kurier dla organizacji przestępczych. Podczas transportu kolejnej przesyłki, wplątuje się w niebezpieczną intrygę.', 'Jason Statham, Qi Shu, Matt Schulze', '2002-10-02'),
(7, 'The Bourne Identity', 89, 'the-bourne-identity-2002.jpg', 1, 'http://thevideos.tv/embed-kv2z4z6etvk0-728x410.html', 'Ekranizacja powieści Roberta Ludluma. Jason Bourne w wyniku wypadku traci pamięć. Wszystko wskazuje na to, że pracował wcześniej jako płatny zabójca.', 'Franka Potente, Matt Damon, Chris Cooper', '2002-06-06'),
(8, 'Lost in Translation', 59, 'lost-in-translation-2003.jpg', 4, 'http://thevideos.tv/embed-ahx2gwxw55mg-728x410.html', 'Podstarzały aktor i młoda dziewczyna nawiązują nić porozumienia podczas pobytu w Tokio, gdzie oboje czują się osamotnieni.', 'Bill Murray, Scarlett Johansson, Giovanni Ribisi', '2003-08-29');

-- --------------------------------------------------------

--
-- Struktura tabeli dla tabeli `recenzja`
--

CREATE TABLE IF NOT EXISTS `recenzja` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `tytul` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `tresc` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `autor` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `data_utworzenia` datetime NOT NULL,
  `movie` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_polish_ci AUTO_INCREMENT=3 ;

--
-- Zrzut danych tabeli `recenzja`
--

INSERT INTO `recenzja` (`id`, `tytul`, `tresc`, `autor`, `data_utworzenia`, `movie`) VALUES
(1, 'pierwsza recenzja', 'recenzja wacka', 'wacek', '2015-02-24 00:00:00', 1),
(2, 'Za liniom wroga', 'Ciekawy film. Polecam', 'jola', '2015-02-26 00:00:00', 2);

-- --------------------------------------------------------

--
-- Struktura tabeli dla tabeli `t0`
--

CREATE TABLE IF NOT EXISTS `t0` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `username` varchar(120) COLLATE utf8_polish_ci NOT NULL,
  `username_canonical` varchar(50) COLLATE utf8_polish_ci NOT NULL,
  `email` varchar(120) COLLATE utf8_polish_ci NOT NULL,
  `emailc_anonical` varchar(50) COLLATE utf8_polish_ci NOT NULL,
  `enabled` tinyint(1) NOT NULL,
  `salt` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `password` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `plain_password` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `last_login` datetime NOT NULL,
  `confirmation_token` varchar(255) COLLATE utf8_polish_ci NOT NULL,
  `password_requested_at` datetime NOT NULL,
  `groups` int(11) NOT NULL,
  `locked` tinyint(1) NOT NULL,
  `expired` tinyint(1) NOT NULL,
  `expires_at` datetime NOT NULL,
  `roles` int(11) NOT NULL,
  `credentials_expired` tinyint(1) NOT NULL,
  `credentials_expire_at` datetime NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_polish_ci AUTO_INCREMENT=1 ;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
