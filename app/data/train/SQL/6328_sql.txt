-- phpMyAdmin SQL Dump
-- version 3.4.10.1deb1
-- http://www.phpmyadmin.net
--
-- Client: localhost
-- Généré le : Jeu 15 Mai 2014 à 18:52
-- Version du serveur: 5.5.32
-- Version de PHP: 5.5.7-1+sury.org~precise+1

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Base de données: `pronostics`
--

-- --------------------------------------------------------

--
-- Structure de la table `prono_bet`
--

CREATE TABLE IF NOT EXISTS `prono_bet` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `game_id` int(11) DEFAULT NULL,
  `user_id` int(11) DEFAULT NULL,
  `scoreA` int(11) NOT NULL,
  `scoreB` int(11) NOT NULL,
  `risquette` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `IDX_3B62D951E48FD905` (`game_id`),
  KEY `IDX_3B62D951A76ED395` (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `prono_game`
--

CREATE TABLE IF NOT EXISTS `prono_game` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` varchar(100) COLLATE utf8_unicode_ci NOT NULL,
  `scoreA` int(11) DEFAULT NULL,
  `scoreB` int(11) DEFAULT NULL,
  `date` datetime NOT NULL,
  `location` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `risquette` longtext COLLATE utf8_unicode_ci,
  `risquetteValidee` tinyint(1) DEFAULT NULL,
  `channel` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `teamA_id` int(11) DEFAULT NULL,
  `teamB_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `IDX_585A88178C146F38` (`teamA_id`),
  KEY `IDX_585A88179EA1C0D6` (`teamB_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=2 ;

--
-- Contenu de la table `prono_game`
--

INSERT INTO `prono_game` (`id`, `title`, `scoreA`, `scoreB`, `date`, `location`, `risquette`, `risquetteValidee`, `channel`, `teamA_id`, `teamB_id`) VALUES
(1, 'Brésil -Croatie', NULL, NULL, '2014-06-12 00:00:00', 'Brésil', 'Au moins 2 buts d''écart', 0, NULL, 1, 2);

-- --------------------------------------------------------

--
-- Structure de la table `prono_group`
--

CREATE TABLE IF NOT EXISTS `prono_group` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(100) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=9 ;

--
-- Contenu de la table `prono_group`
--

INSERT INTO `prono_group` (`id`, `name`) VALUES
(1, 'Groupe A'),
(2, 'Groupe B'),
(3, 'Groupe C'),
(4, 'Groupe D'),
(5, 'Groupe E'),
(6, 'Groupe F'),
(7, 'Groupe G'),
(8, 'Groupe H');

-- --------------------------------------------------------

--
-- Structure de la table `prono_team`
--

CREATE TABLE IF NOT EXISTS `prono_team` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `group_id` int(11) DEFAULT NULL,
  `name` varchar(100) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`),
  KEY `IDX_BF911F84FE54D947` (`group_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=31 ;

--
-- Contenu de la table `prono_team`
--

INSERT INTO `prono_team` (`id`, `group_id`, `name`) VALUES
(1, 1, 'Brésil'),
(2, 1, 'Croatie'),
(3, 1, 'Mexique'),
(4, 1, 'Cameroun'),
(5, 2, 'Espagne'),
(6, 2, 'Pays-Bas'),
(7, 2, 'Chili'),
(8, 2, 'Australie'),
(9, 3, 'Colombie'),
(10, 3, 'Grèce'),
(11, 3, 'Côte d''Ivoire'),
(12, 3, 'Japon'),
(13, 4, 'Uruguay'),
(14, 4, 'Costa-Rica'),
(15, 4, 'Angleterre'),
(16, 4, 'Italie'),
(17, 5, 'Suisse'),
(18, 5, 'Equateur'),
(19, 5, 'France'),
(20, 5, 'Honduras'),
(21, 6, 'Argentine'),
(22, 6, 'Bosnie - Hérzégovine'),
(23, 6, 'Iran'),
(24, 6, 'Nigeria'),
(25, 7, 'Allemagne'),
(26, 7, 'Portugal'),
(27, 7, 'Ghana'),
(28, 7, 'Etats-Unis'),
(29, 8, 'Belgique'),
(30, 8, 'Algérie');

-- --------------------------------------------------------

--
-- Structure de la table `usergroup`
--

CREATE TABLE IF NOT EXISTS `usergroup` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(100) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `users`
--

CREATE TABLE IF NOT EXISTS `users` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `usergroup_id` int(11) DEFAULT NULL,
  `username` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `username_canonical` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `email` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `email_canonical` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `enabled` tinyint(1) NOT NULL,
  `salt` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `password` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `last_login` datetime DEFAULT NULL,
  `locked` tinyint(1) NOT NULL,
  `expired` tinyint(1) NOT NULL,
  `expires_at` datetime DEFAULT NULL,
  `confirmation_token` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `password_requested_at` datetime DEFAULT NULL,
  `roles` longtext COLLATE utf8_unicode_ci NOT NULL COMMENT '(DC2Type:array)',
  `credentials_expired` tinyint(1) NOT NULL,
  `credentials_expire_at` datetime DEFAULT NULL,
  `firstname` varchar(100) COLLATE utf8_unicode_ci DEFAULT NULL,
  `lastname` varchar(100) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `UNIQ_1483A5E992FC23A8` (`username_canonical`),
  UNIQUE KEY `UNIQ_1483A5E9A0D96FBF` (`email_canonical`),
  KEY `IDX_1483A5E9D2112630` (`usergroup_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=2 ;

--
-- Contenu de la table `users`
--

INSERT INTO `users` (`id`, `usergroup_id`, `username`, `username_canonical`, `email`, `email_canonical`, `enabled`, `salt`, `password`, `last_login`, `locked`, `expired`, `expires_at`, `confirmation_token`, `password_requested_at`, `roles`, `credentials_expired`, `credentials_expire_at`, `firstname`, `lastname`) VALUES
(1, NULL, 'greg', 'greg', 'gregquat@gmail.com', 'gregquat@gmail.com', 1, 'b4q52wrvpoo4s80cw8g0w80kkkgkwc', '1Zw3nn3dfMif60MaYlPG3QomonibHrALJ20VJRKP0PBbwtf/KBf39qraNq34o0z1xa3Foc2BGYSa2ZW207k0gQ==', '2014-05-15 14:02:46', 0, 0, NULL, NULL, NULL, 'a:1:{i:0;s:16:"ROLE_SUPER_ADMIN";}', 0, NULL, NULL, NULL);

--
-- Contraintes pour les tables exportées
--

--
-- Contraintes pour la table `prono_bet`
--
ALTER TABLE `prono_bet`
  ADD CONSTRAINT `FK_3B62D951A76ED395` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`),
  ADD CONSTRAINT `FK_3B62D951E48FD905` FOREIGN KEY (`game_id`) REFERENCES `prono_game` (`id`);

--
-- Contraintes pour la table `prono_game`
--
ALTER TABLE `prono_game`
  ADD CONSTRAINT `FK_585A88179EA1C0D6` FOREIGN KEY (`teamB_id`) REFERENCES `prono_team` (`id`),
  ADD CONSTRAINT `FK_585A88178C146F38` FOREIGN KEY (`teamA_id`) REFERENCES `prono_team` (`id`);

--
-- Contraintes pour la table `prono_team`
--
ALTER TABLE `prono_team`
  ADD CONSTRAINT `FK_BF911F84FE54D947` FOREIGN KEY (`group_id`) REFERENCES `prono_group` (`id`);

--
-- Contraintes pour la table `users`
--
ALTER TABLE `users`
  ADD CONSTRAINT `FK_1483A5E9D2112630` FOREIGN KEY (`usergroup_id`) REFERENCES `usergroup` (`id`);

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
