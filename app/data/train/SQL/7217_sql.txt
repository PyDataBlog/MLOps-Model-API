-- phpMyAdmin SQL Dump
-- version 4.1.14
-- http://www.phpmyadmin.net
--
-- Client :  127.0.0.1
-- Généré le :  Jeu 05 Juin 2014 à 19:26
-- Version du serveur :  5.6.17
-- Version de PHP :  5.5.12

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Base de données :  `drivecommunal`
--

-- --------------------------------------------------------

--
-- Structure de la table `address`
--

CREATE TABLE IF NOT EXISTS `address` (
  `idAddress` int(11) NOT NULL AUTO_INCREMENT,
  `idUser` int(11) NOT NULL,
  `title` varchar(255) NOT NULL,
  `displayName` varchar(255) NOT NULL,
  `address` varchar(255) NOT NULL,
  `zipCode` int(11) NOT NULL,
  `phoneNumber` varchar(255) NOT NULL,
  PRIMARY KEY (`idAddress`),
  UNIQUE KEY `idAddress_UNIQUE` (`idAddress`),
  KEY `idUser_FK_idx` (`idUser`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Structure de la table `product`
--

CREATE TABLE IF NOT EXISTS `product` (
  `idProduct` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `price` decimal(10,5) NOT NULL,
  `quantity` int(11) NOT NULL,
  `description` text,
  `idSeller` int(11) DEFAULT NULL,
  PRIMARY KEY (`idProduct`),
  UNIQUE KEY `idProduct_UNIQUE` (`idProduct`),
  UNIQUE KEY `name_UNIQUE` (`name`),
  KEY `idSeller_FK_idx` (`idSeller`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=9 ;

--
-- Contenu de la table `product`
--

INSERT INTO `product` (`idProduct`, `name`, `price`, `quantity`, `description`, `idSeller`) VALUES
(1, 'Saucisse-de-volaille', '5.50000', 1, 'Saucisse-de-volaille', 2),
(2, 'poulet-escalopes-fermieres-surchoix', '5.86000', 1, 'poulet-escalopes-fermieres-surchoix', 2),
(3, 'Raisin italia', '1.20000', 1, 'Raisin italia', 2),
(4, 'jambon-blanc', '5.26000', 1, 'jambon-blanc', 2),
(5, 'Pessac Leognan 2009', '5.38000', 1, 'Pessac Leognan 2009', 2),
(6, 'Persil', '2.27000', 1, 'Persil', 2),
(7, 'chabli-cendre', '4.31000', 1, 'chabli-cendre', 2),
(8, 'Faux filets', '6.10000', 1, 'Faux filets', 2);

-- --------------------------------------------------------

--
-- Structure de la table `user`
--

CREATE TABLE IF NOT EXISTS `user` (
  `idUser` int(11) NOT NULL AUTO_INCREMENT,
  `isSeller` tinyint(1) NOT NULL DEFAULT '0',
  `email` varchar(255) NOT NULL,
  `password` varchar(255) NOT NULL,
  `displayName` varchar(255) NOT NULL,
  `titleOrGender` varchar(255) NOT NULL,
  PRIMARY KEY (`idUser`),
  UNIQUE KEY `email_UNIQUE` (`email`),
  UNIQUE KEY `idUser_UNIQUE` (`idUser`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=3 ;

--
-- Contenu de la table `user`
--

INSERT INTO `user` (`idUser`, `isSeller`, `email`, `password`, `displayName`, `titleOrGender`) VALUES
(1, 0, 'buyer1@example.com', 'buyer1', 'Buyer #1', 'Mr.'),
(2, 1, 'seller1@example.com', 'seller1', 'Seller #1', 'Boucher');

--
-- Contraintes pour les tables exportées
--

--
-- Contraintes pour la table `address`
--
ALTER TABLE `address`
  ADD CONSTRAINT `idUser_FK` FOREIGN KEY (`idUser`) REFERENCES `user` (`idUser`) ON DELETE NO ACTION ON UPDATE NO ACTION;

--
-- Contraintes pour la table `product`
--
ALTER TABLE `product`
  ADD CONSTRAINT `idSeller_FK` FOREIGN KEY (`idSeller`) REFERENCES `user` (`idUser`) ON DELETE NO ACTION ON UPDATE NO ACTION;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
