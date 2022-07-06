-- phpMyAdmin SQL Dump
-- version 4.0.10deb1
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Jul 05, 2015 at 04:01 PM
-- Server version: 10.0.17-MariaDB-1~trusty-log
-- PHP Version: 5.5.9-1ubuntu4.7

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `UNLOCODE_TEST`
--

-- --------------------------------------------------------

--
-- Table structure for table `tbl_currency`
--

CREATE TABLE IF NOT EXISTS `tbl_currency` (
  `pk_currency_id` smallint(11) unsigned NOT NULL AUTO_INCREMENT,
  `currency_code` varchar(3) COLLATE utf8_unicode_ci NOT NULL COMMENT 'Use ISO 4217 Currency Codes',
  `currency_full_name` varchar(64) COLLATE utf8_unicode_ci NOT NULL,
  `currency_convertion_rate_to_base_currency` decimal(13,4) NOT NULL,
  PRIMARY KEY (`pk_currency_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=158 ;

--
-- Dumping data for table `tbl_currency`
--

INSERT INTO `tbl_currency` (`pk_currency_id`, `currency_code`, `currency_full_name`, `currency_convertion_rate_to_base_currency`) VALUES
(3, 'USD', 'US Dollar', 1.0000),
(4, 'INR', 'Indian Rupee', 0.0158),
(5, 'LKR', 'Sri Lanka Rupee', 0.0075),
(6, 'CAD', 'Canadian Dollar', 0.7955),
(7, 'AED', 'United Arab Emirates Dirham', 0.2722),
(8, 'GBP', 'British Pound', 1.5571),
(9, 'CNY', 'Yuan (Chinese) Renminbi', 0.1611),
(10, 'KRW', '(South) Korean Won', 0.0009),
(11, 'AFA', 'Afghanistan Afghani', 0.0000),
(12, 'ALL', 'Albanian Lek', 0.0079),
(13, 'DZD', 'Algerian Dinar', 0.0101),
(14, 'ADP', 'Andorran Peseta', 0.0000),
(15, 'AOK', 'Angolan Kwanza', 0.0000),
(16, 'ARS', 'Argentine Peso', 0.1099),
(17, 'AMD', 'Armenian Dram', 0.0021),
(18, 'AWG', 'Aruban Florin', 0.5587),
(19, 'AUD', 'Australian Dollar', 0.7517),
(20, 'BSD', 'Bahamian Dollar', 1.0000),
(21, 'BHD', 'Bahraini Dinar', 2.6546),
(22, 'BDT', 'Bangladeshi Taka', 0.0129),
(23, 'BBD', 'Barbados Dollar', 0.5000),
(24, 'BZD', 'Belize Dollar', 0.4963),
(25, 'BMD', 'Bermudian Dollar', 1.0000),
(26, 'BTN', 'Bhutan Ngultrum', 0.0158),
(27, 'BOB', 'Bolivian Boliviano', 0.1449),
(28, 'BWP', 'Botswanian Pula', 0.1005),
(29, 'BRL', 'Brazilian Real', 0.3190),
(30, 'BND', 'Brunei Dollar', 0.7430),
(31, 'BGN', 'Bulgarian Lev', 0.5672),
(32, 'BUK', 'Burma Kyat', 0.0000),
(33, 'BIF', 'Burundi Franc', 0.0006),
(34, 'CVE', 'Cape Verde Escudo', 0.0101),
(35, 'KYD', 'Cayman Islands Dollar', 1.2195),
(36, 'EUR', 'Euro', 1.1114),
(37, 'CLP', 'Chilean Peso', 0.0016),
(38, 'CLF', 'Chilean Unidades de Fomento', 40.6511),
(39, 'COP', 'Colombian Peso', 0.0004),
(40, 'XOF', 'Communauté Financière Africaine BCEAO - Francs', 0.0017),
(41, 'XAF', 'Communauté Financière Africaine BEAC, Francs', 0.0017),
(42, 'KMF', 'Comoros Franc', 0.0023),
(43, 'XPF', 'Comptoirs Français du Pacifique Francs', 0.0093),
(44, 'CRC', 'Costa Rican Colon', 0.0019),
(45, 'CUP', 'Cuban Peso', 1.0000),
(46, 'CYP', 'Cyprus Pound', 1.9247),
(47, 'CZK', 'Czech Republic Koruna', 0.0410),
(48, 'DKK', 'Danish Krone', 0.1490),
(49, 'YDD', 'Democratic Yemeni Dinar', 0.0000),
(50, 'DOP', 'Dominican Peso', 0.0224),
(51, 'XCD', 'East Caribbean Dollar', 0.3704),
(52, 'TPE', 'East Timor Escudo', 0.0000),
(53, 'ECS', 'Ecuador Sucre', 0.0000),
(54, 'EGP', 'Egyptian Pound', 0.1295),
(55, 'SVC', 'El Salvador Colon', 0.1144),
(56, 'EEK', 'Estonian Kroon (EEK)', 0.0000),
(57, 'ETB', 'Ethiopian Birr', 0.0484),
(58, 'FKP', 'Falkland Islands Pound', 1.5667),
(59, 'FJD', 'Fiji Dollar', 0.4757),
(60, 'GMD', 'Gambian Dalasi', 0.0255),
(61, 'GHC', 'Ghanaian Cedi', 0.0000),
(62, 'GIP', 'Gibraltar Pound', 1.5610),
(63, 'XAU', 'Gold, Ounces', 1168.9000),
(64, 'GTQ', 'Guatemalan Quetzal', 0.1312),
(65, 'GNF', 'Guinea Franc', 0.0001),
(66, 'GWP', 'Guinea-Bissau Peso', 0.0000),
(67, 'GYD', 'Guyanan Dollar', 0.0048),
(68, 'HTG', 'Haitian Gourde', 0.0191),
(69, 'HNL', 'Honduran Lempira', 0.0457),
(70, 'HKD', 'Hong Kong Dollar', 0.1290),
(71, 'HUF', 'Hungarian Forint', 0.0035),
(72, 'IDR', 'Indonesian Rupiah', 0.0001),
(73, 'XDR', 'International Monetary Fund (IMF) Special Drawing Rights', 1.4011),
(74, 'IRR', 'Iranian Rial', 0.0000),
(75, 'IQD', 'Iraqi Dinar', 0.0008),
(76, 'IEP', 'Irish Punt', 1.4303),
(77, 'ILS', 'Israeli Shekel', 0.2663),
(78, 'JMD', 'Jamaican Dollar', 0.0086),
(79, 'JPY', 'Japanese Yen', 0.0081),
(80, 'JOD', 'Jordanian Dinar', 1.4117),
(81, 'KHR', 'Kampuchean (Cambodian) Riel', 0.0002),
(82, 'KES', 'Kenyan Schilling', 0.0100),
(83, 'KWD', 'Kuwaiti Dinar', 3.3058),
(84, 'LAK', 'Lao Kip', 0.0001),
(85, 'LBP', 'Lebanese Pound', 0.0007),
(86, 'LSL', 'Lesotho Loti', 0.0812),
(87, 'LRD', 'Liberian Dollar', 0.0118),
(88, 'LYD', 'Libyan Dinar', 0.7306),
(89, 'MOP', 'Macau Pataca', 0.1252),
(90, 'MGF', 'Malagasy Franc', 0.0000),
(91, 'MWK', 'Malawi Kwacha', 0.0023),
(92, 'MYR', 'Malaysian Ringgit', 0.2640),
(93, 'MVR', 'Maldive Rufiyaa', 0.0649),
(94, 'MTL', 'Maltese Lira', 0.0000),
(95, 'MRO', 'Mauritanian Ouguiya', 0.0031),
(96, 'MUR', 'Mauritius Rupee', 0.0284),
(97, 'MXP', 'Mexican Peso', 0.0000),
(98, 'MNT', 'Mongolian Tugrik', 0.0005),
(99, 'MAD', 'Moroccan Dirham', 0.1026),
(100, 'MZM', 'Mozambique Metical', 0.0000),
(101, 'NAD', 'Namibian Dollar', 0.0812),
(102, 'NPR', 'Nepalese Rupee', 0.0098),
(103, 'ANG', 'Netherlands Antillian Guilder', 0.5587),
(104, 'YUD', 'New Yugoslavia Dinar', 0.0000),
(105, 'NZD', 'New Zealand Dollar', 0.6692),
(106, 'NIO', 'Nicaraguan Cordoba', 0.0367),
(107, 'NGN', 'Nigerian Naira', 0.0050),
(108, 'KPW', 'North Korean Won', 0.0011),
(109, 'NOK', 'Norwegian Kroner', 0.1252),
(110, 'OMR', 'Omani Rial', 2.5961),
(111, 'PKR', 'Pakistan Rupee', 0.0098),
(112, 'XPD', 'Palladium Ounces', 693.1500),
(113, 'PAB', 'Panamanian Balboa', 1.0000),
(114, 'PGK', 'Papua New Guinea Kina', 0.3635),
(115, 'PYG', 'Paraguay Guarani', 0.0002),
(116, 'PEN', 'Peruvian Nuevo Sol', 0.3148),
(117, 'PHP', 'Philippine Peso', 0.0222),
(118, 'XPT', 'Platinum, Ounces', 1083.6000),
(119, 'PLN', 'Polish Zloty', 0.2652),
(120, 'QAR', 'Qatari Rial', 0.2747),
(121, 'RON', 'Romanian Leu', 0.2476),
(122, 'RUB', 'Russian Ruble', 0.0178),
(123, 'RWF', 'Rwanda Franc', 0.0014),
(124, 'WST', 'Samoan Tala', 0.3920),
(125, 'STD', 'Sao Tome and Principe Dobra', 0.0000),
(126, 'SAR', 'Saudi Arabian Riyal', 0.2666),
(127, 'SCR', 'Seychelles Rupee', 0.0765),
(128, 'SLL', 'Sierra Leone Leone', 0.0002),
(129, 'XAG', 'Silver, Ounces', 15.5370),
(130, 'SGD', 'Singapore Dollar', 0.7431),
(131, 'SKK', 'Slovak Koruna', 0.0000),
(132, 'SBD', 'Solomon Islands Dollar', 0.1270),
(133, 'SOS', 'Somali Schilling', 0.0014),
(134, 'ZAR', 'South African Rand', 0.0812),
(135, 'SHP', 'St. Helena Pound', 1.5610),
(136, 'SDP', 'Sudanese Pound', 0.0000),
(137, 'SRG', 'Suriname Guilder', 0.0000),
(138, 'SZL', 'Swaziland Lilangeni', 0.0812),
(139, 'SEK', 'Swedish Krona', 0.1183),
(140, 'CHF', 'Swiss Franc', 1.0635),
(141, 'SYP', 'Syrian Potmd', 0.0053),
(142, 'TWD', 'Taiwan Dollar', 0.0324),
(143, 'TZS', 'Tanzanian Schilling', 0.0005),
(144, 'THB', 'Thai Baht', 0.0296),
(145, 'TOP', 'Tongan Paanga', 0.4823),
(146, 'TTD', 'Trinidad and Tobago Dollar', 0.1573),
(147, 'TND', 'Tunisian Dinar', 0.5106),
(148, 'TRY', 'Turkish Lira', 0.3716),
(149, 'UGX', 'Uganda Shilling', 0.0003),
(150, 'UYU', 'Uruguayan Peso', 0.0368),
(151, 'VUV', 'Vanuatu Vatu', 0.0091),
(152, 'VEF', 'Venezualan Bolivar', 0.1575),
(153, 'VND', 'Vietnamese Dong', 0.0000),
(154, 'YER', 'Yemeni Rial', 0.0047),
(155, 'ZRZ', 'Zaire Zaire', 0.0000),
(156, 'ZMK', 'Zambian Kwacha', 0.0002),
(157, 'ZWD', 'Zimbabwe Dollar', 0.0000);

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
