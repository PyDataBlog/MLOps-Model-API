-- phpMyAdmin SQL Dump
-- version 3.5.3
-- http://www.phpmyadmin.net
--
-- Host: 127.0.0.1:3306
-- Generation Time: Dec 06, 2013 at 03:03 PM
-- Server version: 5.1.66-community
-- PHP Version: 5.3.18

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `bibles_sae`
--

-- --------------------------------------------------------

--
-- Table structure for table `bible_index`
--

DROP TABLE IF EXISTS `bible_index`;
CREATE TABLE IF NOT EXISTS `bible_index` (
  `volume` char(255) NOT NULL,
  `e_name` varchar(255) DEFAULT NULL,
  `c_name` varchar(255) DEFAULT NULL,
  `v_abb` varchar(255) DEFAULT NULL,
  `v_total` varchar(255) DEFAULT NULL,
  `all` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`volume`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

--
-- Dumping data for table `bible_index`
--

INSERT INTO `bible_index` (`volume`, `e_name`, `c_name`, `v_abb`, `v_total`, `all`) VALUES
('01', 'Genesis', '创世记', '创', '50', '1 Genesis-创世记(创-50)'),
('02', 'Exodus', '出埃及记', '出', '40', '2 Exodus-出埃及记(出-40)'),
('03', 'Leviticus', '利未记', '利', '27', '3 Leviticus-利未记(利-27)'),
('04', 'Numbers', '民数记', '民', '36', '4 Numbers-民数记(民-36)'),
('05', 'Deuteronomy', '申命记', '申', '34', '5 Deuteronomy-申命记(申-34)'),
('06', 'Joshua', '约书亚记', '书', '24', '6 Joshua-约书亚记(书-24)'),
('07', 'Judges', '士师记', '士', '21', '7 Judges-士师记(士-21)'),
('08', 'Ruth', '路得记', '得', '4', '8 Ruth-路得记(得-4)'),
('09', '1Samuel', '撒母耳记上', '撒上', '31', '9 1Samuel-撒母耳记上(撒上-31)'),
('10', '2Samuel', '撒母耳记下', '撒下', '24', '10 2Samuel-撒母耳记下(撒下-24)'),
('11', '1Kings', '列王记上', '王上', '22', '11 1Kings-列王记上(王上-22)'),
('12', '2Kings', '列王记下', '王下', '25', '12 2Kings-列王记下(王下-25)'),
('13', '1Chronicles', '历代志上', '代上', '29', '13 1Chronicles-历代志上(代上-29)'),
('14', '2Chronicles', '历代志下', '代下', '36', '14 2Chronicles-历代志下(代下-36)'),
('15', 'Ezra', '以斯拉记', '拉', '10', '15 Ezra-以斯拉记(拉-10)'),
('16', 'Nehemiah', '尼希米记', '尼', '13', '16 Nehemiah-尼希米记(尼-13)'),
('17', 'Esther', '以斯帖记', '斯', '10', '17 Esther-以斯帖记(斯-10)'),
('18', 'Job', '约伯记', '伯', '42', '18 Job-约伯记(伯-42)'),
('19', 'Psalm', '诗篇', '诗', '150', '19 Psalm-诗篇(诗-150)'),
('20', 'Proverbs', '箴言', '箴', '31', '20 Proverbs-箴言(箴-31)'),
('21', 'Ecclesiastes', '传道书', '传', '12', '21 Ecclesiastes-传道书(传-12)'),
('22', 'SongofSongs', '雅歌', '歌', '8', '22 SongofSongs-雅歌(歌-8)'),
('23', 'Isaiah', '以赛亚书', '赛', '66', '23 Isaiah-以赛亚书(赛-66)'),
('24', 'Jeremiah', '耶利米书', '耶', '52', '24 Jeremiah-耶利米书(耶-52)'),
('25', 'Lamentations', '耶利米哀歌', '哀', '5', '25 Lamentations-耶利米哀歌(哀-5)'),
('26', 'Ezekiel', '以西结书', '结', '48', '26 Ezekiel-以西结书(结-48)'),
('27', 'Daniel', '但以理书', '但', '12', '27 Daniel-但以理书(但-12)'),
('28', 'Hosea', '何西阿书', '何', '14', '28 Hosea-何西阿书(何-14)'),
('29', 'Joel', '约珥书', '珥', '3', '29 Joel-约珥书(珥-3)'),
('30', 'Amos', '阿摩司书', '摩', '9', '30 Amos-阿摩司书(摩-9)'),
('31', 'Obadiah', '俄巴底亚书', '俄', '1', '31 Obadiah-俄巴底亚书(俄-1)'),
('32', 'Jonah', '约拿书', '拿', '4', '32 Jonah-约拿书(拿-4)'),
('33', 'Micah', '弥迦书', '弥', '7', '33 Micah-弥迦书(弥-7)'),
('34', 'Nahum', '那鸿书', '鸿', '3', '34 Nahum-那鸿书(鸿-3)'),
('35', 'Habakkuk', '哈巴谷书', '哈', '3', '35 Habakkuk-哈巴谷书(哈-3)'),
('36', 'Zephaniah', '西番雅书', '番', '3', '36 Zephaniah-西番雅书(番-3)'),
('37', 'Haggai', '哈该书', '该', '2', '37 Haggai-哈该书(该-2)'),
('38', 'Zechariah', '撒迦利亚书', '亚', '14', '38 Zechariah-撒迦利亚书(亚-14)'),
('39', 'Malachi', '玛拉基书', '玛', '4', '39 Malachi-玛拉基书(玛-4)'),
('40', 'Matthew', '马太福音', '太', '28', '40 Matthew-马太福音(太-28)'),
('41', 'Mark', '马可福音', '可', '16', '41 Mark-马可福音(可-16)'),
('42', 'Luke', '路加福音', '路', '24', '42 Luke-路加福音(路-24)'),
('43', 'John', '约翰福音', '约', '21', '43 John-约翰福音(约-21)'),
('44', 'Acts', '使徒行传', '徒', '28', '44 Acts-使徒行传(徒-28)'),
('45', 'Romans', '罗马书', '罗', '16', '45 Romans-罗马书(罗-16)'),
('46', '1Corinthians', '哥林多前书', '林前', '16', '46 1Corinthians-哥林多前书(林前-16)'),
('47', '2Corinthians', '哥林多后书', '林后', '13', '47 2Corinthians-哥林多后书(林后-13)'),
('48', 'Galatians', '加拉太书', '加', '6', '48 Galatians-加拉太书(加-6)'),
('49', 'Ephesians', '以弗所书', '弗', '6', '49 Ephesians-以弗所书(弗-6)'),
('50', 'Philippians', '腓立比书', '腓', '4', '50 Philippians-腓立比书(腓-4)'),
('51', 'Colossians', '歌罗西书', '西', '4', '51 Colossians-歌罗西书(西-4)'),
('52', '1Thessalonians', '帖撒罗尼迦前书', '帖前', '5', '52 1Thessalonians-帖撒罗尼迦前书(帖前-5)'),
('53', '2Thessalonians', '帖撒罗尼迦后书', '帖后', '3', '53 2Thessalonians-帖撒罗尼迦后书(帖后-3)'),
('54', '1Timothy', '提摩太前书', '提前', '6', '54 1Timothy-提摩太前书(提前-6)'),
('55', '2Timothy', '提摩太后书', '提后', '4', '55 2Timothy-提摩太后书(提后-4)'),
('56', 'Titus', '提多书', '多', '3', '56 Titus-提多书(多-3)'),
('57', 'Philemon', '腓利门书', '门', '1', '57 Philemon-腓利门书(门-1)'),
('58', 'Hebrews', '希伯来书', '来', '13', '58 Hebrews-希伯来书(来-13)'),
('59', 'James', '雅各书', '雅', '5', '59 James-雅各书(雅-5)'),
('60', '1Peter', '彼得前书', '彼前', '5', '60 1Peter-彼得前书(彼前-5)'),
('61', '2Peter', '彼得后书', '彼后', '3', '61 2Peter-彼得后书(彼后-3)'),
('62', '1John', '约翰一书', '约一', '5', '62 1John-约翰一书(约一-5)'),
('63', '2John', '约翰二书', '约二', '1', '63 2John-约翰二书(约二-1)'),
('64', '3John', '约翰三书', '约三', '1', '64 3John-约翰三书(约三-1)'),
('65', 'Jude', '犹大书', '犹', '1', '65 Jude-犹大书(犹-1)'),
('66', 'Revelation', '启示录', '启', '22', '66 Revelation-启示录(启-22)');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
