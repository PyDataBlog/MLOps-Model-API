-- -----------------------------
-- Think MySQL Data Transfer 
-- 
-- Host     : 127.0.0.1
-- Port     : 3306
-- Database : easycms
-- 
-- Part : #1
-- Date : 2015-12-16 12:50:15
-- -----------------------------

SET FOREIGN_KEY_CHECKS = 0;


-- -----------------------------
-- Table structure for `easy_access`
-- -----------------------------
DROP TABLE IF EXISTS `easy_access`;
CREATE TABLE `easy_access` (
  `role_id` smallint(6) unsigned NOT NULL,
  `node_id` smallint(6) unsigned NOT NULL,
  `level` tinyint(1) NOT NULL,
  `module` varchar(50) DEFAULT NULL,
  KEY `groupId` (`role_id`),
  KEY `nodeId` (`node_id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;


-- -----------------------------
-- Table structure for `easy_article`
-- -----------------------------
DROP TABLE IF EXISTS `easy_article`;
CREATE TABLE `easy_article` (
  `article_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `tid` int(10) unsigned NOT NULL,
  `title` varchar(40) NOT NULL,
  `keyword` varchar(255) NOT NULL,
  `pubtime` int(10) unsigned NOT NULL,
  `summary` varchar(255) NOT NULL,
  `content` text NOT NULL,
  `iscommend` tinyint(1) unsigned NOT NULL,
  `ispush` tinyint(1) unsigned NOT NULL,
  `islock` tinyint(1) unsigned NOT NULL,
  `sort` int(6) DEFAULT '10000',
  PRIMARY KEY (`article_id`)
) ENGINE=MyISAM AUTO_INCREMENT=66 DEFAULT CHARSET=utf8;


-- -----------------------------
-- Table structure for `easy_banner`
-- -----------------------------
DROP TABLE IF EXISTS `easy_banner`;
CREATE TABLE `easy_banner` (
  `banner_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `title` varchar(60) NOT NULL,
  `sort` int(6) NOT NULL DEFAULT '10',
  `url` varchar(255) NOT NULL,
  `link` varchar(255) DEFAULT NULL,
  `isshow` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`banner_id`)
) ENGINE=MyISAM AUTO_INCREMENT=60 DEFAULT CHARSET=utf8;


-- -----------------------------
-- Table structure for `easy_category`
-- -----------------------------
DROP TABLE IF EXISTS `easy_category`;
CREATE TABLE `easy_category` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` char(15) NOT NULL DEFAULT '',
  `pid` int(10) unsigned NOT NULL DEFAULT '0',
  `sort` int(6) NOT NULL DEFAULT '100',
  `modelid` tinyint(1) NOT NULL DEFAULT '0',
  `isshow` tinyint(1) NOT NULL DEFAULT '1',
  `isverify` tinyint(1) NOT NULL DEFAULT '1',
  `ispush` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`),
  KEY `pid` (`pid`)
) ENGINE=MyISAM AUTO_INCREMENT=95 DEFAULT CHARSET=utf8;

-- -----------------------------
-- Table structure for `easy_comment`
-- -----------------------------
DROP TABLE IF EXISTS `easy_comment`;
CREATE TABLE `easy_comment` (
  `commend_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `uid` int(10) unsigned NOT NULL,
  `aid` int(10) unsigned NOT NULL,
  `content` text NOT NULL,
  `islock` tinyint(1) unsigned NOT NULL,
  `pubtime` int(11) NOT NULL,
  PRIMARY KEY (`commend_id`)
) ENGINE=MyISAM AUTO_INCREMENT=32 DEFAULT CHARSET=utf8;


-- -----------------------------
-- Table structure for `easy_fields`
-- -----------------------------
DROP TABLE IF EXISTS `easy_fields`;
CREATE TABLE `easy_fields` (
  `fields_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `field` varchar(30) NOT NULL,
  `content` text NOT NULL,
  `issystem` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`fields_id`)
) ENGINE=MyISAM AUTO_INCREMENT=7 DEFAULT CHARSET=utf8;

-- -----------------------------
-- Table structure for `easy_link`
-- -----------------------------
DROP TABLE IF EXISTS `easy_link`;
CREATE TABLE `easy_link` (
  `link_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `url` varchar(255) NOT NULL,
  `isverify` tinyint(1) unsigned NOT NULL,
  PRIMARY KEY (`link_id`)
) ENGINE=MyISAM AUTO_INCREMENT=8 DEFAULT CHARSET=utf8;


-- -----------------------------
-- Table structure for `easy_member_user`
-- -----------------------------
DROP TABLE IF EXISTS `easy_member_user`;
CREATE TABLE `easy_member_user` (
  `user_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(15) NOT NULL,
  `password` char(32) NOT NULL,
  `email` varchar(32) NOT NULL,
  `sex` tinyint(1) unsigned NOT NULL,
  `photo` char(100) NOT NULL,
  `regtime` int(10) unsigned NOT NULL DEFAULT '0',
  `regip` char(15) NOT NULL,
  `islock` tinyint(1) unsigned NOT NULL,
  PRIMARY KEY (`user_id`)
) ENGINE=MyISAM AUTO_INCREMENT=29 DEFAULT CHARSET=utf8;


-- -----------------------------
-- Table structure for `easy_node`
-- -----------------------------
DROP TABLE IF EXISTS `easy_node`;
CREATE TABLE `easy_node` (
  `id` smallint(6) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) NOT NULL,
  `title` varchar(50) DEFAULT NULL,
  `status` tinyint(1) DEFAULT '0',
  `remark` varchar(255) DEFAULT NULL,
  `sort` smallint(6) unsigned DEFAULT NULL,
  `pid` smallint(6) unsigned NOT NULL,
  `level` tinyint(1) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `level` (`level`),
  KEY `pid` (`pid`),
  KEY `status` (`status`),
  KEY `name` (`name`)
) ENGINE=MyISAM AUTO_INCREMENT=64 DEFAULT CHARSET=utf8;



-- -----------------------------
-- Table structure for `easy_plugin`
-- -----------------------------
DROP TABLE IF EXISTS `easy_plugin`;
CREATE TABLE `easy_plugin` (
  `plugin_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(32) NOT NULL,
  `desc` varchar(255) NOT NULL DEFAULT '无',
  `method` varchar(255) NOT NULL,
  `isinstalled` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `position` tinyint(4) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`plugin_id`)
) ENGINE=MyISAM AUTO_INCREMENT=8 DEFAULT CHARSET=utf8;



-- -----------------------------
-- Table structure for `easy_role`
-- -----------------------------
DROP TABLE IF EXISTS `easy_role`;
CREATE TABLE `easy_role` (
  `id` smallint(6) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) NOT NULL,
  `pid` smallint(6) DEFAULT NULL,
  `status` tinyint(1) unsigned DEFAULT NULL,
  `remark` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `pid` (`pid`),
  KEY `status` (`status`)
) ENGINE=MyISAM AUTO_INCREMENT=8 DEFAULT CHARSET=utf8;



-- -----------------------------
-- Table structure for `easy_role_user`
-- -----------------------------
DROP TABLE IF EXISTS `easy_role_user`;
CREATE TABLE `easy_role_user` (
  `role_id` mediumint(9) unsigned DEFAULT NULL,
  `user_id` char(32) DEFAULT NULL,
  KEY `group_id` (`role_id`),
  KEY `user_id` (`user_id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;



-- -----------------------------
-- Table structure for `easy_user`
-- -----------------------------
DROP TABLE IF EXISTS `easy_user`;
CREATE TABLE `easy_user` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `username` char(20) NOT NULL DEFAULT '',
  `password` char(32) NOT NULL DEFAULT '',
  `logintime` int(10) unsigned NOT NULL,
  `loginip` varchar(30) NOT NULL,
  `lock` tinyint(1) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `username` (`username`)
) ENGINE=MyISAM AUTO_INCREMENT=14 DEFAULT CHARSET=utf8;
