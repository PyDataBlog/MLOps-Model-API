/*
Navicat MySQL Data Transfer

Source Server         : mysql
Source Server Version : 50629
Source Host           : localhost:3306
Source Database       : bookrack

Target Server Type    : MYSQL
Target Server Version : 50629
File Encoding         : 65001

Date: 2017-03-19 18:14:56
*/

SET FOREIGN_KEY_CHECKS=0;

-- ----------------------------
-- Table structure for bookrack_book
-- ----------------------------
DROP TABLE IF EXISTS `bookrack_book`;
CREATE TABLE `bookrack_book` (
  `id` varchar(32) NOT NULL,
  `isbn` varchar(100) DEFAULT NULL,
  `title` varchar(100) DEFAULT NULL,
  `subtitle` varchar(255) DEFAULT NULL,
  `origin_title` varchar(255) DEFAULT NULL,
  `average` varchar(10) DEFAULT NULL,
  `author` varchar(255) DEFAULT NULL,
  `translator` varchar(255) DEFAULT NULL,
  `pubdate` varchar(100) DEFAULT NULL,
  `publisher` varchar(100) DEFAULT NULL,
  `image` varchar(255) DEFAULT NULL,
  `binding` varchar(100) DEFAULT NULL,
  `pages` varchar(100) DEFAULT NULL,
  `price` varchar(100) DEFAULT NULL,
  `summary` text,
  `author_intro` text,
  `catalog` text,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Table structure for bookrack_user
-- ----------------------------
DROP TABLE IF EXISTS `bookrack_user`;
CREATE TABLE `bookrack_user` (
  `id` varchar(32) NOT NULL,
  `nick_name` varchar(100) DEFAULT NULL,
  `email` varchar(100) DEFAULT NULL,
  `email_state` varchar(100) DEFAULT NULL,
  `phone` varchar(100) DEFAULT NULL,
  `password` varchar(100) DEFAULT NULL,
  `weixin_id` varchar(100) DEFAULT NULL,
  `weixin_name` varchar(100) DEFAULT NULL,
  `weibo_id` varchar(100) DEFAULT NULL,
  `weibo_name` varchar(100) DEFAULT NULL,
  `github_id` varchar(100) DEFAULT NULL,
  `github_name` varchar(100) DEFAULT NULL,
  `douban_id` varchar(100) DEFAULT NULL,
  `douban_name` varchar(100) DEFAULT NULL,
  `user_img_url` varchar(255) DEFAULT NULL,
  `user_name` varchar(100) DEFAULT NULL,
  `job_num` varchar(100) DEFAULT NULL,
  `profile` varchar(100) DEFAULT NULL,
  `create_time` datetime DEFAULT NULL,
  `role` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Table structure for bookrack_user_book
-- ----------------------------
DROP TABLE IF EXISTS `bookrack_user_book`;
CREATE TABLE `bookrack_user_book` (
  `id` varchar(32) NOT NULL,
  `user_id` varchar(32) DEFAULT NULL,
  `book_id` varchar(32) DEFAULT NULL,
  `location` varchar(100) DEFAULT NULL,
  `level` varchar(100) DEFAULT NULL,
  `score` varchar(100) DEFAULT NULL,
  `comment` varchar(255) DEFAULT NULL,
  `create_time` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
