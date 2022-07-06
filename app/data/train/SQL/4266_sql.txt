-- phpMyAdmin SQL Dump
-- version 4.1.14
-- http://www.phpmyadmin.net
--
-- Host: 127.0.0.1
-- Generation Time: 2015-07-21 20:09:28
-- 服务器版本： 5.6.17
-- PHP Version: 5.5.12

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `jinlingweb`
--

-- --------------------------------------------------------

--
-- 表的结构 `about_us`
--

CREATE TABLE IF NOT EXISTS `about_us` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'id',
  `about_us` text COMMENT '公司简介',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='公司简介' AUTO_INCREMENT=2 ;

--
-- 转存表中的数据 `about_us`
--

INSERT INTO `about_us` (`id`, `about_us`) VALUES
(1, '我公司前身是金能保温材料经营部，因扩大经营规模，于2009年3月变更为金陵贸\r\n				易有限公 。1994年开始从事保温、保冷、吸音、耐火、化工、建材等产品的经营贸易\r\n				，对控制产品质量方面具备非常好的经验，是中国东南地区最大的保温材料经营贸易公\r\n				司之一。公司经营宗旨是：诚信经营，质量第一。欢迎您的洽谈！1994年开始从事保温\r\n				、保冷、吸音、耐火、化工、建材等产品的经营贸易，对控制产品质量方面具备非常好\r\n				的经验，是中国东南地区最大的保温材料经营贸易公司之一。公司经营宗旨是：诚信经\r\n				营，质量第一。欢迎您的洽谈！');

-- --------------------------------------------------------

--
-- 表的结构 `admin`
--

CREATE TABLE IF NOT EXISTS `admin` (
  `admin_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `admin_name` varchar(255) DEFAULT NULL COMMENT '账号',
  `admin_pwd` char(32) DEFAULT NULL COMMENT '密码',
  `power` tinyint(4) DEFAULT NULL COMMENT '权限（1，超级管理员、2，普通管理员）',
  `register_time` int(11) DEFAULT NULL COMMENT '注册时间',
  `register_ip` varchar(255) DEFAULT NULL COMMENT '注册IP',
  `login_endtime` int(11) DEFAULT NULL COMMENT '最后登录时间',
  PRIMARY KEY (`admin_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='管理员表' AUTO_INCREMENT=7 ;

--
-- 转存表中的数据 `admin`
--

INSERT INTO `admin` (`admin_id`, `admin_name`, `admin_pwd`, `power`, `register_time`, `register_ip`, `login_endtime`) VALUES
(1, '聪聪', 'e10adc3949ba59abbe56e057f20f883e', 1, 1437479359, '::1', NULL),
(2, '是对方说到底是发生的', 'ddd08213b57e6c83ece30dc2b24aa892', 1, 1437479487, '::1', NULL),
(3, 'sadsada', '7c1cadb6887373dacb595c47166bfbd9', 1, 1437479590, '::1', NULL),
(4, 'ada', '4124bc0a9335c27f086f24ba207a4912', 1, 1437479784, '::1', NULL),
(5, 'asdasd', 'e2eb3a0cca905d8906c23f42515bac36', 1, 1437485611, '127.0.0.1', NULL),
(6, 'xiaoxioao', 'e10adc3949ba59abbe56e057f20f883e', 1, 1437485627, '127.0.0.1', NULL);

-- --------------------------------------------------------

--
-- 表的结构 `article`
--

CREATE TABLE IF NOT EXISTS `article` (
  `article_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `title` varchar(255) DEFAULT NULL COMMENT '标题',
  `content` text COMMENT '内容',
  `add_author` varchar(255) DEFAULT NULL COMMENT '添加作者',
  `add_time` int(11) NOT NULL COMMENT '添加日期',
  `number` int(11) DEFAULT NULL COMMENT '点击数',
  `is_show` tinyint(3) DEFAULT '1' COMMENT '是否显示',
  `orders` int(11) NOT NULL COMMENT '排序',
  `article_type` tinyint(3) unsigned DEFAULT NULL COMMENT '文章分类（1，新闻；2，通知）',
  `new_cate_id` tinyint(3) unsigned DEFAULT '1' COMMENT '新闻分类id',
  PRIMARY KEY (`article_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='公司文章表' AUTO_INCREMENT=5 ;

--
-- 转存表中的数据 `article`
--

INSERT INTO `article` (`article_id`, `title`, `content`, `add_author`, `add_time`, `number`, `is_show`, `orders`, `article_type`, `new_cate_id`) VALUES
(1, '祝贺公司网站正式上线', '我公司前身是金能保温材料经营部，因扩大经营规模，于2009年3月变更为金陵贸\n				易有限公 。1994年开始从事保温、保冷、吸音、耐火、化工、建材等产品的经营贸易\n				，对控制产品质量方面具备非常好的经验，是中国东南地区最大的保温材料经营贸易公\n				司之一。公司经营宗旨是：诚信经营，质量第一。欢迎您的洽谈！1994年开始从事保温\n				、保冷、吸音、耐火、化工、建材等产品的经营贸易，对控制产品质量方面具备非常好\n				的经验，是中国东南地区最大的保温材料经营贸易公司之一。公司经营宗旨是：诚信经\n				营，质量第一。欢迎您的洽谈！', '小小', 201512122, NULL, 1, 0, 1, 1),
(2, '禁止保温材料现场调配', '哈哈大环境大师的话就是', '小明', 0, NULL, 1, 0, NULL, 1),
(3, '祝贺公司网站正式上线', NULL, NULL, 0, NULL, 1, 0, NULL, 1),
(4, '祝贺公司网站正式上线', NULL, NULL, 0, NULL, 1, 0, NULL, 1);

-- --------------------------------------------------------

--
-- 表的结构 `banners`
--

CREATE TABLE IF NOT EXISTS `banners` (
  `ad_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT '广告id',
  `ad_title` varchar(255) DEFAULT NULL COMMENT '广告标题',
  `ad_imgurl` varchar(255) DEFAULT NULL COMMENT '广告路径',
  PRIMARY KEY (`ad_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='广告表' AUTO_INCREMENT=5 ;

--
-- 转存表中的数据 `banners`
--

INSERT INTO `banners` (`ad_id`, `ad_title`, `ad_imgurl`) VALUES
(1, '嘻嘻嘻', 'images/guanggao_bg.gif'),
(2, '啊哈哈哈哈', 'images/1324566_220639484000_2.jpg'),
(3, '嘿嘿', 'images/200841823454914_2.jpg'),
(4, '小孩子呀要听话', 'images/8070017_152213486000_2.jpg');

-- --------------------------------------------------------

--
-- 表的结构 `company`
--

CREATE TABLE IF NOT EXISTS `company` (
  `company_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `company_name` varchar(255) DEFAULT NULL COMMENT '公司名称',
  `company_keywords` varchar(255) DEFAULT NULL COMMENT '关键字',
  `company_description` text COMMENT '描述',
  `company_logo` varchar(255) DEFAULT NULL COMMENT '公司logo',
  `company_linkman` varchar(255) DEFAULT NULL COMMENT '联系人',
  `company_mobile` varchar(255) DEFAULT NULL COMMENT '移动电话',
  `company_phone` varchar(255) DEFAULT NULL COMMENT '固定电话',
  `company_qq` varchar(255) DEFAULT NULL COMMENT 'QQ号码',
  `company_fax` varchar(255) DEFAULT NULL COMMENT '传真',
  `company_address` varchar(255) DEFAULT NULL COMMENT '公司地址',
  `company_map` text COMMENT '公司地图',
  `company_content` text COMMENT '公司简介',
  `company_copyright` varchar(255) DEFAULT NULL COMMENT '公司版权',
  `company_recordnumber` varchar(255) DEFAULT NULL COMMENT '公司备案号',
  `company_erweima` varchar(255) DEFAULT NULL COMMENT '公司二维码',
  `company_weixin` varchar(255) DEFAULT NULL COMMENT '公司微信号',
  PRIMARY KEY (`company_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='公司信息配置表' AUTO_INCREMENT=2 ;

--
-- 转存表中的数据 `company`
--

INSERT INTO `company` (`company_id`, `company_name`, `company_keywords`, `company_description`, `company_logo`, `company_linkman`, `company_mobile`, `company_phone`, `company_qq`, `company_fax`, `company_address`, `company_map`, `company_content`, `company_copyright`, `company_recordnumber`, `company_erweima`, `company_weixin`) VALUES
(1, '金陵网络科技有限公司', '金陵，电子网，金陵网络', '呵呵呵呵呵呵嘻嘻嘻嘻嘻爱啊哈哈哈哈哈', 'uploads/201507081340469861.jpg', '乐乐', '13106627320', '020-123455', '22222222', '2345678', '广州市海珠去新港西路1号', 'images/weizhi.gif', '哈哈哈哈哈哈哈', '2015', NULL, NULL, '呵呵');

-- --------------------------------------------------------

--
-- 表的结构 `friendlink`
--

CREATE TABLE IF NOT EXISTS `friendlink` (
  `link_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `link_name` varchar(255) DEFAULT NULL COMMENT '外链名称',
  `link_url` varchar(255) DEFAULT NULL COMMENT '外链网址',
  `is_show` tinyint(4) DEFAULT NULL COMMENT '是否显示',
  `orders` int(11) DEFAULT NULL COMMENT '排序',
  PRIMARY KEY (`link_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='公司友情链接表' AUTO_INCREMENT=7 ;

--
-- 转存表中的数据 `friendlink`
--

INSERT INTO `friendlink` (`link_id`, `link_name`, `link_url`, `is_show`, `orders`) VALUES
(1, '百度', 'http://www.baidu.com', 1, NULL),
(2, '文豆', 'http://www.wengdo.com', 1, NULL),
(3, '凤凰网', 'http://www.baidu.com', NULL, NULL),
(4, '凤凰网', NULL, NULL, NULL),
(5, '太平洋电脑网', NULL, NULL, NULL),
(6, '中关村在线', NULL, NULL, NULL);

-- --------------------------------------------------------

--
-- 表的结构 `member`
--

CREATE TABLE IF NOT EXISTS `member` (
  `member_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `member_name` varchar(255) DEFAULT NULL COMMENT '用户名',
  `member_pwd` char(32) DEFAULT NULL COMMENT '密码',
  `member_email` varchar(255) DEFAULT NULL COMMENT '电子邮箱',
  `member_sex` int(11) NOT NULL DEFAULT '0' COMMENT '性别（0，女；1，男）',
  `member_desc` text COMMENT '个人介绍',
  `member_nickname` varchar(255) DEFAULT NULL COMMENT '昵称',
  `member_job` varchar(255) DEFAULT NULL COMMENT '职业',
  `member_company` varchar(255) DEFAULT NULL COMMENT '公司名称',
  `member_demailaddress` varchar(255) DEFAULT NULL COMMENT '详细地址',
  `member_liveaddress` varchar(255) DEFAULT NULL COMMENT '现居地',
  `member_signature` text COMMENT '个人签名',
  `member_photo` varchar(255) DEFAULT NULL COMMENT '头像',
  PRIMARY KEY (`member_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='公司会员表' AUTO_INCREMENT=2 ;

--
-- 转存表中的数据 `member`
--

INSERT INTO `member` (`member_id`, `member_name`, `member_pwd`, `member_email`, `member_sex`, `member_desc`, `member_nickname`, `member_job`, `member_company`, `member_demailaddress`, `member_liveaddress`, `member_signature`, `member_photo`) VALUES
(1, '聪聪', 'e10adc3949ba59abbe56e057f20f883e', '12345678@qq.com', 1, NULL, '聪哥', '工程师', '文豆有限公司', '天地之间', '广州市', '哈哈哈哈哈，嘻嘻嘻，呵呵呵，加油驾驭！', 'uploads/user_info/201507160212591331.jpg');

-- --------------------------------------------------------

--
-- 表的结构 `message`
--

CREATE TABLE IF NOT EXISTS `message` (
  `message_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `username` varchar(255) DEFAULT NULL COMMENT '用户名',
  `email` varchar(255) DEFAULT NULL COMMENT '电子邮件',
  `title` varchar(255) DEFAULT NULL COMMENT '主题',
  `content` text COMMENT '描述',
  `add_time` int(11) DEFAULT NULL COMMENT '发布时间',
  `member_id` int(11) DEFAULT '0' COMMENT '会员id',
  PRIMARY KEY (`message_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='公司留言表' AUTO_INCREMENT=21 ;

--
-- 转存表中的数据 `message`
--

INSERT INTO `message` (`message_id`, `username`, `email`, `title`, `content`, `add_time`, `member_id`) VALUES
(17, NULL, 'sdasddfgghjh', NULL, 'sddfdsfsdfdsfsdfsd', 1437053556, 0),
(18, NULL, 'bbbbbbbbbbb@qq.com', NULL, '西I嘻嘻', 1437053865, 0),
(19, '123456', '12345678@qq.com', '阿斯顿法国和会尽快', '阿斯顿撒大大', 1437054028, 0),
(20, '笑笑', '12345678@qq.com', '嗨你好！', '哈哈哈哈', 1437054061, 0),
(12, '笑笑', '123456@qq.com', NULL, '哈哈哈哈哈', 1436885285, 0),
(14, '嘻嘻', '123456@qq.com', NULL, '哈哈哈哈哈', 1436886254, 0),
(15, 'abcd', '12345678@qq.com', NULL, '', 1437051486, 0),
(16, 'adasdas', 'assdfdgfhgh', 'adsdsadas', 'sdasdasdasd', 1437053540, 0);

-- --------------------------------------------------------

--
-- 表的结构 `navs`
--

CREATE TABLE IF NOT EXISTS `navs` (
  `nav_id` tinyint(3) unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `nav_name` varchar(255) DEFAULT NULL COMMENT '导航名称',
  `nav_url` varchar(255) DEFAULT NULL COMMENT '导航地址',
  `parent_id` tinyint(4) DEFAULT NULL COMMENT '主导航ID',
  `is_show` tinyint(4) DEFAULT NULL COMMENT '是否显示',
  `orders` tinyint(4) DEFAULT NULL COMMENT '排序',
  PRIMARY KEY (`nav_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='导航（二级）表' AUTO_INCREMENT=15 ;

--
-- 转存表中的数据 `navs`
--

INSERT INTO `navs` (`nav_id`, `nav_name`, `nav_url`, `parent_id`, `is_show`, `orders`) VALUES
(1, '首页', 'index.php', NULL, 1, NULL),
(2, '公司简介', NULL, NULL, 1, NULL),
(3, '产品展示', NULL, NULL, 1, NULL),
(4, '行业知识', NULL, NULL, 1, NULL),
(5, '客户留言', '', NULL, 1, NULL),
(6, '联系我们', 'about_us.php', NULL, 1, NULL),
(7, '公司历史', 'about_us.php', 1, 1, NULL),
(11, '产品展示2', NULL, 2, 1, NULL),
(10, '产品展示3', NULL, 2, 1, NULL),
(12, '新闻中心2', NULL, 3, 1, NULL),
(13, '新闻3', NULL, 3, 1, NULL),
(14, '人才招聘2', NULL, 4, 1, NULL);

-- --------------------------------------------------------

--
-- 表的结构 `new_cate`
--

CREATE TABLE IF NOT EXISTS `new_cate` (
  `new_cate_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT '新闻分类ID',
  `new_cate_name` varchar(30) DEFAULT NULL COMMENT '新闻分类名称',
  PRIMARY KEY (`new_cate_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='新闻分类表' AUTO_INCREMENT=5 ;

--
-- 转存表中的数据 `new_cate`
--

INSERT INTO `new_cate` (`new_cate_id`, `new_cate_name`) VALUES
(1, '娱乐新闻'),
(2, 'IT新闻'),
(3, '科技新闻'),
(4, '社会新闻');

-- --------------------------------------------------------

--
-- 表的结构 `product`
--

CREATE TABLE IF NOT EXISTS `product` (
  `product_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT '产品id',
  `product_name` varchar(255) DEFAULT NULL COMMENT '产品名称',
  `product_imgurl` varchar(255) DEFAULT NULL COMMENT '产品路径',
  `product_desc` longtext COMMENT '描述',
  PRIMARY KEY (`product_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 COMMENT='产品表' AUTO_INCREMENT=7 ;

--
-- 转存表中的数据 `product`
--

INSERT INTO `product` (`product_id`, `product_name`, `product_imgurl`, `product_desc`) VALUES
(1, '网格布', 'images/gaoluzhuang.gif', '哈哈哈哈'),
(2, '像素管', 'images/sujiaoban.gif', '哈哈哈哈哈，西喜爱'),
(3, '塑胶版', 'images/wanggebu.gif', '阿红的结案数华盛顿时'),
(4, '哈哈哈哈', 'images/pic6.jpg', '哈哈哈哈哈'),
(5, '哈哈哈', 'images/pic6.jpg', '哈啊哈哈'),
(6, '呵呵呵', 'images/pic6.jpg', '啊U盾挥洒的哈市');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
