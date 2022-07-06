# ************************************************************
# Sequel Pro SQL dump
# Version 4096
#
# http://www.sequelpro.com/
# http://code.google.com/p/sequel-pro/
#
# Host: localhost (MySQL 5.6.26)
# Database: p2p
# Generation Time: 2015-10-28 15:52:05 +0000
# ************************************************************


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;


# Dump of table fin_p2p_invest_contract
# ------------------------------------------------------------

DROP TABLE IF EXISTS `fin_p2p_invest_contract`;

CREATE TABLE `fin_p2p_invest_contract` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL DEFAULT '',
  `master_id` int(11) DEFAULT NULL,
  `description` varchar(100) DEFAULT NULL,
  `amount_min` int(11) DEFAULT NULL,
  `amount_max` int(11) DEFAULT NULL,
  `duration_min` int(11) DEFAULT NULL,
  `duration_max` int(11) DEFAULT NULL,
  `duration_type` int(11) DEFAULT NULL COMMENT '0: 天; 1:周; ２:月; 3:年',
  `for_register` tinyint(4) DEFAULT NULL COMMENT '0: 非新注册用户专享;1:新注册用户专享',
  `rate` float DEFAULT NULL COMMENT '预期收益率',
  `early_terminate` tinyint(4) DEFAULT NULL COMMENT '0:可提前赎回 ;1:不可提前赎回, 不可提前转让 ;2: 不可提前赎回，可提前转让 ',
  `risk_rank` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `fin_p2p_invest_contract` WRITE;
/*!40000 ALTER TABLE `fin_p2p_invest_contract` DISABLE KEYS */;

INSERT INTO `fin_p2p_invest_contract` (`id`, `name`, `master_id`, `description`, `amount_min`, `amount_max`, `duration_min`, `duration_max`, `duration_type`, `for_register`, `rate`, `early_terminate`, `risk_rank`)
VALUES
	(1867,'e路同心 - 活期宝-按天计息',2302,'理财周特供标[专享]高收益,国企担保',100,0,1,0,2,1,6.88,1,0),
	(1868,'PPmoney - 短期精选理财',2303,'注册就送8888元,投资送60元红包',100,0,1,0,2,1,25,0,0),
	(1869,'爱钱帮 - 【爱房贷项目】—短期精选理财',2304,'银行存管,上市公司盛大投资,注册送88元,当日计息.',100,0,1,0,2,1,16,2,0),
	(1870,'易通贷  - e起购理财',2305,'放心投易通贷，100元起投，年化收益16.5%，注册送红包',100,0,3,0,2,1,16.5,2,0),
	(1871,'积木盒子 - 新手专享项目',2306,'新手专享,100元起投,理财新选择,安全有保障',100,0,1,0,2,1,12.5,2,0),
	(1872,'小牛在线 - 【新手专享】',2307,'100起投,收益高,期限灵活,安全有保障',100,0,1,0,2,1,15,2,0),
	(1873,'平安陆金所 - 【点金计划5】',-1,'一键组合投资\"稳盈-安e\"加\"陆金宝\"赚取更高收益',50000,0,6,0,2,1,9.8,2,0),
	(1874,'合时代 - 【新手专享标2015091553】',2309,'新手专享8天超短期理财，100元起投，18%年化收益',100,0,1,0,2,1,18,2,0),
	(1875,'玖富 - 短期特供理财-新手专享标',2310,'玖富9年安全,21天特供标,15%年化收益,注册送150元红包',1000,0,1,0,2,1,15,2,0),
	(1876,'你我贷 - 嘉财有道月月盈',2311,'年化收益率高达14%,国家AA级P2P平台',2000,0,12,0,2,1,13,0,0),
	(1877,'点融网  - 新手投资团',2312,'新手专享，10%年化利率外加白领1万元体验金',100,0,1,0,2,1,10,0,0),
	(1878,'你我贷 - 月 月 升',2311,'6%起，逐月涨0.5%,高达11%',2000,0,1,0,2,1,6,0,0),
	(1879,'付融宝 - -新用户专享项目',2313,'新手注册送50元红包,投资更享多重优惠.',100,0,1,0,2,1,10,2,0),
	(1880,'e租宝 - 【e租富盈】理财精选',2314,'1元起投，13.8%预期收益，易投，易赚，易取',1,0,6,0,2,1,13.8,0,0),
	(1881,'抱财网  - 房抵贷-30天理财项目',2315,'安心投抱财,100元起投,年化收益15%,注册即送2470',100,0,1,0,2,1,15,2,0),
	(1882,'e路同心 - 【理财季特供标D】',2302,'理财周特供标[专享]高收益,国企担保',100,0,1,0,2,1,20,2,0),
	(1883,'小牛在线 - 【安心牛】',2307,'低风险，高流动性，自动复投，可提前赎回',100,0,12,0,2,1,15,2,0),
	(1884,'君融贷 - 供应链-联想经销商融资',2316,'供应链金融项目，联想推荐经销商',100,0,1,0,2,1,15,0,0),
	(1885,'永利宝  - 【新手专享】',2317,'100元起投，年化收益18%，注册即送100，首投返308',100,0,1,0,2,1,18,2,0),
	(1886,'普资金融 -  普资华企-华企通',2318,'保本理财,收益15%.央行征信系统可查',10000,0,6,0,2,1,15,2,0),
	(1887,'小牛在线 - 【天天牛】',2307,'货币基金理财,随时赎回,当日提现,次日到账,1元起投',1,0,1,0,2,1,7.5,0,0),
	(1888,'易通贷  - 中小企业短期资金周转-A',2305,'放心投易通贷，100元起投，年化收益17.6%，注册送红包',100,0,3,0,2,1,17.6,2,0),
	(1889,'玖富 - 新手专享-21天短期理财标',2310,'28家银行总行签约,首批银行资金托管,1000万用户的信赖',1000,0,1,0,2,1,15,2,0),
	(1890,'易通贷  - 短期理财【新手专享】',2305,'100元起投，年化收益16.5%，注册送50元，邀请再送20元',100,0,3,0,2,1,16.5,2,0),
	(1891,'新联在线 - 【新手活动专区】',2319,'注册送10000元理财基金（奖）',50,0,1,0,2,1,18,0,0),
	(1892,'微金所 - 微小宝短期理财',2320,'微小宝-期限短,年化高,安全有保障',100,0,1,0,2,1,10.5,0,0),
	(1893,'积木盒子 - 购房周转贷',2306,'收益快,次日计息,100元起投,大型担保公司提供担保',100,0,2,0,2,1,9.5,2,0),
	(1894,'e路同心 - 活期宝-【百度专享标】',2302,'国企担保,注册送68888元体验金',100,0,1,0,2,1,6.88,1,0),
	(1895,'小牛在线 - 【红本赎楼标】',2307,'房产证书赎回，全程跟踪，100元低门槛',100,0,1,0,2,1,15,2,0),
	(1896,'三分贷  - 中小企业短期资金周转稳健型-AAA',2321,'车辆抵押，投资有保障，放心理财，收益高上高',100,0,3,0,2,1,22,2,0),
	(1897,'可溯贷 - 优农企融贷(3个月)',2322,'国资背景,资金托管,国家AAA级P2P平台',1000,0,3,0,2,1,15,2,0),
	(1898,'抱财网  - 担保贷【新手专享】',2315,'上市公司背景,资金托管P2P平台,限时投资返1%',100,0,1,0,2,1,15,2,0),
	(1899,'e租宝 - 【e租年享】理财精选',2314,'1元起投,P2P理财正规军，支持提前赎回',1,0,12,0,2,1,14.6,0,0),
	(1900,'平安陆金所 - 【点金计划1】',-1,'一键组合投资\"稳盈-安e\"加\"陆金宝\"赚取更高收益',10000,0,6,0,2,1,9.2,2,0),
	(1901,'妙资金融  - 【新手标】新手专享M12',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,17,2,0),
	(1902,'口袋理财  - 稳盈Z计划',2324,'固定收益8%加浮动收益0-12%，一月即可转让',1000,0,12,0,2,1,12,2,0),
	(1903,'付融宝 - [金融超市]--月盈宝',2313,'付融宝[金融超市]专业的投资理财平台',100,0,3,0,2,1,12,2,0),
	(1904,'e租宝 - 【e租稳盈】短期-灵活理财',2314,'1元起投,安稳盈收,按月付息,随时赎回',1,0,1,0,2,1,9,0,0),
	(1905,'易通贷  - 新手理财精选',2305,'100元起投，年化收益16%，注册送50元，邀请再送20元',100,0,3,0,2,1,16,2,0),
	(1906,'妙资金融  - 【新手标】新手专享M15',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,17,2,0),
	(1907,'e租宝 - 【e租年丰】理财精选',2314,'1元起投，14.2%预期收益，易投，易赚，易取',1,0,12,0,2,1,14.2,0,0),
	(1908,'e路同心 - 活期宝-T+0起息',2302,'同心活期宝,国企担保,随存随取',100,0,1,0,2,1,6.88,1,0),
	(1909,'金海贷 - 大型实业集团债权转让融资(12月标) ',2325,'金海贷提供稳健安全的高收益理财产品，专注P2P理财',100,0,12,0,2,1,18,0,0),
	(1910,'巨涟金融  - 【会存】活期理财',2326,'8.8%活期理财，随存随取，注册立送15000元体验金',1,0,1,0,2,1,8.8,0,0),
	(1911,'懒财信息 - 活期理财-【T+0计息 随存随取】',2327,'7x24h可取，限额60万，首信易支付托管，君联资本入股',1,0,1,0,2,1,8,0,0),
	(1912,'e路同心 - 【理财周专享标C】',2302,'理财周特供标[专享]高收益,国企担保',100,0,3,0,2,1,10,2,0),
	(1913,'易通贷  - 中小企业短期资金周转-AA',2305,'放心投易通贷，100元起投，年化收益17.9%，注册送红包',100,0,3,0,2,1,16.5,2,0),
	(1914,'银湖网 - 【新手专享项目】',2328,'新用户注册送50红包,满额送大礼',100,0,2,0,2,1,13,0,0),
	(1915,'平安陆金所 - 【点金计划3】',-1,'一键组合投资\"稳盈-安e\"加\"陆金宝\"赚取更高收益',30000,0,6,0,2,1,9.5,2,0),
	(1916,'付融宝  - 活期活利宝',2313,'活利宝,活期理财,7%年化收益,50元起投,7天可赎回.',50,0,1,0,2,1,7,1,0),
	(1917,'e路同心 - 活期理财-灵活存取',2302,'同心活期宝,国企担保,随存随取',100,0,1,0,2,1,6.88,1,0),
	(1918,'金联储 - 10天理财项目',2329,'10天理财标特享8%年化收益,时间短,收益高',100,0,1,0,2,1,8,0,0),
	(1919,'全民财富 - 全民新手体验标',2330,'15%年化收益率，注册就送10000元',100,0,1,0,2,1,15,2,0),
	(1920,'爱投资 - 【短期理财-3个月】',2331,'3个月短期理财,按月付息，时间短回本快，投资放心',100,0,3,0,2,1,10,2,0),
	(1921,'三分贷  - 周末理财',2321,'线下真实车贷抵押.注册就送88现金红包',100,0,6,0,2,1,17.8,2,0),
	(1922,'91旺财 - 新手专享',2332,'91旺财-中国最安全的P2P理财平台',100,0,1,0,2,1,12,0,0),
	(1923,'三分贷  - 新手专享短期理财推荐AAA',2321,'百元起投，专属特供，期限灵活，安全无忧',100,0,1,0,2,1,17.8,2,0),
	(1924,'e路同心 - 活期宝-灵活存取',2302,'同心活期宝,国企担保,随存随取',100,0,1,0,2,1,6.88,1,0),
	(1925,'全民财富 - 全民月月涨',2330,'30天之后每月可取，收益逐月递增0.5%',100,0,1,0,2,1,12.5,2,0),
	(1926,'点融网  - 季季翻投资团',2312,'白领1万元，预计年化收益8%（利息复投开启约为8.32%）',1000,0,12,0,2,1,8,0,0),
	(1927,'妙资金融  - 【新手标】新手专享M2',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,17,2,0),
	(1928,'永利宝  - 【企利宝】企业经营周转贷',2317,'稳定收益，100元起投，注册即送100，首投返现308',100,0,3,0,2,1,12,0,0),
	(1929,'金海贷 - 新手专享理财',2325,'注册即送10元红包,投资还送20元及VIP',100,0,1,0,2,1,16,0,0),
	(1930,'妙资金融  - 【新手标】新手专享M5',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,17,2,0),
	(1931,'合力贷 - 企业经营贷',2333,'央行互联网金融专业委员，老牌P2P金融网贷平台',50,0,5,0,2,1,16,2,0),
	(1932,'e租宝 - 【e租财富】40-365天灵活理财',2314,'1元起投，12%预期收益，260万用户[明智选择]',1,0,1,0,2,1,12,0,0),
	(1933,'小牛在线 - 【优质债权标】',2307,'投资灵活，可提前赎回，注册即送200元红包',100,0,12,0,2,1,15,2,0),
	(1934,'君融贷 - 供应链-雅戈尔经销商融资',2316,'供应链金融项目，雅戈尔推荐经销商',100,0,1,0,2,1,15,0,0),
	(1935,'你我贷 - 嘉财有道双季盈',2311,'年化收益率高达14%,国家AA级P2P平台',2000,0,6,0,2,1,11,0,0),
	(1936,'易通贷  - 专业理财【精选标】',2305,'100元起投，年化收益16.5%，注册送50元，邀请再送20元',100,0,1,0,2,1,10.8,2,0),
	(1937,'妙资金融  - 【新手标】新手专享M6',2323,'100元起投，注册资金10亿，准金融机构',100,0,1,0,2,1,17,2,0),
	(1938,'普资金融 - 普资华企-e言九鼎',2318,'等额本息,风控把关,月月收益',1000,0,2,0,2,1,14.4,1,0),
	(1939,'积木盒子 - 核心商圈贷',2306,'收益快,次日计息,100元起投,大型担保公司提供担保',100,0,6,0,2,1,12,2,0),
	(1940,'生菜金融  - 金财区-高投入,高回报',2334,'收益高,期限灵活,安全,',30000,0,6,0,2,1,9.8,2,0),
	(1941,'口袋理财  - 商业地产稳盈',2324,'千元做房东，享上海黄金地段商铺租金，固定浮动双收益',1000,0,36,0,2,1,20,2,0),
	(1942,'网信理财 - 新手专享【3天短期标】',2335,'建行入股企业，银行级安全系数',100,0,1,0,2,1,8,2,0),
	(1943,'懒财信息 - 1分起投-【限额60万】',2327,'1分起投，7X24可取，首信易支付托管，君联资本入股',1,0,1,0,2,1,8,0,0),
	(1944,'巨涟金融  - 信用专享标【精选】',2326,'涟商贷_信用专享标，收益好，可投期限多',100,0,3,0,2,1,18,2,0),
	(1945,'e租宝 - 【e租富享】理财精选',2314,'1元起投，13.4%预期收益，易投，易赚，易取',1,0,3,0,2,1,13.4,0,0),
	(1946,'永利宝  - 【房利宝】房产抵押周转贷',2317,'100元起投，注册即送100，首投返308，专业平台',100,0,6,0,2,1,12.5,0,0),
	(1947,'银湖网 - 机构合作标',2328,'风险备用金,多重保证,债权转让灵活',100,0,3,0,2,1,15,0,0),
	(1948,'昂道招财猫  - 活零活现【灵活理财】',2336,'1-30天超短期项目,投资1千立返50现金,投1w立返200现金',1000,0,1,0,2,1,9,2,0),
	(1949,'小牛在线 - 【活期牛】',2307,'货币基金理财,随时赎回,当日提现,次日到账,1元起投',1,0,1,0,2,1,8,0,0),
	(1950,'积木盒子 - 企业经营贷 第二期',2306,'收益高,次日计息,100元起投,大型担保公司提供担保',100,0,12,0,2,1,12.5,2,0),
	(1951,'金联储 - 1个月理财产品',2329,'1月理财标,会员专属.月月都有新标上线',100,0,1,0,2,1,9,0,0),
	(1952,'积木盒子 - 消费周转贷',2306,'收益快,次日计息,100元起投,大型担保公司提供担保',100,0,2,0,2,1,9.5,2,0),
	(1953,'黄河金融  - 【上市公司平台】新手宝',2337,'收益高达14%，与浙商保险合作，为用户保驾护航',500,0,3,0,2,1,14,2,0),
	(1954,'平安陆金所 - 【14日聚财】',-1,'1元起投随存随取，历史收益高于货币基金',1,0,1,0,2,1,5,0,0),
	(1955,'全民财富 - 全民半月赚',2330,'联合银行打造，实物质押，定期高收益，资金安全有保障',100,0,1,0,2,1,6.8,2,0),
	(1956,'e租宝 - 【e租财富】短期精选',2314,'1元起投，12%预期收益，260万用户[明智选择]',1,0,1,0,2,1,12,0,0),
	(1957,'妙资金融  - 【新手标】新手专享M3',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,17,2,0),
	(1958,'点融网  - 经典投资团',2312,'白领1万元，1万投资3个月7%年化收益172元',5000,0,3,0,2,1,7,0,0),
	(1959,'口袋理财  - 月盈宝38期',2324,'上市公司背书安全有保障,注册即送100元红包',1000,0,1,0,2,1,8,2,0),
	(1960,'妙资金融  - 【新手标】新手专享M4',2323,'八年专注理财,准金融机构,交易0手续费,注册送10000',100,0,1,0,2,1,17,2,0),
	(1961,'凤凰信用 - 金融工场-友居贷',2338,'金融工场,新手理财,100起投,注册送120.',100,0,1,0,2,1,8.8,2,0),
	(1962,'全民财富 - 全民全年享',2330,'联合银行打造，实物质押，定期高收益，资金安全有保障',5000,0,12,0,2,1,13,2,0),
	(1963,'银湖网 - 线上认证标',2328,'A股上市公司\"熊猫烟花\"背景，实力派',100,0,8,0,2,1,14,0,0),
	(1964,'e租宝 - 【e租财富】理财精选',2314,'1元起投，12%预期收益，260万用户[明智选择]',1,0,1,0,2,1,12,0,0),
	(1965,'巨涟金融  - 定期宝 100元起投',2326,'巨涟金融_高收益本息保障（10%-20%年化收益）',100,0,3,0,2,1,18,2,0),
	(1966,'凤凰信用 - 金融工场-易赚盈',2338,'年化15%高收益,上市公司背景,银行存管资金,低风险',100,0,12,0,2,1,14,2,0),
	(1967,'普资金融 - 普资华企-限时标',2318,'100起投,周期短,14%年利率',100,0,2,0,2,1,14,1,0),
	(1968,'黄河金融  - 【上市公司平台】担保贷',2337,'收益高达10%，与浙商保险合作，为用户保驾护航',1000,0,1,0,2,1,10,0,0),
	(1969,'全民财富 - 全民单月涨',2330,'联合银行打造，实物质押，定期高收益，资金安全有保障',100,0,1,0,2,1,8,2,0),
	(1970,'全民财富 - 全民双季盈',2330,'联合银行打造，实物质押，定期高收益，资金安全有保障',5000,0,6,0,2,1,11.5,2,0),
	(1971,'口袋理财  - 口袋新手宝',2324,'新手专项，利率加百分之一',1000,0,1,0,2,1,9.5,2,0),
	(1972,'全民财富 - 全民单季赢',2330,'联合银行打造，实物质押，定期高收益，资金安全有保障',100,0,3,0,2,1,11,2,0),
	(1973,'新联在线 - 全球化互联网理财平台',2319,'注册领10000元理财基金',50,0,3,0,2,1,17,0,0),
	(1974,'巨涟金融  - 7天新手体验标',2326,'7天新手标,12-20%年化收益',100,0,1,0,2,1,18,2,0),
	(1975,'妙资金融  - 【新手标】新手专享M7',2323,'年化高达17%，注册送1万，理财正规军',100,0,1,0,2,1,17,2,0),
	(1976,'积木盒子 - 房产抵押贷',2306,'收益快,次日计息,100元起投,大型担保公司提供担保',100,0,3,0,2,1,10,2,0),
	(1977,'三分贷  - 中小企业车辆抵押资金周转精选型03-AAA',2321,'稳健收益，车辆质押，17.8%年化，还有奖励',100,0,6,0,2,1,17.8,2,0),
	(1978,'小牛在线 - 【安居宝】',2307,'红本赎楼理财产品，超强风控，100元低门槛',100,0,1,0,2,1,14,2,0),
	(1979,'三分贷  - 中小企业车辆抵押资金周转精选型02-AAA',2321,'安全第一，加大年化，质押保障，稳健收益',100,0,3,0,2,1,17.8,2,0),
	(1980,'妙资金融  - 【新手标】新手专享M10',2323,'100元起投,八年专注理财,安全有保障,交易无手续费',100,0,1,0,2,1,17,2,0),
	(1981,'妙资金融  - 【新手标】新手专享M9',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,17,2,0),
	(1982,'e租宝 - 【e租稳盈】理财精选',2314,'1元起投，9%预期收益，易投，易赚，易取',1,0,1,0,2,1,9,0,0),
	(1983,'金海贷 - 稳健型6月标',2325,'金海贷提供稳健安全的高收益理财产品，专注P2P理财',100,0,6,0,2,1,17,0,0),
	(1984,'三分贷  - 中小企业房产抵押资金周转稳健型09-AAA',2321,'短期小额项目，有质押有保障',50000,0,1,0,2,1,17.8,2,0),
	(1985,'新联在线 - 上市公司产品-保利通',2319,'联合上市公司打造，全新理财产品',50,0,1,0,2,1,18,0,0),
	(1986,'凤凰信用 - 金融工场-利随享',2338,'金融工场理财，8%-15%高收益，首投1万，再送280元',100,0,3,0,2,1,11,2,0),
	(1987,'三分贷  - 假期计划',2321,'安全为本，有质押有保障，无忧理财尽在三分贷',100,0,6,0,2,1,17.8,2,0),
	(1988,'金海贷 - 资产管理公司债权融资一期',2325,'金海贷专注P2P理财,现注册立送10元,投资再送20元和VIP',100,0,12,0,2,1,15.87,0,0),
	(1989,'懒财信息 - 随存随取-【银行理财同级安全】',2327,'1分起投，限额60万，首信易支付托管，君联资本入股',1,0,1,0,2,1,8,0,0),
	(1990,'易通贷  - 中小企业短期资金周转-AAA',2305,'放心投易通贷，100元起投，年化收益11.8%，注册送红包',100,0,1,0,2,1,11.8,2,0),
	(1991,'全民财富 - 全民双月涨',2330,'联合银行打造，实物质押，定期高收益，资金安全有保障',100,0,2,0,2,1,9.5,2,0),
	(1992,'三分贷  - 中小企业车辆抵押短期型10-AAA',2321,'短期理财，周转灵活，年化17.8%，放心无忧',200000,0,1,0,2,1,17.8,2,0),
	(1993,'网信理财 - 新手专享【3天理财项目】',2335,'信诚保险战略合作，300万用户[明智选择]',100,0,1,0,2,1,8.8,1,0),
	(1994,'投宝金融 - 【短期理财-2个月】',2339,'国资平台,国有担保,银联托管,稳保高收益',100,0,3,0,2,1,16,2,0),
	(1995,'三分贷  - 新手专享短期理财精选推荐03-AAA',2321,'投资理财稳如山，持续收益拿不停',100,0,3,0,2,1,17.8,2,0),
	(1996,'三分贷  - 周末计划',2321,'线下真实房产抵押.注册就送88现金红包',100,0,6,0,2,1,17.8,2,0),
	(1997,'三分贷  - 汇利安心理财计划B',2321,'安全无忧，实物质押，固定资产，保驾护航',100,0,6,0,2,1,17.8,2,0),
	(1998,'三分贷  - 汇利优选信贷',2321,'稳定收益，盈利加大，质押为本，创收共赢',100,0,3,0,2,1,17.8,2,0),
	(1999,'银湖网 - 实地认证标',2328,'尽大限度控制风险,标的真实,收益高',100,0,6,0,2,1,11,0,0),
	(2000,'网信理财 - 中长期精选理财项目',2335,'投资100元，三天赚回130元，让赚钱速度飞起来',100,0,6,0,2,1,12,2,0),
	(2001,'口袋理财  - 年盈宝7期 ',2324,'新用户投资定期送10000体验金，老用户投多少送多少',1000,0,12,0,2,1,11,0,0),
	(2002,'投宝金融 - 【企业补充标】',2339,'国资背景,17%年化收益,1OO元起投,稳赚易投',100,0,3,0,2,1,17,2,0),
	(2003,'三分贷  - 中小企业房产抵押资金周转稳健型05-AAA',2321,'理财好收益，尽在三分贷',10000,0,1,0,2,1,17.8,2,0),
	(2004,'三分贷  - 中小企业短期资金周转02-AAA',2321,'百元起投，新手88红包，VIP一站式体验',100,0,1,0,2,1,17.8,2,0),
	(2005,'妙资金融  - 【新手标】新手专享M1 ',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,17,2,0),
	(2006,'投宝金融 - 投宝网-【活宝理财】A',2339,'国资背景,国有担保,双层风控,安全理财',1,0,5,0,2,1,18,0,0),
	(2007,'黄河金融  - 【上市公司平台】零钱包',2337,'收益高达8%，与浙商保险合作，为用户保驾护航',1,0,1,0,2,1,8,0,0),
	(2008,'投宝金融 - 投宝网-【新手理财精选产品】',2339,'国资背景,银联托管,1元起投,按秒计息,随存随取',1,0,15,0,2,1,18,0,0),
	(2009,'三分贷  - 中小企业短期资金周转10-AAA',2321,'利息多一点，活动丰富点，平台更民主一点',50000,0,3,0,2,1,17.8,2,0),
	(2010,'小牛在线 - 【企业信用贷项目】',2307,'100元起，年化收益15%，到期一次性还本付息',100,0,12,0,2,1,15,2,0),
	(2011,'黄河金融  - 【上市公司平台】车融贷',2337,'收益高达12%，与浙商保险合作，为用户保驾护航',1000,0,6,0,2,1,12,0,0),
	(2012,'妙资金融  - 【新手标】新手专享M11',2323,'八年专注理财,准金融机构,交易0手续费,注册送10000',100,0,1,0,2,1,17,2,0),
	(2013,'三分贷  - 中小企业车辆抵押资金周转稳健型11-AAA',2321,'苹果watch，ipad,mini2等给力奖品大放送',200000,0,3,0,2,1,17.8,2,0),
	(2014,'三分贷  - 中小企业房产抵押资金周转稳健型06-AAA',2321,'更多奖励，更多活动，尽在理财专家三分贷',10000,0,3,0,2,1,17.8,2,0),
	(2015,'精融汇  - 【定存赚理财计划】华安财险承保,保本息优',2340,'华安财险全额承保,上市公司背景,新浪支付托管',100,0,6,0,2,1,7.8,2,0),
	(2016,'妙资金融  - 【新手标】新手专享M8',2323,'新手专享,100元起投,八年专注理财,安全有保障',100,0,1,0,2,1,17,2,0),
	(2017,'妙资金融  - 【新手标】新手专享M14',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,17,2,0),
	(2018,'妙资金融  - 【新手标】新手专享M13',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,17,2,0),
	(2019,'e租宝 - 【e租稳盈】短期精选',2314,'1元起投,2至365天灵活理财，易投，易赚，易取',1,0,1,0,2,1,9,0,0),
	(2020,'三分贷  - 中小企业车辆抵押短期型11-AAA',2321,'年化17.8%，活动丰富，奖励多多，6splus等你来拿',500000,0,1,0,2,1,17.8,2,0),
	(2021,'金联储 - 固融金项目',2329,'（金联储）亿元安保金现行垫付,专注P2B理财注册送百元',100,0,3,0,2,1,10,2,0),
	(2022,'合力贷 - 企业经营标',2333,'老牌P2P金融网贷平台,央行金融专业委员会委员',50,0,2,0,2,1,15,2,0),
	(2023,'三分贷  - 中小企业短期资金周转精选型05-AAA',2321,'优质理财，安全有保障，利益有新高，投资方便快捷',50000,0,6,0,2,1,17.8,2,0),
	(2024,'三分贷  - 中小企业短期资金周转12-AAA',2321,'时间累积财富，质押安全保障，稳如山般的收益',50000,0,6,0,2,1,17.8,2,0),
	(2025,'投宝金融 - 【活宝理财产品】',2339,'国资平台,国有担保,银联托管,灵活转让,稳赚易投',100,0,14,0,2,1,17.03,2,0),
	(2026,'小牛在线 - 【安逸信】',2307,'100元起投，年化10%-15%，注册即送200元红包',100,0,1,0,2,1,10.4,2,0),
	(2027,'三分贷  - 中小企业车辆抵押资金周转稳健型12-AAA',2321,'我们高的不仅仅是收益，还有安全和保障',500000,0,3,0,2,1,17.8,2,0),
	(2028,'三分贷  - 中小企业短期资金周转13-AAA',2321,'加强利息和奖励，财富创收，安全保障',100000,0,3,0,2,1,17.8,2,0),
	(2029,'三分贷  - 中小企业房产抵押资金周转稳健型02-AAA',2321,'用心做理财，给客户多一分利益，让客户多一份欢笑',100,0,3,0,2,1,17.8,2,0),
	(2030,'三分贷  - 中小企业车辆抵押资金周转稳健型13-AAA',2321,'理财新体验，安全新标准，奖励新时尚',200000,0,6,0,2,1,17.8,2,0),
	(2031,'三分贷  - 中小企业房产抵押资金周转稳健型07-AAA',2321,'在三分贷，你的钱将更加努力的工作，也会增长的更快',10000,0,6,0,2,1,17.8,2,0),
	(2032,'金联储 - 3个月理财项目',2329,'金联储P2B理财，100元起投，注册送100元',100,0,3,0,2,1,10,0,0),
	(2033,'巨涟金融  - 新手体验标 保本息',2326,'涟网贷，收益高，招行千万风险备用金保障，放心理财',100,0,6,0,2,1,12,2,0),
	(2034,'三分贷  - 中小企业车辆抵押资金周转稳健型14-AAA',2321,'最高送iphone6Splus，无限精彩，理财生活新体验',500000,0,6,0,2,1,17.8,2,0),
	(2035,'黄河金融  - 【上市公司平台】小微贷',2337,'收益高达11%，与浙商保险合作，为用户保驾护航',1000,0,3,0,2,1,11,0,0),
	(2036,'投宝金融 - 【短期理财-1个月】',2339,'国资平台,国有担保,银联托管,稳定加收益',100,0,3,0,2,1,15,2,0),
	(2037,'三分贷  - 中小企业短期资金周转精选型06-AAA',2321,'实时数据，安全透明，贴心服务，轻松理财',100000,0,6,0,2,1,17.8,2,0),
	(2038,'口袋理财  - 季盈宝29期 ',2324,'收益快，安全高，一键投资享高收益更放心,注册送100元',1000,0,3,0,2,1,9.5,2,0),
	(2039,'网信理财 - 行业龙头经销商短期资金周转',2335,'100起投，注册就送红包，首投最高返150元',100,0,3,0,2,1,11,2,0),
	(2040,'金联储 - 供应链金融',2329,'专注金融产业链投资理财，315金银岛为您保航护驾',100,0,6,0,2,1,10,2,0),
	(2041,'新联在线 - 【1~3月短期理财】',2319,'棕榈树网贷评级正面AAA级,年度最佳P2P网络贷款平台',50,0,1,0,2,1,18,0,0),
	(2042,'口袋理财  - 活期口袋宝',2324,'[一元起投][随用随取]更快捷更方便',1,0,1,0,2,1,8,0,0),
	(2043,'巨涟金融  - 18天新手体验标',2326,'18天新手体验标,12-20%年化收益',100,0,1,0,2,1,18,2,0),
	(2044,'妙资金融  - 【天天赢】灵活理财M8',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,6.5,2,0),
	(2045,'小牛在线 - 【安商信】',2307,'本息担保,期限灵活,投资起点低.',100,0,1,0,2,1,10.4,2,0),
	(2046,'小牛在线 - 【安车信】',2307,'精选汽车抵押贷款,15%年收益,保本保息',100,0,3,0,2,1,12,2,0),
	(2047,'新联在线 - 【新手理财专区】',2319,'注册即送10000元理财金',50,0,1,0,2,1,18,0,0),
	(2048,'投宝金融 - 稳健型9月标',2339,'17%年收益，国资背景，国有担保，银联托管，投资无忧',100,0,3,0,2,1,17,2,0),
	(2049,'懒财信息 - 懒财宝-【1分起投】',2327,'1分起投，随存随取，首信易支付托管，君联资本入股',1,0,1,0,2,1,8,0,0),
	(2050,'投宝金融 - 新手标【新手专享】',2339,'15%年收益，国资背景，国有担保，银联托管，投资无忧',100,0,3,0,2,1,15,2,0),
	(2051,'小牛在线 - 【小额项目】',2307,'小额抵押理财产品,100元起,15%年化收益,安全有保障',100,0,6,0,2,1,13.2,2,0),
	(2052,'投宝金融 - 【活宝专享】',2339,'0元起投,随存随取,按秒计息,保本保息',1,0,10,0,2,1,15,0,0),
	(2053,'投宝金融 - 活宝-懒人理财',2339,'国资背景,按秒计息,随存随取,注册即送红包',1,0,1,0,2,1,10.99,0,0),
	(2054,'投宝金融 - 投宝网-【精选理财-月标】',2339,'国资背景,国有担保,1元起投,按秒计息,随存随取',1,0,7,0,2,1,17.99,0,0),
	(2055,'投宝金融 - 【企业补充标06】',2339,'国有担保,银联托管,1元起投,随存随取',1,0,1,0,2,1,17,1,0),
	(2056,'投宝金融 - 投宝网-【精选灵活理财产品】',2339,'国资背景,银联托管,按秒计息,随存随取,年化收益18%',1,0,3,0,2,1,12,0,0),
	(2057,'小牛在线 - 【车贷项目】',2307,'投资周期短,收益高,等额本息,注册即送200元红包',100,0,10,0,2,1,13.9,2,0),
	(2058,'投宝金融 - 投宝网-【精选新手理财标】',2339,'活宝理财,1元起投,按秒计息,随存随取',1,0,5,0,2,1,14.99,0,0),
	(2059,'投宝金融 - 投宝网-【新手标】新手理财',2339,'国资背景,银联托管,新手理财,低门槛高收益,稳赚易投',1,0,13,0,2,1,15,0,0),
	(2060,'金海贷 - 综合性实业集团融资（8月标）',2325,'金海贷提供稳健安全的高收益理财产品，专注P2P理财',100,0,8,0,2,1,16,0,0),
	(2061,'精融汇  - 【月月赚理财计划】华安财险承保,上市公司',2340,'上市公司系,华安财险全额承保,100元起投',100,0,12,0,2,1,9.8,1,0),
	(2062,'精融汇  - 【月月赚理财计划】保本息优选,100元起投',2340,'华安财险全额承保,上市公司系,100元起投',100,0,6,0,2,1,9.8,2,0),
	(2063,'妙资金融  - 【天天赢】灵活理财M5',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,6.5,2,0),
	(2064,'投宝金融 - 【活宝】新手专享理财',2339,'国资平台,银联托管,17%年收益,稳赚易投,稳定加收益',100,0,3,0,2,1,17,0,0),
	(2065,'小牛在线 - 【安企贷】',2307,'尽大限度控制风险,标的真实,收益高',100,0,6,0,2,1,14,2,0),
	(2066,'新联在线 - 【新人活动专区】',2319,'注册即送10000元理财金',50,0,1,0,2,1,18,0,0),
	(2067,'妙资金融  - 【天天赢】灵活理财M7',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,6.5,2,0),
	(2068,'投宝金融 - 【活宝理财E】',2339,'国资背景,银联托管,稳赚易投,保本加收益,0风险',100,0,1,0,2,1,12,2,0),
	(2069,'精融汇  - 【月月赚理财计划】华安财险承保,100元起投',2340,'华安财险承保,100元起投',100,0,12,0,2,1,9.8,2,0),
	(2070,'投宝金融 - 【新手专享】',2339,'国资背景,0元起投,按秒计息,随存随取',1,0,5,0,2,1,14.98,0,0),
	(2071,'投宝金融 - 【活宝计划】',2339,'国资平台,国有担保,银联托管,安全稳定,收益高',100,0,9,0,2,1,15.66,2,0),
	(2072,'投宝金融 - 投宝网-新手理财项目',2339,'17%年收益，国资背景，国有担保，银联托管，投资无忧',100,0,1,0,2,1,10,2,0),
	(2073,'投宝金融 - 【长期理财-年标】',2339,'国资背景,国有担保,银联托管,稳赚易投,安全放心',100,0,12,0,2,1,16,0,0),
	(2074,'投宝金融 - 【活宝】新手月入理财',2339,'国资背景,国有担保,银联托管,稳赚易投,安全有保障',100,0,10,0,2,1,16,0,0),
	(2075,'投宝金融 - 投宝网-【精选新手体验理财标】',2339,'国资背景,银联托管,按秒计息,随存随取,1元起投',1,0,16,0,2,1,5,0,0),
	(2076,'巨涟金融  - 【定期宝】',2326,'巨涟金融_定期宝，10%-20%年化收益',100,0,1,0,2,1,18,2,0),
	(2077,'投宝金融 - 新手体验',2339,'国资背景,国有担保,16%年收益,本息保障,灵活转让,',100,0,3,0,2,1,16,2,0),
	(2078,'投宝金融 - 【活宝】灵活理财A',2339,'国家CFCA认证,银联托管,稳赚易投',100,0,11,0,2,1,16.83,0,0),
	(2079,'投宝金融 - 【企业流动标】',2339,'国资平台,国有担保,银联托管,稳赚易投',100,0,6,0,2,1,17.03,2,0),
	(2080,'投宝金融 - 投宝网-【精选短期理财产品】',2339,'国资背景,银联托管,1元起投,18%年化收益,随存随取',1,0,1,0,2,1,5,0,0),
	(2081,'新联在线 - 新手专享1月标',2319,'1-3月短期理财产品推荐',50,0,1,0,2,1,18,0,0),
	(2082,'投宝金融 - 【短期理财】',2339,'国资背景,国有担保,银联托管,保本加收益,安全有保障',100,0,3,0,2,1,16,2,0),
	(2083,'投宝金融 - 投宝网-【精选活宝理财】',2339,'国资背景,银联托管,18%年化收益,按秒计息,随存随取',1,0,10,0,2,1,17.99,0,0),
	(2084,'投宝金融 - 投宝网-【长期理财】12个月',2339,'国资背景,国有担保,1元起投,按秒计息,随存随取',2,0,1,0,2,1,5,0,0),
	(2085,'投宝金融 - 投宝网-【精选理财】季度标',2339,'国资背景,银联托管,1元起投,按秒计息,随存随取',1,0,8,0,2,1,14.88,0,0),
	(2086,'金海贷 - 优企贷月息通系列',2325,'金海贷提供稳健安全的高收益理财产品，月息通新手优选',100,0,1,0,2,1,13,0,0),
	(2087,'投宝金融 - 【新手标】',2339,'16%年化收益,国有担保,银联托管,安全有保障',100,0,4,0,2,1,16,2,0),
	(2088,'君融贷 - 京东联想融资项目-20万起投稳健型',2316,'京东联想金融项目，注册即送百元红包，投资还送IWatch',200000,0,12,0,2,1,15,0,0),
	(2089,'投宝金融 - 投宝网-【新手理财】',2339,'国资背景,银联托管,保本加收益,稳赚易投,安全有保障',100,0,1,0,2,1,15,2,0),
	(2090,'投宝金融 - 投宝网-【灵活理财】AA',2339,'国资背景,银联托管,1元起投,按秒计息,随存随取',2,0,8,0,2,1,13.88,0,0),
	(2091,'投宝金融 - 投宝网-【低收入理财】',2339,'国资背景,1元起投,按秒计息,随存随取,灵活理财',2,0,4,0,2,1,12.88,0,0),
	(2092,'投宝金融 - 投宝网-【3个月标】',2339,'国资背景,银联托管,注册即送红包,随存随取,稳赚易投',1,0,1,0,2,1,7,0,0),
	(2093,'投宝金融 - 【活宝】灵活理财C',2339,'国资背景,银联托管,国有担保,稳定加高收益',100,0,4,0,2,1,9.98,0,0),
	(2094,'投宝金融 - 投宝网-【短期理财-三个月】',2339,'国资平台,国有担保,银联托管,稳定加高收益',100,0,3,0,2,1,16,0,0),
	(2095,'君融贷 - 京东联想融资项目-10万起投稳健型',2316,'京东联想金融项目，注册即送百元红包，投资还送IWatch',100000,0,3,0,2,1,15,0,0),
	(2096,'投宝金融 - 【活宝】抢购理财标',2339,'国资背景,低门槛,高收益,按秒计息,随存随取',1,0,1,0,2,1,15,0,0),
	(2097,'妙资金融  - 【天天赢】灵活理财M1',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,6.5,2,0),
	(2098,'妙资金融  - 【天天赢】灵活理财M3',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,6.5,2,0),
	(2099,'投宝金融 - 【活宝】稳妥理财M1',2339,'国资背景,银联托管,注册送红包,邀约即有机会赢取苹果6',1,0,4,0,2,1,12,0,0),
	(2100,'投宝金融 - 投宝网-【活宝】灵活理财A1',2339,'国资背景,注册即送红包,年化收益15%,随存随取',1,0,1,0,2,1,5,0,0),
	(2101,'投宝金融 - 投宝网-【新手理财M2】',2339,'16%年收益,国资背景,国有担保,保本加收益,安全有保障',100,0,4,0,2,1,16,2,0),
	(2102,'投宝金融 - 【短期理财-3个月】',2339,'国资背景,国有担保，银联托管,稳赚易投',100,0,10,0,2,1,12.69,2,0),
	(2103,'投宝金融 - 企业经营贷',2339,'国资背景,国有担保,0元起投,按秒计息,随存随取',100,0,12,0,2,1,12,2,0),
	(2104,'投宝金融 - 【投宝金融】短期精选',2339,'国资背景,银联托管,按秒计息,随存随取,注册送红包',1,0,1,0,2,1,7,0,0),
	(2105,'金海贷 - 建材销售企业融资',2325,'金海贷提供稳健安全的高收益理财产品，专注P2P理财',100,0,10,0,2,1,15.89,0,0),
	(2106,'投宝金融 - 投宝网-【精选长期理财产品】',2339,'1元起投,按秒计息,随存随取,国资背景,国有担保',1,0,8,0,2,1,12,0,0),
	(2107,'投宝金融 - 投宝网【企业经营标A】',2339,'国资背景,银联托管，稳定加高收益,稳赚易投',100,0,2,0,2,1,4.98,0,0),
	(2108,'投宝金融 - 投宝网-【活宝】家庭理财',2339,'国资背景,银联托管,按秒计息,随存随取,家庭理财首选',1,0,1,0,2,1,5,0,0),
	(2109,'投宝金融 - 【活宝理财M4】',2339,'联手民生,块钱倾力打造,按秒计息,随存随取',1,0,18,0,2,1,15,0,0),
	(2110,'投宝金融 - 【活宝理财D】',2339,'12%年收益,七天课提现,稳赚易投,国资背景,只赚不赔',100,0,12,0,2,1,12,2,0),
	(2111,'投宝金融 - 投宝网-【新手理财M4】',2339,'国资背景,银联托管,高收益低风险,稳赚易投',100,0,4,0,2,1,14.66,2,0),
	(2112,'妙资金融  - 【天天赢】灵活理财M6',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,6.5,2,0),
	(2113,'投宝金融 - 投宝网-【活宝理财B】',2339,'国资背景,0元起投,按秒计息,随存随取',1,0,1,0,2,1,7,0,0),
	(2114,'投宝金融 - 【长期理财-8个月】',2339,'国资平台,银联托管,稳赚易投,稳定加高收益理财',100,0,8,0,2,1,13.89,2,0),
	(2115,'投宝金融 - 投宝网-【活宝理财C】',2339,'16%年收益,七天可提,稳赚易投,灵活转让',100,0,3,0,2,1,15,2,0),
	(2116,'投宝金融 - 【活宝】灵活理财',2339,'0元起投,按秒计息,随存随取,国资背景',1,0,3,0,2,1,10,0,0),
	(2117,'投宝金融 - 【活宝】灵活理财B',2339,'100元起投,国资平台,银联托管,17%年化收益,安全无忧',100,0,1,0,2,1,7,0,0),
	(2118,'投宝金融 - 投宝网-【活宝理财产品M2】',2339,'携手民生,块钱倾力打造,按秒计息,随存随取',1,0,4,0,2,1,10.5,0,0),
	(2119,'投宝金融 - 【短期理财-季度标】',2339,'国资平台,银联托管,国有担保,稳定加高收益',100,0,2,0,2,1,13,2,0),
	(2120,'妙资金融  - 【天天赢】灵活理财M4',2323,'八年专注理财,准金融机构,交易0手续费,注册送1万红包',100,0,1,0,2,1,6.5,2,0),
	(2121,'投宝金融 - 投宝网-【活宝短期理财】',2339,'0元起投,随存随取,按秒计息,全球首款',1,0,6,0,2,1,13,1,0),
	(2122,'投宝金融 - 投宝网-【活宝】理财优质标',2339,'16%年收益,国资背景,国有担保,稳赚易投,随存随取',100,0,1,0,2,1,16,0,0),
	(2123,'投宝金融 - 【长期理财-12个月】',2339,'国资背景,按秒计息,随存随取,稳赚易投',1,0,15,0,2,1,7,0,0),
	(2124,'妙资金融  - 【月月满】稳妥理财M1',2323,'100元起投,八年专注理财,安全有保障,交易0手续费',100,0,1,0,2,1,8,2,0),
	(2125,'投宝金融 - 活宝-新手体验标',2339,'0元起投,随存随取,按秒计息,全球首款',1,0,3,0,2,1,10.5,0,0),
	(2126,'投宝金融 - 【3月短期标】',2339,'15%年收益,国资背景,银联托管,保本加收益,理财有保障',100,0,3,0,2,1,15,2,0),
	(2127,'投宝金融 - 投宝网-【活宝】新手标',2339,'国资平台,p2c理财,100元起投,随存随取,智能理财',100,0,1,0,2,1,12,0,0),
	(2128,'投宝金融 - 投宝网-【新手理财B】',2339,'国资背景,国有担保,银联托管,稳赚易投,投资即享收益',100,0,1,0,2,1,4.93,0,0),
	(2129,'投宝金融 - 【短期理财-4个月】',2339,'联手民生,块钱倾力打造,0元起投,按秒计息',1,0,10,0,2,1,12,0,0),
	(2130,'投宝金融 - 【企业补充标07】',2339,'国资平台,国有担保,银联托管,资金可灵活转让',100,0,10,0,2,1,17.01,2,0),
	(2131,'投宝金融 - 投宝网-【活宝理财产品】',2339,'国资背景,国有担保,0元起投,按秒计息,随存随取',100,0,1,0,2,1,8,0,0),
	(2132,'投宝金融 - 投宝网-企业补充标01',2339,'国资背景,国有担保,银联托管,稳定加高收益',100,0,6,0,2,1,4.99,0,0),
	(2133,'投宝金融 - 投宝网-【活宝A计划】',2339,'国资背景,国有担保,银联托管,注册有礼,投资即享收益',100,0,4,0,2,1,4.99,1,0),
	(2134,'投宝金融 - 投宝网【新手理财A】',2339,'国资背景,银联托管,国有担保,稳赚易投',100,0,1,0,2,1,4.97,0,0),
	(2135,'投宝金融 - 投宝网-【短期理财专享】',2339,'国资背景,国有担保,银联托管,0元起投,按秒计息',100,0,1,0,2,1,6,2,0),
	(2136,'投宝金融 - 【活宝B计划】',2339,'国资背景,按秒计息,全球首款,随存随取',1,0,1,0,2,1,7,1,0),
	(2137,'投宝金融 - 投宝网-企业流动标08',2339,'国资背景,银联托管,国有担保,高收益安全可靠',100,0,7,0,2,1,4.08,0,0),
	(2138,'投宝金融 - 【活宝理财M3】',2339,'0元起投,按秒计息，全球首款,随存随取',1,0,2,0,2,1,10,0,0),
	(2139,'投宝金融 - 活宝理财',2339,'国资背景,安全理财,0元起投,随存随取',1,0,7,0,2,1,8.9,0,0),
	(2140,'投宝金融 - 【活宝】短期理财',2339,'国资背景,国有担保,0元起投,按秒计息,随存随取',100,0,6,0,2,1,9.95,0,0),
	(2141,'投宝金融 - 投宝网-【新手理财M1】',2339,'国资背景,国有担保,银联托管,注册有礼,投资即享收益',100,0,5,0,2,1,4.95,0,0),
	(2142,'投宝金融 - 【企业流动标05】',2339,'民生,块钱倾力打造,0元起投,随存随取',1,0,2,0,2,1,10.3,0,0),
	(2143,'投宝金融 - 【活宝理财3期】',2339,'国资背景,国有担保,0元起投,按秒计息,随存随取',100,0,5,0,2,1,11.88,2,0),
	(2144,'投宝金融 - 投宝网-【新手理财专区】',2339,'国资平台,国有担保，银联托管,稳赚易投',100,0,13,0,2,1,4.99,2,0);

/*!40000 ALTER TABLE `fin_p2p_invest_contract` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table fin_p2p_lov
# ------------------------------------------------------------

DROP TABLE IF EXISTS `fin_p2p_lov`;

CREATE TABLE `fin_p2p_lov` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `code` int(11) DEFAULT NULL,
  `name` varchar(50) DEFAULT NULL,
  `type` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `fin_p2p_lov` WRITE;
/*!40000 ALTER TABLE `fin_p2p_lov` DISABLE KEYS */;

INSERT INTO `fin_p2p_lov` (`id`, `code`, `name`, `type`)
VALUES
	(1,1,'投资金额',NULL),
	(2,2,'投资周期',NULL),
	(3,3,'预期收益',NULL),
	(4,0,'0-5万元',1),
	(5,1,'5-10万元',1),
	(6,2,'10-20万元',1),
	(7,3,'20-50万元',1),
	(8,4,'50万元以上',1),
	(9,0,'1个月以内',2),
	(10,1,'1-3个月',2),
	(11,2,'3-6个月',2),
	(12,3,'6个月-1年',2),
	(13,4,'1年以上',2),
	(14,0,'0-5%',3),
	(15,1,'5-10%',3),
	(16,2,'10-15%',3),
	(17,3,'15%以上',3);

/*!40000 ALTER TABLE `fin_p2p_lov` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table fin_p2p_master
# ------------------------------------------------------------

DROP TABLE IF EXISTS `fin_p2p_master`;

CREATE TABLE `fin_p2p_master` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  `official_url` varchar(1000) NOT NULL DEFAULT '',
  `product_description` varchar(100) DEFAULT NULL COMMENT '产品描述',
  `duration_description` varchar(100) DEFAULT NULL COMMENT '投资周期描述\n',
  `invest_area` varchar(50) DEFAULT NULL,
  `platform_est_rate` float DEFAULT NULL COMMENT '平台预期收益',
  `platform_min_amount` float DEFAULT NULL,
  `platform_icon_url` varchar(100) DEFAULT NULL,
  `risk_rank` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `fin_p2p_master` WRITE;
/*!40000 ALTER TABLE `fin_p2p_master` DISABLE KEYS */;

INSERT INTO `fin_p2p_master` (`id`, `name`, `official_url`, `product_description`, `duration_description`, `invest_area`, `platform_est_rate`, `platform_min_amount`, `platform_icon_url`, `risk_rank`)
VALUES
	(2302,'e路同心','http://www.baidu.com/zhixin.php?url=000000aRo4khh0sbkRjlJeMnaVxABBt-HEstFdfQ5Z9SA1ALddJmUi6CD6c5tEhdMtWOaBhLCotNoQ9gF3T3ddZI6jv3V2hSHG2UtgRJeO5Xyuvgu9hQdoGSPStOppgyNKWh5HD.Db_ju_EuBeRfRgs_Eek3vQvTya7-SEKdCnlhQvTya7-SEKb_RdrYG4TXGmuCy2qMA1G0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcsP1bvnHf4nj60ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYk0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWcznHmsPWTdrj60pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YdPHD1rf','理财周特供标[专享]高收益,国企担保','','',0,0,'http://t10.baidu.com/it/u=3580911948,1958448498&fm=58 ',0),
	(2303,'PPmoney','http://www.baidu.com/zhixin.php?url=000000aGDTfe1n7PfpQNnFG9sOHdm6fYitzMj5no0TDgcYAnm0ORoNI_YwB26kS2tDuKrh_uA6ll2ZWVmLetRYDUGZQg2pJfnltqw1sSGe4W4f7Rde0i982qX4n82025pQEKQDc.7b_ifkR5QZWY8BHCwKZWHaqhWKY5BWJI64qt7jHzk8sHfGmqx-OcO3ZgKfYt8-P1tA-W4xMl32AM-9I7fH7fmzU-PHV2XgZJyAp7WIugElf.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcsP1bvnHf4nj60ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYk0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWRLnjDkn1bsnHf0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YkrHf3r0','注册就送8888元,投资送60元红包','','',0,0,'http://t12.baidu.com/it/u=3113540738,2486375800&fm=58',0),
	(2304,'爱钱帮','http://www.baidu.com/zhixin.php?url=000000aT6U8kyqQMqiB4iOioGiA6jgwCP6tYmS1xRgRUcR8mYdOYBar3XRwwWalVvC-H4MkR1R1-ezSE50qQXD5sbkvwl8js_majHA-NErGrZOidl7iG4vabWbZTYjodJsooEa0.Db_KO_X17X1hwKLswGhd1rY_g_3_DQQqXKHReiM-kl-9h9movNeJs0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcsP1bvnHf4nj60ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYk0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWR1nWm1PHbYP1b0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YvnWDs','银行存管,上市公司盛大投资,注册送88元,当日计息.','','',0,0,'http://t11.baidu.com/it/u=2971430797,3985661381&fm=58',0),
	(2305,'易通贷 ','http://www.baidu.com/zhixin.php?url=0000000liLV4kwMhTBSyk9UxpAeSx0gxyzblL6BKmU5vYzr6K1Ic9NMOia6QNMgqHEP2GOy-NlA36ZeMqpbk9xP-30pxDzEnP2mvxxhILuojCvP57lBl8TpG-xAd46A1_v8CZHf.7D_in3txbh6ePTzqDZxgnqnNLL8mOBxCp_M4TE4tMqZtejahGv-5QWdQjPakvyU5ZG0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcsP1bvnHf4nj60ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYk0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWRkn10YnWb4nWn0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YYnWTk','放心投易通贷，100元起投，年化收益16.5%，注册送红包','','',0,0,'http://t11.baidu.com/it/u=2938314220,671126516&fm=58 ',0),
	(2306,'积木盒子','http://www.baidu.com/zhixin.php?url=00000000b1nNisEYSqi_RkSp20I8V_s1hJUu-ucrzgdooy4-cJBTEnw7EHGpWPLF2dSXW-Yud33fYc6Rq2WY3e9qJINXN2j0eXUP7cYN1y2dPwayHML2yDK9HpepKHbO9B23ci_.7b_NR2Ar5Od663rj6t_oqraqLfBjOuWHPKOoHJxZfHPLC_kg51nXLqUnN1tLgyk-x7nePbl32AM-WI6h9ik-x7nePbl32AM-YG8x6Y_f33X8a9G4myIrP-SJFBz8EGl_l8gV3nHA1ko1tTOHj__LsebqPvi1k_Lq5-vTU8zO_lyIdLAE_ovg2pMpRt85R_nYQZu_3txD.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcsP1bvnHf4nj60ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYk0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqrjRYPj6vnjc4P6KGTdqLpgF-UAN1T1Ys0AN3Ijd9mvP-TLPRXgK-rhdsr1VsIh-brWDYPjR1nWRsPWn1PWDkP1nsP1cerv-1N-PfrW0erLKzUvwdmLwRXgK-rWnknjcerLw1Tv-brW00mLFW5HRsPWnz','新手专享,100元起投,理财新选择,安全有保障','','',0,0,'http://t12.baidu.com/it/u=618571349,2129196358&fm=58',0),
	(2307,'小牛在线','http://www.baidu.com/zhixin.php?url=0000000UDuM13u57rd3-5sx4HPtRhWcGxCDMrM9pgB7KzMV8OecxMURWn1njiN8yJ_yZpj25RL9U7CAq9d6H52wTIZdhYY7WFXYZH4mjGV3D30piXxhYoqnd2Hi5rf7zkfz_udT.7b_jCXzlFSju_wKoxD17aqhWKwuB8xh6ksHFd5WCFhesdnkwFCOX4WFx_en-Segx8zfeEZWbltjGohrMy1u3qPheFkzypSri7HReiM-kl-9h9mlpSz8_.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcsP1bvnHf4nj60ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYk0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWR3PHfYnWfsnjf0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YdPWD1n6','100起投,收益高,期限灵活,安全有保障','','',0,0,'http://t11.baidu.com/it/u=1368362572,1796150291&fm=58',0),
	(2308,'陆金所','http://www.baidu.com/zhixin.php?url=000000j47d4EwfyumL5taQbBttkrQPv07tYpKT1m6Fl0Q7PwCRDLg6c2oHGp1XKkC2OataKRmwkqORqDDnlTKyinWwhxGG6oAelxoswmtMT7bQ9aN9_VvCA9VJ7FviFXR7kyhEm.DD_inlqUlQvpax2W6w01jfXlQ8zkMolYAnOW3erMJeZfmLImvnMuvTIOyWWmJCRnTXZB19zptrHg6WHP4RTanrHg6WHP9zplrrurGtIKnvZWou9C5I793LRTahGv-5QWdQjPakbtILWb.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcsP1bvnHf4nj60ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYk0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnHmvnjTYPW61nWf0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1Yvnjm4','一键组合投资\"稳盈-安e\"加\"陆金宝\"赚取更高收益','','',0,0,'http://t12.baidu.com/it/u=2263955969,2201160335&fm=58',0),
	(2309,'合时代','http://www.baidu.com/zhixin.php?url=0000000j-TROGKVhvQh71kZCdcF5gfac4hGV3npTFhrng8Xh11NrZTrPRuZFZOOcxK7KHu-Frq7s3uRe0wO-ifDaiLUZCNqt_GlIP5ZwYXwTKNj0w5fXtvPvVs8ZGKlWIMH3V83.7D_iwt5-RhroDzqD7ZZKwkq55EnMyhpun1D5ZGLUtMWuY_qPh1G8ol32AM-9uY3vglCh5ktO3MgKfYt_U_DY2yQvTya7-SEKdsRP5QGHTOKGm9ksqT7jHzYD1pyn2XMjElTPqh75H9vUtrzOaMGs_vmIMBGJIGHz3qis1f_NqPhzs0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcsP1bvnHf4nj60ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYk0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWRzrjbvnWbvP1c0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YznHbLns','新手专享8天超短期理财，100元起投，18%年化收益','','',0,0,'http://t12.baidu.com/it/u=519643589,258886245&fm=58',0),
	(2310,'玖富','http://www.baidu.com/zhixin.php?url=000000K3sfM06xekUFOmF-wShqxcJ3Z-kxz3k6SBe9eNLiQJkDYqYMEo3jxwwHJXYIfIU1eIMKc6ku3hGJJfBsCmDrUmaxBC5-yiGhpxnpP2Nou8hVQsuHeyfjvkIDnaCag7ufs.DD_NR2Ar5Od66vXyOgX17vC6kdrBHvxlYlMkYD1TSOgKfYt_U_DY2yQvTyaZgdsRP5QGHTOKGmksqT7jHzs_lTUQqRHZ_Cf3S8a9G4mgwRDkRA2Aoo6CpXy6hUikfdTNPHV2XgZJyAp7WIYqris.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcsP1bvnHf4nj60ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYk0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWT4rjcYrjfLPWc0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YvPHfvn0','玖富9年安全,21天特供标,15%年化收益,注册送150元红包','','',0,0,'http://t10.baidu.com/it/u=987352662,3844233168&fm=58 ',0),
	(2311,'你我贷','http://www.baidu.com/zhixin.php?url=000000amzIbfgm_8iXSKIvEgdYaOOnMc_1fY1z0ysvsOLZSn8BwsiVSkzAQxprAzytUKierCBShCIrQMUU6GAvw4PWd3q1us1ZqoY3mEEz21figJZDVRD_13ma-I9oVm_PZcgkn.Db_aFO0l4-lcrSa9G4mLmFCR_g_3_7j93pMqj388a9G4I2UM3PQDrrZo6CpXy7YNKnNKWj4SrZxEsSEM9tqvZdq8ZvqrZ1tTrOl3xU8sSXejlOgj_33x5kseenr1GsSX1jlqhZv4r8a9G4myIrP-SJFWj4e_rOu9vUt5M_sSEu9q8Hj4qrZxtrZuEsSXOj_oseSrZ1lTrO3Tr1u9tSMjESZj_vmx5kseOgj_v2pMpRt85R_nYQ7X1Fb0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcsP1bvnHf4nj60ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYk0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnHfdnjbkPWRzPHT0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YYP1csn0','年化收益率高达14%,国家AA级P2P平台','','',0,0,'http://t11.baidu.com/it/u=16430590,1820411262&fm=58',0),
	(2312,'点融网 ','http://www.baidu.com/zhixin.php?url=000000jMXJTWyXJfOxenf128Q8weiIWqmPcOP4wm2pJLnKjse4o9y0BI2nLVKDwx-rwzdpE0Z06Kx8S7qegEMG22c5XMrqeha8ruA9gW0Cbef2_INvSNID1fGGxkyZibAPQlldb.7R_a4enR1_zc2QnN1tY7t1nNvizEIugKLCn1pbYAcAmmgmcJDAMwMu99I6BCmTMAKhA7va29MWspIQnvWonMyhpuPLoCmYtEoS81Fkl32AM-WI6h9iksqZgKfYt8-P1tA-BPrMo6CpXy7YNKnNKWCx7ZReKfsnk2pMpRt85R_nYQAi1_Lv0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHc1nj6vnW0srHb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYz0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWfLrjnkPWD3PWT0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YzPWcLP0','新手专享，10%年化利率外加白领1万元体验金','','',0,0,'http://t12.baidu.com/it/u=3812490077,2997628408&fm=58 ',0),
	(2313,'付融宝','http://www.baidu.com/zhixin.php?url=000000aBrumvqgVdmHh53dCM-zaq135imfQgg48RmLVIgOQVULq1Icf-iH_dnSD0DVp6YFdArsXY7pKv8054av1ybiPraBFDVk6QYf3tdYnbRQuWurwcPiHQBtCwMdTeM9YqSdT.7b_NR2Ar5Od663rj6tVTdeQbfUXxXPBqM76z1T5wMlZkwdrs2pMpRt85R_nYQAHFke0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHc1nj6vnW0srHb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYz0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqrjDzn1c3PjDLn6KGTdqLpgF-UAN1T1Ys0AN3Ijd9mvP-TLPRXgK-rhdsr1VsIh-brWDYPjR1nWRsPWn1PWDkP1nsP1cerv-1N-PfrW0erLKzUvwdmLwRXgK-rWnknjcerLw1Tv-brW00mLFW5HmYPWnk','新手注册送50元红包,投资更享多重优惠.','','',0,0,'http://t12.baidu.com/it/u=3198461312,2366586275&fm=58',0),
	(2314,'e租宝','http://www.baidu.com/zhixin.php?url=0000000A63ij27xaGPbjbdN4rkfLamX3QIjC8seIETUMlhaSlyG0ZEHueGk11pHygig6kTwEUBtnUuVeUTV10I-Yv95XWS8zwMJLllgv_QmiQJV7ejJGdiuIoAvh5YrsaoYg7y_.7D_NR2Ar5Od663rj6tbtraqzcP2IPluCcgpIQCCFblMkYD1fILkxfS6-WSwOsnUO3Mzq8WtEzdmX5zlkG9sH5CpAQVviSZWb8uE_HjgjYsq5WkLUePX8EuIIgjbRdrYG4TXGmuCyPv1uub0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHc1nj6vnW0srHb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYz0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWRYPW03n1Tznjb0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YdnWbkrf','1元起投，13.8%预期收益，易投，易赚，易取','','',0,0,'http://t10.baidu.com/it/u=3553142325,1241595200&fm=58 ',0),
	(2315,'抱财网 ','http://www.baidu.com/zhixin.php?url=000000079n0X8m6aKzjnwCzkqDHfo80PD0ruFUwFXx2BVz1V_DbqYTDp6uoSBGGHUr-sXXHzhhYDbX7kU-UcLNXyy77zMO2UMaFKtL4NYBtdp_EbGVuF20uxNMEZke1s_BEAkFY.DD_NR2Ar5Od663rj6tovgf_Ew01jfxqjlABaB17TOPKLsRP5QAeKPa-BqM76z1IT7jHzlRL5spycTT5gKfYt_QrMAzONDk9tOZjEL3xg1sSXejlSLjeXMj4etr1kselt5M_seO5j_SyZFt_5M_sSLl3ISej4qrZueTrOI9tSMjEen5o4qT7jHz_LpGMmnUAZ1lTr1u9qxo9tSMj_q8ZdLmx5u9qX1jlSrZ1LmIMzseqvZ1LmIhej_3JpMpRt85R_nYQZZFkvU0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHc1nj6vnW0srHb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYz0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWfsnHbYPjRznHm0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YYPWf4ns','安心投抱财,100元起投,年化收益15%,注册即送2470','','',0,0,'http://t11.baidu.com/it/u=333791203,1111903910&fm=58 ',0),
	(2316,'君融贷','http://www.baidu.com/zhixin.php?url=000000K6ZLFpKCmm40B6gK9DXeuM0rVimX4deItVzvHqzWkM8NXI5fEwv3-ez2SqzJcOAO4JeoVqs56K7pMf5YJ6kx95LDZWL7uYz_oP99_tzyq3JnJZCPaHdPLWiOoewxbR1C_.7D_NR2Ar5Od663rj6t_8a2uvp4-lc2QnN1tYA1E6CpXyPvap7Q7erQKz4u88a9G4I2UM3PQ7erQK-hl32AM-CFhY_Rg_kIjvTzhGv-5QWdQjPakYe8rls0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHc1nj6vnW0srHb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYz0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWDYn1nLPHTvrjT0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YYPjnzn6','供应链金融项目，联想推荐经销商','','',0,0,'http://t12.baidu.com/it/u=1846308956,1379762240&fm=58',0),
	(2317,'永利宝 ','http://www.baidu.com/zhixin.php?url=000000KrDExf_-u1Vc6Qp3MZLg04UcNKbF0-ADxfuPtxMHhBaTocV44QnhWNj6nFxd_K2xo4n1nGSkduscP5CtWduqzON2TTfTGJs4nSOHETRhnY5J4HPPlyEUWQZD985UEDCGD.7R_NR2Ar5Od663rj6tVj_d7qeQbfIt7jHzk8sHfGmEukmrgQPsd5lZkIT7jHzlRL5spycTT5gKfYtVKnv-B6MCCRgkIEgKfYt_QrMAzONDkY5BWJI6EXz4HReiM-kl-9h9m3eSh1B0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHc1nj6vnW0srHb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYz0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWR4n16srH03n1c0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YzPWD1Ps','100元起投，年化收益18%，注册即送100，首投返308','','',0,0,'http://t11.baidu.com/it/u=2458181578,2447328124&fm=58 ',0),
	(2318,'普资金融','http://www.baidu.com/zhixin.php?url=0000000_V5f9-9n6g8Jr_M9HNoHJcv3zYVexJ8Z7DOJNOoB4PIlzpeII0VsUoz6rs6n0ZWnnC-ljRrYBcStUGyhlTRzIonrWwdFvYMVvqCX8Q2vkuwEVykYpH9xVEQljUn1a08b.7D_jZjjXBKtgYFWCfwF32EufeL_3_ahGv-5QWdQjPakgE_tXs0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHc1nj6vnW0srHb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYz0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnH63rHn3PHcdPWc0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YLrHcs','保本理财,收益15%.央行征信系统可查','','',0,0,'http://t12.baidu.com/it/u=4085738399,4206017096&fm=58',0),
	(2319,'新联在线','http://www.baidu.com/zhixin.php?url=0000000H3ak2qTDgsiGniing6_u1UQP45hppWSOEZS9n-YjyGt_8siQ7ipH--rGpsK9Zn_j5Riylboo53f01t8YzLMwqdtrqZ323qM2QO0EObPbJiezwXif_5fIQFy0d1AIZfkf.7Y_aF5K6BXlQb6CHnkwlLHpypjLwXN18SPhuMikYsGzgI-IMWTMxE988ldpm4hQvTyj9JxY5BldpIP1ZyoHWC_IpV5UsnIpVnQv7PHV2XgZJyAp7WW8tI-m.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcdnWTYPHmdrHb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWY10Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnHTsn1msrHRsnWc0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YknHnLns','注册送10000元理财基金（奖）','','',0,0,'http://t12.baidu.com/it/u=588178082,676951430&fm=58',0),
	(2320,'微金所','http://www.baidu.com/zhixin.php?url=000000joEUJTeRVpsI9S08HXsoY4VQqC68xIkj0lDqO875ZWXPGSKN7N2HL_RYACnxIcXTOc7cO6fjg0ilVJdpOc1-VsqRGwG_fPqayL7uVQ1SxiaHpLwbuOn_ccUW3DGGpiHGY.DY_iTag4gkw3Zc6HYRDkwEI74grek1f_g_3_D7IhNtR1_zhGv-5QWdQjPak8vI-Im0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcdnWTYPHmdrHb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWY10Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWmYrH63Pjm4rjc0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1Y1njD3n6','微小宝-期限短,年化高,安全有保障','','',0,0,'http://t11.baidu.com/it/u=237404614,4224149349&fm=58',0),
	(2321,'三分贷 ','http://www.baidu.com/zhixin.php?url=000000a1fNLgZFJ3FjTqfYq0aTLtozrT0G_chs0cZUBCfHO9wRltUZTUgpry8i-v0HpACuZgexqoG1E8cKZDgse7SRSoWZPymfkjG0Md68K8C8tpL_8hjbhGfJ33U0tKNitP3hb.Db_jWoH7TzqDAaFeCCQLHHr3ccSySl7-SE0GJIGHz3qis1f_UorPxD0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcdnWTYPHmdrHb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWY10Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWmdn1nsn1R1nWc0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YkrHcLn6','车辆抵押，投资有保障，放心理财，收益高上高','','',0,0,'http://t10.baidu.com/it/u=1680784644,2885798057&fm=58 ',0),
	(2322,'可溯贷','http://www.baidu.com/zhixin.php?url=000000j2eJ_iTEJH4eiZShk7UnlXnoaYb-MwQ4geoCE7K7-OzOEfrffioYiuGhDf1AJL97NxV7QR6gcdavqC01lEmbivFmvCbMS3eCDCqFnY_Drx9AO6s277uYtXP_ujxH7kaWT.DR_NR2Ar5Od66k_kk3LHR5QVHZ69IPWw0SQlZYDt_UqRgQ6kqt7jikkzXyHuuvX5-bvyIh1zzU2q-M-bvNeqlhGv-5QWdQjPakbtXMG360.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcLrjbYnH0YnHm0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYY0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnHmknWc3Pj01PWf0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YYPjm1r0','国资背景,资金托管,国家AAA级P2P平台','','',0,0,'http://t11.baidu.com/it/u=3381586819,2771664094&fm=58',0),
	(2323,'妙资金融 ','http://www.baidu.com/zhixin.php?url=000000adbwhlkO0ejnLq6ry0axBkxAoRVpbymNncXvt-zDe9m6tj2gSTAg2Oi-MLwo8yaC5AbZ3NJxzFhu1vx7PsZjXV7pb_tJbDDP8q4R6ZtuPx9Ce4EVwoh3XK1A7o-xoIQL0.DD_NR2Ar5Od663rj6tV5jGIDMfBar61fxZfxj01IqDOxi1PvGCnUPOVlxU_zXrH3x54Txg_oSgHkLSxHugtVLW_lxU8SJhGv-5QWdQjPakYeShs.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcLrjbYnH0YnHm0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYY0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWR1P1D3PHT4rH00pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YYP1mvPf','八年专注理财,准金融机构,交易0手续费,注册送1万红包','','',0,0,'http://t10.baidu.com/it/u=467352774,3852498741&fm=58 ',0),
	(2324,'口袋理财 ','http://www.baidu.com/zhixin.php?url=000000aEgMsgy7YGgyhwrDi964za6qfMZubbgCUYQ0p2aRk3qshE2uSYVJt6DO2XY9NYXk3Xbd9DWGDnyL3u1N3PyL3DH7H_BWaTpj2a58HAidnG8MyrL5Zdwu53Tt-ShBAwYsC.7b_jVI68uxw9w01jfxqjlABaB17IMk_lbq3LmFCRtVKMRkX1BsflSKHReiM-kl-9h9m3enMHE0.THdsnMKxnHfYPHbvPjD4n6KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHcLrjbYnH0YnHm0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYY0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWmYrjczPjm3nWc0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YkPW6zPs','固定收益8%加浮动收益0-12%，一月即可转让','','',0,0,'http://t10.baidu.com/it/u=3877452159,753678259&fm=58 ',0),
	(2325,'金海贷','http://www.baidu.com/zhixin.php?url=0000000PF-tKs7Xu1lIghyNlwOjmUZNGSZrzpExAlwlVJL75fhs7npC8ZQ2AAhB2igMc5PeYcd49oooxz1K5D6CZ4regQz38A2mzK3JuUl3F_CIvciB9_gui8FG6EtBddhaWJXc.7D_j2S2GMDzqDAaPnYAPv5KwuhxsGedpm4hQvTya7-SEKdJYlUh2thW0MoNGkYyrVyMV3ArVyh7eahGv-5QWdQjPakYvUeQ0.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnsnjcznWcYn1b0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYd0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnHTvPjRYn1nsPHm0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YkPW04r0','金海贷提供稳健安全的高收益理财产品，专注P2P理财','','',0,0,'http://t12.baidu.com/it/u=2310468077,1823921963&fm=58',0),
	(2326,'巨涟金融 ','http://www.baidu.com/zhixin.php?url=0000000oaLtlWhl2gpm-Xyv3ZEu2GBQFLLgZ_by3hGrSkQD7FcS_enN3r_6TczBzhM-FD6qO9cklFALzqZuxpdoNY6BlQqXQFEuk4bQZtq6A7CmKsAHxSwQPvcD4maIRiSrJSb6.7Y_NR2Ar5Od66Js59swXygKwH7TBjqqtHnR2mclhubsUXgZjS4LsRP5QAeKPa-WFk_zNvNv2pMpRt85R_nYQAexELXc.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnsnjcznWcYn1b0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYd0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWmkP16Ln1T3nj00pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YkP104nf','8.8%活期理财，随存随取，注册立送15000元体验金','','',0,0,'http://t11.baidu.com/it/u=2584486981,1944919565&fm=58 ',0),
	(2327,'懒财信息','http://www.baidu.com/zhixin.php?url=000000K9ncfv_49eyqSquA7RP8_SO4LSxeasPftO-A-L32Oo8Bytbncj4j9jrjei2LixNQdcEN_Kcu0h8JXEUGccfHaat6UViflm7N7UKasCgOrIu1QVOjpPI5cY2_ZT1YOcwQn.DY_aB17azOb6x7fhII69C5I7KXzeML9knUorP-hwHReiM-kl-9h9mzI-xuY6.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnsnjcznWcYn1b0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYd0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWfYP1n3nHbzrjn0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YvPWDs','7x24h可取，限额60万，首信易支付托管，君联资本入股','','',0,0,'http://t11.baidu.com/it/u=4128638536,409361378&fm=58',0),
	(2328,'银湖网','http://www.baidu.com/zhixin.php?url=000000j3FM5lKgvUNvjz_H42TB8ThqD2pdrv65B-3x8k7rOKivqybnGAzw57hs_bY0HLicH0uE3NbRrHtXPH1ex2Ia8n_G-pYH2dhp-xBUL0QvzNw3Q__XFMPA8PLW5EBwTlLo3.DD_NR2Ar5Od663rj6tVj3J_ac56tol5X5KxSgiseAhQEZ6EOQGmIWH7dWI6h9HSgikl_3_ZyPvap75twP1tA-W3xodWI6h9HtQrMAzONDkSU3X_U_DY2x9IT-Bz1cGJIGHz3qis1f_I--xHb.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnsnjcznWcYn1b0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYd0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnHRsnWDvP1TvP1b0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1Y1rHbLPs','新用户注册送50红包,满额送大礼','','',0,0,'http://t10.baidu.com/it/u=1547726331,2978545955&fm=58',0),
	(2329,'金联储','http://www.baidu.com/zhixin.php?url=000000jHHACD8WHSM0EmQqrwl4fjkYk8U0gRfmyuPDvwm8qCIY8BejUGkpieMp7RsRCWDHTdzqvwte1BWntbMmblvN0bqNxvnurUjRAQxrVpwjM59OOHdGIc37nOet1EiM__42D.7R_j2S2Bqh2Ao0Bar61fxj01XeKf4yUJOzILTnUAzz1xuY4t-Mu3JpMpRt85R_nYQZu_3en6.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnzn103Pj0YPWb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYv0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnW0zPWDknWn4PWT0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YvnHDLP6','10天理财标特享8%年化收益,时间短,收益高','','',0,0,'http://t11.baidu.com/it/u=895355809,693481341&fm=58',0),
	(2330,'全民财富','http://www.baidu.com/zhixin.php?url=0000000yxmuUqvXyB6Jocysmi-l7zB0zOU7q5iKnBgGasRQY_KwD9f0cTUxJfysauJ4CudsHL1Ek7tS8bLV8gWRPHXL1QxBXJUZfjwHWThN1M1Gvhr7JjDQIDWhaih7-cUBY1lR.Db_NR2Ar5Od663rj6tEuG9uxosc6WCnkgn1gTStPyFBqM76Wi1PjNdsRP5QAeKPa-BqM76l32AM-YG8x6Y_f33X8a9G4myIrP-SJFWj4e_r1I9q81j4qrZd_selt5M_sSEu9q8Hj4e_5VS8ZutrZFt_5M33ISOjlSXZ1Lmxgv3xUY3x5u9qX1jeThZ1LmxUqTr13X8a9G4mgwRDkRAr8a9G4pauVQAZ1LmIh1jEqT5MY3IMu9vxgjbSyZ1lTrOGsSVhZ1lTr1x9vX8Z1en5ot_rOW9tqSZvSLj_SZj4qrZvvmIMBGJIGHz3qis1f_I-xHuk0.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnzn103Pj0YPWb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYv0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWfLPWc3rjRdPHn0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YkP1T4Ps','15%年化收益率，注册就送10000元','','',0,0,'http://t10.baidu.com/it/u=4081870182,2831530068&fm=58 ',0),
	(2331,'爱投资','http://www.baidu.com/zhixin.php?url=0000000hOhPwHyo_ACpxkfWDMF7HtQNsJqzXYkgC4kez9wbdsb7PJGtUyOmw0OGaSHlbEyEk8XJX_qVRee8pnmGbzkFlhyNK3OmyX4neZRJspmoJNifWJATXMOrEN9SHAirBdRb.7D_KqKvj9XBaF5K6WHPK6BXl2jULW4pQPlpQ7qvNtR1_LQYwt5wYlnaxmOZb9lYXgDkkRdrYG4TXGmuCy5ZWgE0.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnzn103Pj0YPWb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYv0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWfLnW6YnWn3n1f0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YvPHnL','3个月短期理财,按月付息，时间短回本快，投资放心','','',0,0,'http://t11.baidu.com/it/u=3985425439,3157180920&fm=58',0),
	(2332,'91旺财','http://www.baidu.com/zhixin.php?url=000000KG6pUU1nSUsxpa1c2JHSIHK7nSXoG4JOAwHhIC9LrG49ziWtp5hQqd7Icua0FL-g-Xmi4Amdq7JwINR0wW5k4okU5zIEuUyLWSNbJB0diKiUKxsTQwnQci7NmxN_vk0gT.7D_jxgsn4CygC6ekGYqmLmFCR_m3XyPrM-E84UeCIphGv-5QWdQjPakg8LqMD0.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnzn103Pj0YPWb0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYv0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWTsrHn4PjTsnWT0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YzrjRLnf','91旺财-中国最安全的P2P理财平台','','',0,0,'http://t12.baidu.com/it/u=2798567072,1714316513&fm=58',0),
	(2333,'合力贷','http://www.baidu.com/zhixin.php?url=000000a-qWXa74p8Ze6ztjGfETAVY_Mn-9OUOKD2HDk63VIXAhqrxLnLFJBR5yoMyP6JNkA9eG1FoaS7u_oxq4KHbK5DvpOFEs3_qMTSioIWZeNpvQ0DFvGoJ3POJxg64KgJ1Qb.7D_aGPAB4hQb6qn2m3322mo8qyhruWj4SrZxEsSEM9tqvZdq8ZvqrZ1tTrOl3xU8sSXejlOgj_3qpVrh4Wj4qrZxY3IMW9tqvZvOQjEtXVyMZwmldpXL6mldpmg_2pMpRt85R_nYQAl1GLu0.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnYP10kP1mzPjf0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYL0Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnHnvnHmsPj0kn100pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1Ykrj01n6','央行互联网金融专业委员，老牌P2P金融网贷平台','','',0,0,'http://t12.baidu.com/it/u=150083450,3954442362&fm=58',0),
	(2334,'生菜金融 ','http://www.baidu.com/zhixin.php?url=000000jDxdgNiP7iShxL0aZ8GpCiTX0ZCCDVx_5ye9XefeP6-lZOrnKig-3tIDaUA72Zw7ypOsTUIvURwPE8torQYRAumbsUouubn2g2cAbAR0qk9NIk6SGh7Rlx0-Tq4mfXHtD.7Y_arGrp7-SNtR1_zcP_ZXSuh2eeOFWGLU8zkHReiM-kl-9h9menhHFm.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnvP1bsPHDYn1D0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWY30Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWT3nj0znjmdnj00pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YznWnYr0','收益高,期限灵活,安全,','','',0,0,'http://t10.baidu.com/it/u=92462034,2328018451&fm=58',0),
	(2335,'网信理财','http://www.baidu.com/zhixin.php?url=0000000D_Xh2_uh8hNuoNm9M5AZkFIOdvSCnecbOjU_goKzCHt8rT73kw5fDqSD6xwsSU-EMVoJvNg63fUiBGYeaz-jUv2TJ16oY31lbvvsQKeqI9o3yiLAKVO5oeJUW-zUe4Qs.7Y_jySpr9WzmYfI3mh5oOv5LOSQSwyGJIGHz3qis1f_XPxZB.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHnvP1bsPHDYn1D0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWY30Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWRknHm1PHcvPHD0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YzrHTdPs','建行入股企业，银行级安全系数','','',0,0,'http://t12.baidu.com/it/u=381798716,3315296581&fm=58 ',0),
	(2336,'昂道招财猫 ','http://www.baidu.com/zhixin.php?url=000000jdxHxqAQooOQaPiMYM5HuvW7Dvn75NIsfl_mXF5Nh4T4Tq1_SD5nxz9m7s3o0Xw-PfC4DPN7-LqD98C0s7-BLYlvV3ZakeiGd6G6ZhAkNs4UvzXy78REZ3EQBTo7OHKYR.7R_j99YkFCc6WHGhHJOTRdrYG4TXGmuCyPvHktIs.THdsnMKxnHfYPHbvPjD4nsKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHn3rHDsn10sPHf0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWY40Zw-ThdGUh7_5H00XMfqTZn0TZ0q0A7bpyfqnWTsPWczPW0Ynjm0pgPxIv-zuyk-TLnqn0K-XZfqmyPWugP1NZ-suHGVTj_eTZuGujCkPjfdn1cdnjm1n1mknHT1njTzr1VGTduHRjCsr1VsThqbIyPYNZ-suHC1nH0zr1VYTLPGujCs0APzm1YYPHTzPf','1-30天超短期项目,投资1千立返50现金,投1w立返200现金','','',0,0,'http://t12.baidu.com/it/u=2797576127,2245695267&fm=58 ',0),
	(2337,'黄河金融 ','http://www.baidu.com/zhixin.php?url=000000KRdLqlpG3zyKSp0RXBvn5efwyv2xKjClkcpGpAdfHUEqxx3_WXJDx6iUIXEm9kZl_-2u3lhQNSofoPjF_NUUNZkAteGMRQRlw5V8YBpQVI4XyqjMHvgRyPIRRWqEoFXgf.7R_aJGP1QYfI3phmCbYXE_mvqpeQ2OUd3D_Yp7NphGv-5QWdQjPak8vyyN6.THdsnMKxnHfYPHbvPjD4P0KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHfkPH0kn1ndnjm0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYkn0KYugFVpy49UjYs0ZGY5gK10ZKs5fK9uA-b5HcznWb4nWRdPHDd0A-1gLIGThN_ugP15H00ug9Y5y7WmvN1Tdw4TARlUg0erLKvpyflnHfYPHnzPH0vn1nvnHDLn10LnW_epgPyRd0lnj_eTZFEuZNWI7w4TARln1DsnW_eIZP1pyfln0KWThnqnWfsPWc','收益高达14%，与浙商保险合作，为用户保驾护航','','',0,0,'http://t11.baidu.com/it/u=3751684744,4114151254&fm=58 ',0),
	(2338,'凤凰信用','http://www.baidu.com/zhixin.php?url=000000KZT8ulth-BY4Zvfg7ChAsaEs2ZlD9PF3gb2yRNe1C-UH_CXivdFT5UVggUCL3WGAy6y9wXVkef6USM1OQ3DkQ4o5uJ_U3GpmxblP-M5LYPLwb6ZfxwaX_0AdjJBzFpZOc.7D_jx_vUriKhA6OlZYPDDiXj56WHPMdmnimrO_8mqx-OVxeg4E9l3cIXj-WI6h9iklOQOpOvE1xjdsfltZQ6knTPHS9tNphGv-5QWdQjPakggklXs0.THdsnMKxnHfYPHbvPjD4P0KYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHfkPH0kn1ndnjm0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYkn0KYugFVpy49UjYs0ZGY5gK10ZKs5fK9uA-b5HcvnWckPjnLnWRv0A-1gLIGThN_ugP15H00ug9Y5y7WmvN1Tdw4TARlUg0erLKvpyflnHfYPHnzPH0vn1nvnHDLn10LnW_epgPyRd0lnj_eTZFEuZNWI7w4TARln1DsnW_eIZP1pyfln0KWThnqnWT4rjb','金融工场,新手理财,100起投,注册送120.','','',0,0,'http://t12.baidu.com/it/u=3397502489,2721585319&fm=58 ',0),
	(2339,'投宝金融','http://www.baidu.com/zhixin.php?url=00000003POGGzIG38nrKyhAmBgmT_MtU6KeymbD4Vv44yoTEaMiKRTSj2W8XIhYwBE3QnJjDLr4N4c6mDy11y80mbwhhOENaEu8Ft7vKKcnxZwadDsrupg-bdYEjzr6tl-yG6EC.DR_aEkCrsg_8fGJIGHz3qis1f_UEZW8L00.THdsnMKxnHfYPHbvPjD4PfKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHRsPWcznHfLnjR0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYkP0KYugFVpy49UjYs0ZGY5gK10ZKs5fK9uA-b5HcdPjDsnHT1nWDY0A-1gLIGThN_ugP15H00ug9Y5y7WmvN1Tdw4TARlUg0erLKvpyflnHfYPHnzPH0vn1nvnHDLn10LnW_epgPyRd0lnj_eTZFEuZNWI7w4TARln1DsnW_eIZP1pyfln0KWThnqnWmvP6','国资平台,国有担保,银联托管,稳保高收益','','',0,0,'http://t11.baidu.com/it/u=1580544139,2866307893&fm=58',0),
	(2340,'精融汇 ','http://www.baidu.com/zhixin.php?url=00000004ekhbkz2i10xTjowuPKco29V-VHPEdL6wP0uD0lzKWn8WaoQxgZHBXFRJfgH_3CCTmOn4Wedguf45XbjhV7XjCoxT35iA9GzSAbfxDpSCg870FTfoD_AZO9xN5t3kXw0.7Y_NR2Ar5Od663rj6tM_l3praqDj3ECHZf6e33MnXvzz1WdWCnIpn1PKQZyWfHPGmRdrYG4TXGmuCyPdI-m.THdsnMKxnHfYPHbvPjD4PfKYTh7buy-b5Hm4njD0mvq1I7qzmy4o5H00TLNBTy-b5HDYPjR4PWfkrHRdnjfYnH6LnHD0ugw4TARqnHD0uy-b5H00uyw-TvPGujYs0AP_pyPogLw4TARqn6KsUWYkP6KYugFVpy49UjYs0ZGY5gK10ZKs5fK9uA-b5Hczrjb4njDzP1f10A-1gLIGThN_ugP15H00ug9Y5y7WmvN1Tdw4TARlUg0erLKvpyflnHfYPHnzPH0vn1nvnHDLn10LnW_epgPyRd0lnj_eTZFEuZNWI7w4TARln1DsnW_eIZP1pyfln0KWThnqPWD4Pjb','华安财险全额承保,上市公司背景,新浪支付托管','','',0,0,'http://t11.baidu.com/it/u=2823320809,4153516600&fm=58 ',0);

/*!40000 ALTER TABLE `fin_p2p_master` ENABLE KEYS */;
UNLOCK TABLES;



/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
