CREATE SCHEMA `wisencrazy_restaraunt` DEFAULT CHARACTER SET utf8 ;

CREATE TABLE `customer` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) DEFAULT NULL,
  `email_id` varchar(50) DEFAULT NULL,
  `password` varchar(45) DEFAULT NULL,
  `account_status` enum('ACTIVE','IN_ACTIVE','SYSTEM_GENERATED','DISABLE') DEFAULT NULL,
  `token` varchar(100) DEFAULT NULL,
  `google_access_token` text,
  `photo_url` varchar(255) DEFAULT NULL,
  `customer_type` enum('INDIVIDUAL','GROUP') NOT NULL DEFAULT 'INDIVIDUAL',
  `primary_contact` varchar(15) DEFAULT NULL,
  `is_mobile_verified` tinyint(1) DEFAULT '0',
  `sid` binary(32) NOT NULL,
  `member_since` timestamp NULL DEFAULT NULL,
  `signup_type` enum('gmail','facebook','android','restaraunt') DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `sid` (`sid`)
) ENGINE=InnoDB AUTO_INCREMENT=3101 DEFAULT CHARSET=utf8;


CREATE TABLE `country` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `country_name` varchar(255) DEFAULT NULL,
  `country_code` char(5) DEFAULT NULL,
  `country_short_name` char(10) DEFAULT NULL,
  `country_image_link` varchar(45) DEFAULT NULL,
  `sid` binary(32) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `sid` (`sid`)
) ENGINE=InnoDB AUTO_INCREMENT=209 DEFAULT CHARSET=utf8;

CREATE TABLE `state` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `state_name` varchar(255) DEFAULT NULL,
  `country_id` int(11) DEFAULT NULL,
  `sid` binary(32) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `sid` (`sid`),
  KEY `fk_state1` (`country_id`),
  CONSTRAINT `fk_state1` FOREIGN KEY (`country_id`) REFERENCES `country` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=87 DEFAULT CHARSET=utf8;

CREATE TABLE `customer_address` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `address_line1` varchar(45) DEFAULT NULL,
  `address_line2` varchar(45) DEFAULT NULL,
  `address_line3` varchar(45) DEFAULT NULL,
  `pin` char(10) DEFAULT NULL,
  `country_id` int(11) NOT NULL,
  `state_id` int(11) DEFAULT NULL,
  `sid` binary(32) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `sid` (`sid`),
  KEY `fk_customer_address_1` (`country_id`),
  KEY `fk_customer_address_2` (`state_id`),
  CONSTRAINT `fk_customer_address_1` FOREIGN KEY (`country_id`) REFERENCES `country` (`id`),
  CONSTRAINT `fk_customer_address_2` FOREIGN KEY (`state_id`) REFERENCES `state` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


CREATE TABLE `customer_has_address` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `customer_address_id` int(11) NOT NULL,
  `customer_id` int(11) NOT NULL,
  `sid` binary(32) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `sid` (`sid`),
  KEY `fk_customer_has_address_1` (`customer_address_id`),
  KEY `fk_customer_has_address_3` (`customer_id`),
  CONSTRAINT `fk_customer_has_address_1` FOREIGN KEY (`customer_address_id`) REFERENCES `customer_address` (`id`),
  CONSTRAINT `fk_customer_has_address_3` FOREIGN KEY (`customer_id`) REFERENCES `customer` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `city` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `city_name` varchar(255) DEFAULT NULL,
  `state_id` int(11) DEFAULT NULL,
  `sid` binary(32) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `sid` (`sid`),
  KEY `fk_city1` (`state_id`),
  CONSTRAINT `fk_city1` FOREIGN KEY (`state_id`) REFERENCES `state` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=87 DEFAULT CHARSET=utf8;

CREATE TABLE `area` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `area_name` varchar(255) DEFAULT NULL,
  `city_id` int(11) DEFAULT NULL,
  `sid` binary(32) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `sid` (`sid`),
  KEY `fk_area1` (`city_id`),
  CONSTRAINT `fk_area1` FOREIGN KEY (`city_id`) REFERENCES `city` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=87 DEFAULT CHARSET=utf8;


CREATE TABLE `restaraunt` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `sid` binary(32) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `photo_url` varchar(255) DEFAULT NULL,
  `email_id` varchar(100) DEFAULT NULL,
  `primary_contact_no` varchar(200) default NULL,
  `is_active` tinyint(1) NOT NULL DEFAULT '1',
  `created_date` timestamp NULL DEFAULT '2000-01-01 00:00:00',
  `address` varchar(255) DEFAULT NULL,
  `pin` varchar(15) NOT NULL,
  `latitude` varchar(20) DEFAULT NULL,
  `longitude` varchar(20) DEFAULT NULL,
  `area_id` int(11) NOT NULL,
  `cost_for_2` int(11) DEFAULT 0,
  PRIMARY KEY (`id`),
  UNIQUE KEY `sid` (`sid`),
  KEY `fk_project4` (`area_id`),
  CONSTRAINT `fk_project4` FOREIGN KEY (`area_id`) REFERENCES `area` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `restaraunt_has_timings` (
  `restaraunt_id` int(11) NOT NULL ,
  `timing` enum('BR','LU','DI') NOT NULL,
  `photo_url` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`restaraunt_id`,`timing`),
  KEY `fk_rht1` (`restaraunt_id`),
  CONSTRAINT `fk_rht1` FOREIGN KEY (`restaraunt_id`) REFERENCES `restaraunt` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `restaraunt_has_reviews` (
  `restaraunt_id` int(11) NOT NULL ,
  `customer_id` int(11) NOT NULL,
  `rating` int(11) DEFAULT 0,
  `comment` TEXT DEFAULT NULL,
  `timestamp` timestamp DEFAULT '2000-01-01 00:00:00',
  PRIMARY KEY (`restaraunt_id`,`customer_id`),
  KEY `fk_rhr1` (`restaraunt_id`),
  KEY `fk_rhr2` (`customer_id`),
  CONSTRAINT `fk_rhr1` FOREIGN KEY (`restaraunt_id`) REFERENCES `restaraunt` (`id`),
  CONSTRAINT `fk_rhr2` FOREIGN KEY (`customer_id`) REFERENCES `customer` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


create view vw_restaraunt_location as 
select r.id,unhex(r.sid) as `sid`,r.name,a.area_name,rht.photo_url,latitude,longitude,timing,'distance' from restaraunt r 
join area a on r.area_id=a.id 
left outer join restaraunt_has_timings rht on r.id=rht.restaraunt_id;
