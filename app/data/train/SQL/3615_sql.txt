CREATE DATABASE  IF NOT EXISTS `luyabaya_binary` /*!40100 DEFAULT CHARACTER SET latin1 */;
USE `luyabaya_binary`;
-- MySQL dump 10.13  Distrib 5.7.12, for Win64 (x86_64)
--
-- Host: localhost    Database: luyabaya_binary
-- ------------------------------------------------------
-- Server version	5.6.33-cll-lve

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `_currentchild`
--

DROP TABLE IF EXISTS `_currentchild`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `_currentchild` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `parent` int(11) NOT NULL,
  `old_count` int(11) DEFAULT '0',
  `new_count` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `_selectedhierarchy`
--

DROP TABLE IF EXISTS `_selectedhierarchy`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `_selectedhierarchy` (
  `_id` int(11) NOT NULL AUTO_INCREMENT,
  `child` int(11) DEFAULT NULL,
  `depth` int(11) DEFAULT NULL,
  `full_name` varchar(150) DEFAULT NULL,
  `parent` int(11) DEFAULT NULL,
  `position` varchar(100) DEFAULT NULL,
  `m_position` varchar(50) DEFAULT NULL,
  `m_parent` int(10) DEFAULT NULL,
  `f_position` varchar(50) DEFAULT NULL,
  PRIMARY KEY (`_id`)
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `codes`
--

DROP TABLE IF EXISTS `codes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `codes` (
  `code_id` int(11) NOT NULL AUTO_INCREMENT,
  `description` varchar(100) DEFAULT NULL,
  `generated_date` varchar(100) DEFAULT NULL,
  `generated_by` varchar(100) DEFAULT NULL,
  `status` varchar(100) DEFAULT NULL,
  `used_by` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`code_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `commission`
--

DROP TABLE IF EXISTS `commission`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `commission` (
  `commission_id` int(11) NOT NULL AUTO_INCREMENT,
  `c_user_id` int(11) DEFAULT NULL,
  `c_amount` float DEFAULT NULL,
  `r_user_id` int(11) DEFAULT NULL,
  `depth` varchar(11) DEFAULT NULL,
  `remarks` varchar(45) DEFAULT NULL,
  `date_create` datetime DEFAULT NULL,
  PRIMARY KEY (`commission_id`)
) ENGINE=InnoDB AUTO_INCREMENT=7 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `hierarchy`
--

DROP TABLE IF EXISTS `hierarchy`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `hierarchy` (
  `p_id` int(11) NOT NULL AUTO_INCREMENT,
  `parent` int(11) DEFAULT NULL,
  `child` int(11) DEFAULT NULL,
  `depth` int(11) DEFAULT NULL,
  `position` varchar(45) DEFAULT NULL,
  `m_position` varchar(45) DEFAULT NULL,
  `created_by` varchar(45) DEFAULT NULL,
  `datetime` datetime DEFAULT NULL,
  PRIMARY KEY (`p_id`)
) ENGINE=InnoDB AUTO_INCREMENT=50 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `position`
--

DROP TABLE IF EXISTS `position`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `position` (
  `position_id` int(11) NOT NULL AUTO_INCREMENT,
  `user_id` varchar(100) DEFAULT NULL,
  `position_left` varchar(100) DEFAULT NULL,
  `position_right` varchar(100) NOT NULL,
  `sponsor_by` varchar(100) DEFAULT NULL,
  `upline` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`position_id`)
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `product_purchase`
--

DROP TABLE IF EXISTS `product_purchase`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `product_purchase` (
  `product_purchase_id` int(11) NOT NULL AUTO_INCREMENT,
  `amount` varchar(100) NOT NULL,
  `date_purchase` date NOT NULL,
  `user_id` int(11) NOT NULL,
  PRIMARY KEY (`product_purchase_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `request_withdrawal`
--

DROP TABLE IF EXISTS `request_withdrawal`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `request_withdrawal` (
  `request_withdrawal_id` int(11) NOT NULL AUTO_INCREMENT,
  `amount` varchar(100) NOT NULL,
  `date_requested` varchar(100) NOT NULL,
  `user_id` int(11) NOT NULL,
  `status` int(11) NOT NULL,
  PRIMARY KEY (`request_withdrawal_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `users` (
  `user_id` int(11) NOT NULL AUTO_INCREMENT,
  `user_name` varchar(100) DEFAULT NULL,
  `password` varchar(100) DEFAULT NULL,
  `first_name` varchar(100) DEFAULT NULL,
  `last_name` varchar(100) DEFAULT NULL,
  `gender` varchar(100) DEFAULT NULL,
  `contact` varchar(100) DEFAULT NULL,
  `address` varchar(100) DEFAULT NULL,
  `entered_on` datetime DEFAULT NULL,
  `status` varchar(100) DEFAULT NULL,
  `position` int(11) DEFAULT NULL,
  `email` varchar(100) DEFAULT NULL,
  `sponsor_by` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`user_id`)
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `withdrawal`
--

DROP TABLE IF EXISTS `withdrawal`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `withdrawal` (
  `withdrawal_id` int(11) NOT NULL AUTO_INCREMENT,
  `w_user_id` int(11) DEFAULT NULL,
  `w_amount` float DEFAULT NULL,
  `remarks` varchar(45) DEFAULT NULL,
  `date_create` datetime DEFAULT NULL,
  PRIMARY KEY (`withdrawal_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping events for database 'luyabaya_binary'
--

--
-- Dumping routines for database 'luyabaya_binary'
--
/*!50003 DROP FUNCTION IF EXISTS `add_userCommission` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` FUNCTION `add_userCommission`(userId INT, amount FLOAT, _desc VARCHAR(45), r_userId INT, _depth VARCHAR(11)) RETURNS varchar(11) CHARSET latin1
BEGIN
	DECLARE _return VARCHAR(11);
    DECLARE _dCounter INT(11);
    DECLARE _maxChild INT(11);
    
    SET _dCounter = (SELECT COUNT(*) FROM commission WHERE remarks = 'upline' AND c_user_id = userId AND depth = _depth);
    SET _maxChild =  ROUND((POW(2, ( REPLACE(_depth, CONCAT('depth_', userId , '-'), '') + 1))/ 2), 0);
    
	IF _desc = 'upline'
		THEN
			IF r_userId IS NOT NULL AND (SELECT COUNT(*) FROM commission WHERE remarks = 'upline' AND c_user_id = userId AND r_user_id = r_userId) != 1
				THEN
					INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create) VALUES(userId, amount, r_userId, _depth, _desc, NOW());
					SET _return = 'success upline';
            ELSE
				SET _return = 'error upline';
            END IF;
            
            
            IF r_userId IS NULL AND _dCounter != _maxChild
				THEN
					INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create) VALUES(userId, amount, null, _depth, _desc, NOW());
					SET _return = 'success upline';
			ELSE
				SET _return = 'error upline';
            END IF;
	ELSEIF _desc = 'referral'
		THEN
			INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create) VALUES(userId, amount, r_userId, null, _desc, NOW());
			SET _return = 'success referral';
	ELSE
			SET _return = 'error referral';
    END IF;
    
    RETURN _return;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP FUNCTION IF EXISTS `count_childs` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` FUNCTION `count_childs`(`childId` INT(11)) RETURNS int(11)
BEGIN
	DECLARE id INT(11);

	SET id = (SELECT COUNT(*) AS descendant_count
				FROM _selectedhierarchy AS c
					INNER JOIN users AS u ON (u.user_id = c.child)
				WHERE c.m_parent = childId AND depth != 0);
    
    IF id IS NULL
		THEN 
			SET id = 0;
	ELSE
		SET id = id;
	END IF;

	RETURN id;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP FUNCTION IF EXISTS `get_DescendantCount` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` FUNCTION `get_DescendantCount`(`childId` INT(11), `depthId` INT(11)) RETURNS int(11)
BEGIN
	DECLARE id INT(11);

	IF depthId = 0
		THEN
			SET id = (SELECT COUNT(*) AS descendant_count
				FROM _selectedhierarchy AS c
					INNER JOIN users AS u ON (u.user_id = c.child)
				WHERE c.parent = childId AND c.depth = 1);
	ELSE
		SET id = (SELECT COUNT(*) AS descendant_count
				FROM hierarchy AS c
					INNER JOIN users AS u ON (u.user_id = c.child)
				WHERE c.parent = childId AND c.depth = 1);
	END IF;
    
    IF id IS NULL
		THEN 
			SET id = 0;
	ELSE
		SET id = id;
	END IF;

	RETURN id;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP FUNCTION IF EXISTS `get_FullName` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` FUNCTION `get_FullName`(`userId` INT(11)) RETURNS varchar(100) CHARSET latin1
BEGIN
	DECLARE _desc VARCHAR(100);

	SET _desc = (SELECT CONCAT(n.first_name, ' ', n.last_name) FROM users n WHERE n.user_id = userId);

	RETURN _desc;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP FUNCTION IF EXISTS `get_Parent` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` FUNCTION `get_Parent`(`childId` INT(11)) RETURNS int(11)
BEGIN
	DECLARE id INT(11);

	SET id = (SELECT parent FROM hierarchy WHERE child = childId AND parent != child  LIMIT 1);
    
    IF id IS NULL
		THEN 
			SET id = childId;
	ELSE
		SET id = id;
	END IF;

	RETURN id;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP FUNCTION IF EXISTS `get_Position` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` FUNCTION `get_Position`(`childId` INT(11)) RETURNS varchar(11) CHARSET latin1
BEGIN
	DECLARE id VARCHAR(11);

	SET id = (SELECT position FROM hierarchy WHERE child = childId AND parent != child  LIMIT 1);
    
    IF id IS NULL
		THEN 
			SET id = (SELECT position FROM hierarchy WHERE child = childId  LIMIT 1);
	ELSE
		SET id = id;
	END IF;

	RETURN id;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP FUNCTION IF EXISTS `update_ChildCount` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` FUNCTION `update_ChildCount`(`depthId` INT(11)) RETURNS int(11)
BEGIN
    DECLARE _oldCount INT(11);
    DECLARE _newCount INT(11);
    DECLARE count INT(11);
    DECLARE counter INT(11);
    
	SET _newCount = (SELECT SUM(get_DescendantCount(s.child, s.depth)) AS count
						FROM   _selectedhierarchy s
						WHERE EXISTS (SELECT 1 
										   FROM   _currentchild c
										   WHERE  s.depth = c.depth)
						AND s.depth = depthId
						GROUP BY s.depth);
	SET _oldCount = (SELECT old_count FROM _currentchild WHERE depth = depthId);
	SET count = (SELECT new_count FROM _currentchild WHERE depth = depthId);
    
	IF _oldCount != _newCount
		THEN
			UPDATE _currentchild SET old_count = count, new_count = _newCount WHERE depth = depthId;
			SET counter = 1;
	ELSE
		SET counter = 0;
	END IF;
    
    RETURN counter;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `add_userCommission` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `add_userCommission`(IN userId INT,IN amount FLOAT,IN _desc VARCHAR(45),IN r_userId INT,IN _depth VARCHAR(11))
BEGIN
	DECLARE _return VARCHAR(11);
    DECLARE _dCounter INT(11);
    DECLARE _maxChild INT(11);
    
    SET _dCounter = (SELECT COUNT(*) FROM commission WHERE remarks = 'upline' AND c_user_id = userId AND depth = _depth);
    SET _maxChild =  ROUND((POW(2, ( REPLACE(_depth, CONCAT('depth_', userId , '-'), '') + 1))/ 2), 0);
    
	IF _desc = 'upline'
		THEN
			IF r_userId IS NOT NULL AND (SELECT COUNT(*) FROM commission WHERE remarks = 'upline' AND c_user_id = userId AND r_user_id = r_userId) != 1
				THEN
					INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create) VALUES(userId, amount, r_userId, _depth, _desc, NOW());
					SET _return = 1;
            ELSE
				SET _return = 0;
            END IF;
            
            
            IF r_userId IS NULL AND _dCounter != _maxChild
				THEN
					INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create) VALUES(userId, amount, null, _depth, _desc, NOW());
					SET _return = 1;
			ELSE
				SET _return = 0;
            END IF;
	ELSEIF _desc = 'referral'
		THEN
			INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create) VALUES(userId, amount, r_userId, null, _desc, NOW());
			SET _return = 1;
	ELSE
			SET _return = 0;
    END IF;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `add_userDownlines` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `add_userDownlines`(IN parentId INT(11), IN childId INT(11), IN _position VARCHAR(100), IN _pos VARCHAR(100), IN createdBy VARCHAR(100))
BEGIN
	DECLARE userId VARCHAR(100);
    DECLARE _depth INT(11);
    
    IF createdBy = '' THEN 
		SET userId = 'SystemCreate';
	ELSE
		SET userId = (SELECT user_name FROM users WHERE user_id = createdBy);
	END IF;
    
    INSERT INTO hierarchy (parent, child, depth, position, m_position, created_by, datetime)
	SELECT parentId, childId, 1, _position, _pos, userId, NOW()
    UNION ALL SELECT parent, childId, depth+1, _position, _pos, userId, NOW() FROM hierarchy
		WHERE child = parentId
	UNION ALL SELECT childId, childId, 0, _position, _pos, userId, NOW();
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `check_Downlines` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `check_Downlines`(IN userId INT, IN _depth INT)
BEGIN
	DECLARE str_list VARCHAR(100);
    
	DROP TEMPORARY TABLE IF EXISTS _temp;
	CREATE TEMPORARY TABLE IF NOT EXISTS _temp AS (select 
			-- group_concat(CONCAT( n.user_name, ' (' , n.user_id , ')') order by a.parent separator ', ') as path,
			d.child, d.depth,
			get_FullName(d.child) AS full_name, COALESCE(get_Parent(d.child), d.parent) AS parent, d.position
		from hierarchy d
		join hierarchy a on (a.child = d.child)
		join users n on (n.user_id = a.parent)
		where d.parent = userId
		group by d.child);
		
	SET str_list = (SELECT GROUP_CONCAT(DISTINCT(depth) SEPARATOR ',') FROM _temp);
    
	DROP TEMPORARY TABLE IF EXISTS _hierarchy;
    CREATE TEMPORARY TABLE IF NOT EXISTS _hierarchy AS (
	select 
		-- group_concat(CONCAT( n.user_name, ' (' , n.user_id , ')') order by a.parent separator ', ') as path,
        d.child, 
		CASE 
			WHEN d.depth IN (0,1,2) THEN d.depth
            WHEN d.depth IN (1,2) THEN d.depth
            WHEN d.depth IN (2, 3) THEN 
				CASE 
					WHEN (SELECT GROUP_CONCAT(DISTINCT(depth) SEPARATOR ',') FROM _temp) =  '0,1,2,3' THEN d.depth 
					ELSE 2 END 
		ELSE d.depth END AS depth, 
        get_FullName(d.child) AS full_name, COALESCE(get_Parent(d.child), d.parent) AS parent, d.position,
        get_DescendantCount(d.child) AS child_count
	from hierarchy d
	join hierarchy a on (a.child = d.child)
	join users n on (n.user_id = a.parent)
	where d.parent = userId
	group by d.child);
    
    SELECT * FROM _hierarchy WHERE depth = _depth;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `clean_DB` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `clean_DB`()
BEGIN
	
	DECLARE cnt INT(11);
    DECLARE insert_id INT(11);
    
	TRUNCATE TABLE users;
	TRUNCATE TABLE position;
	TRUNCATE TABLE codes;
	TRUNCATE TABLE commission;
	TRUNCATE TABLE withdrawal;
	TRUNCATE TABLE hierarchy;
	TRUNCATE TABLE _selectedhierarchy;
    TRUNCATE TABLE _currentchild;
    TRUNCATE TABLE request_withdrawal;

	INSERT INTO `users`
	(`user_id`, `user_name`, `password`, `first_name`, `last_name`, `gender`, `contact`, `address`, `entered_on`, `status`, `position`, `email`, `sponsor_by`)
	VALUES
	(1, 'admin', '12345', 'Luyabaya', 'Admin', '', '(082) 282 8849', 'Jacinto Extension (Infront of Blueberry Hotel), Davao City, Philippines', NOW(), '', 1, 'luyabaya@gmail.com', ''),
	(2, 'luyabaya1', '12345', 'Luyabaya', '1', '', '(082) 282 8849', 'Jacinto Extension (Infront of Blueberry Hotel), Davao City, Philippines', NOW(), '1', 0, 'luyabaya@gmail.com', '2');

	INSERT INTO `position` (`position_id`, `user_id`, `position_left`, `position_right`, `sponsor_by`, `upline`) VALUES (1, '2', '', '', '2', '2');
	INSERT INTO `hierarchy` (`p_id`,`parent`,`child`,`depth`,`position`, `created_by`,`datetime`) VALUES (1,2,2, 0,'parent','SystemCreate', NOW());
    
    SET cnt = 2;
    WHILE cnt <= 7 DO
			INSERT INTO `users`
				(`user_name`, `password`, `first_name`, `last_name`, `gender`, `contact`, `address`, `entered_on`, `status`, `position`, `email`, `sponsor_by`)
			VALUES
				(CONCAT('luyabaya', cnt), '12345', 'Luyabaya', cnt, 'M', '(082) 282 8849', 'Jacinto Extension (Infront of Blueberry Hotel), Davao City, Philippines', NOW(), '1', 0, CONCAT('luyabaya', cnt, '@gmail.com'), '2');
			
            SET insert_id = LAST_INSERT_ID();
            INSERT INTO `position` (`user_id`, `position_left`, `position_right`, `sponsor_by`) VALUES (insert_id, '', '', '2');
            INSERT INTO `commission` (`c_user_id`, `c_amount`, `r_user_id`, `depth`, `remarks`, `date_create`) VALUES ('2', '50', insert_id, NULL, 'referral', NOW());
			INSERT INTO `hierarchy` (`parent`,`child`,`depth`,`position`, `created_by`,`datetime`) VALUES (cnt,cnt, 0,'parent','SystemCreate', NOW());
        SET cnt = cnt + 1;
    END WHILE;
    
    UPDATE position SET position_left = 3, position_right = 4, upline = 2 WHERE user_id = 2;
	UPDATE position SET position_left = 5, position_right = 6, upline = 3 WHERE user_id = 3;
	UPDATE position SET position_left = 7, position_right = 8, upline = 4 WHERE user_id = 4;
    
    -- CALL add_userDownlines('parent_id', 'child_id', 'position', 'main_position', 'system')
    CALL add_userDownlines(2, 3, 'left', 'left', '');
    CALL add_userDownlines(2, 4, 'right', 'right', '');
    CALL add_userDownlines(3, 5, 'left', 'left', '');
    CALL add_userDownlines(3, 6, 'right', 'left', '');
    CALL add_userDownlines(4, 7, 'left', 'right', '');
    CALL add_userDownlines(4, 8, 'right', 'right', '');
    
    SET cnt = 1;
    WHILE cnt <= 100 DO
		INSERT INTO codes  (description, generated_date, generated_by, status) VALUES (CONCAT('a', cnt), NOW(), 'SystemCreate', 0);
        SET cnt = cnt + 1;
    END WHILE;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `generateCommission` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `generateCommission`(IN userId INT(11), IN _depth VARCHAR(50), IN cLevel INT(11))
BEGIN
	DECLARE maxChild INT(11);
    DECLARE iCommission INT(11);
    DECLARE cnt INT(11);
    
    SET iCommission = (SELECT COUNT(*) FROM commission WHERE c_user_id = userId AND depth = _depth LIMIT 1);
    SET maxChild = POW(2, ( _depth));
    
    IF (ROUND((maxChild / 2), 2)) != iCommission
    THEN
		SET cnt = 1;
        WHILE cnt <= cLevel DO
			INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create)
				VALUES(userId, 60, userId, _depth, 'upline', NOW());
			SET cnt = cnt + 1;
		END WHILE;
    END IF;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `generateCommissionSvc` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `generateCommissionSvc`(IN userId INT(11))
BEGIN
	DECLARE tree_generate INT(11);
    DECLARE generated_commission INT(11);
    DECLARE count_commissions INT(11);

	CALL get_Hierarchy(userId);
    SET tree_generate = (SELECT ROW_COUNT());
    
    IF userId = (SELECT user_id FROM users WHERE user_name = 'luyabaya1' OR user_id = 2)
		THEN
			CALL get_childsDepth();
	END IF;
    
    SET count_commissions = (SELECT COUNT(*) FROM commission WHERE c_user_id = userId AND remarks = 'upline' AND DATE(date_create) = DATE(NOW()));
    
    IF count_commissions <= 10
		THEN
        CALL generate_userCommission(userId);
		SET generated_commission = (SELECT ROW_COUNT());
	ELSE
		SET generated_commission = 0;
	END IF;
    
    SELECT tree_generate, generated_commission;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `generate_userCommission` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `generate_userCommission`(IN userId INT)
BEGIN
	DECLARE _depth INT(11);
    DECLARE _maxChild INT(11);
    DECLARE _childCount INT(11);
    DECLARE maxDepth INT(11);
    DECLARE rCount INT(11);
    DECLARE lCount INT(11);
	DECLARE newCount INT(11);
    DECLARE oldCount INT(11);
    DECLARE upCount INT(11);
    DECLARE pairs INT(11);
    DECLARE pairCount INT(11);
    DECLARE cnt INT(11);
    DECLARE _cnt INT(11);
    
    IF (SELECT COUNT(*) FROM _selectedhierarchy WHERE m_parent = userId AND depth = 0) = 2
	    THEN
			SET upCount = (SELECT SUM(count_childs(s.child)) AS count
						FROM   _selectedhierarchy s
						WHERE EXISTS (SELECT 1 
										   FROM   _currentchild c
										   WHERE  s.child = c.parent)
						AND s.m_parent = userId AND m_position IS NULL);
		ELSE
			SET upCount = (SELECT SUM(count_childs(s.child)) AS count
						FROM   _selectedhierarchy s
						WHERE EXISTS (SELECT 1 
										   FROM   _currentchild c
										   WHERE  s.child = c.parent)
						AND s.m_parent = userId);
		END IF;

	SET oldCount = (SELECT SUM(old_count) FROM _currentchild WHERE parent = userId);
	SET newCount = (SELECT SUM(new_count) FROM _currentchild WHERE parent = userId);
    
    SET _depth = 1;
    SET maxDepth = (SELECT MAX(depth) FROM _selectedhierarchy);
    
    WHILE _depth <= maxDepth DO
		SET _maxChild = POW(2, ( _depth));
        SET _childCount = (SELECT COUNT(*) FROM _selectedhierarchy WHERE depth = _depth);
        SET pairCount = (SELECT COUNT(*) FROM commission WHERE c_user_id = userId and r_user_id = userId);
        
        IF (SELECT COUNT(*) FROM _selectedhierarchy WHERE m_parent = userId AND depth = 0) = 2
	    THEN
			SET lCount = (SELECT COUNT(*) FROM _selectedhierarchy WHERE depth = _depth AND m_parent = userId AND position = 'left' AND depth != 0);
			SET rCount = (SELECT COUNT(*) FROM _selectedhierarchy WHERE depth = _depth AND m_parent = userId AND position = 'right' AND depth != 0);
		ELSE
			SET lCount = (SELECT COUNT(*) FROM _selectedhierarchy WHERE depth = _depth AND m_parent = userId AND m_position = 'left' AND depth != 0);
			SET rCount = (SELECT COUNT(*) FROM _selectedhierarchy WHERE depth = _depth AND m_parent = userId AND m_position = 'right' AND depth != 0);
		END IF;
        
         IF newCount != oldCount
			THEN
				IF oldCount < newCount
					THEN
						IF lCount != 0 AND rCount != 0 AND lCount = rCount
						THEN
							IF _maxChild = _childCount
								THEN
									SET pairs = ((_childCount / 2));
									SET cnt = 1;
									WHILE cnt <= pairs DO
										INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create)
											VALUES(userId, 60, userId, CONCAT('pairing_', pairCount + 1), 'upline', NOW());
										SET cnt = cnt + 1;
									END WHILE;
							ELSE
								SET pairs = (((lCount + rCount) / 2));
								SET cnt = 1;
								WHILE cnt <= pairs DO
									INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create)
										VALUES(userId, 60, userId, CONCAT('pairing_', pairCount + 1), 'upline', NOW());
									SET cnt = cnt + 1;
								END WHILE;
							END IF;
						END IF;
				ELSEIF newCount != upCount
					THEN
						IF lCount != 0 AND rCount != 0 AND lCount = rCount
						THEN
							IF _maxChild = _childCount
								THEN
									SET pairs = ((_childCount / 2));
									SET cnt = 1;
									WHILE cnt <= pairs DO
										INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create)
											VALUES(userId, 60, userId, CONCAT('pairing_', pairCount + 1), 'upline', NOW());
										SET cnt = cnt + 1;
									END WHILE;
							ELSE
								SET pairs = (((lCount + rCount) / 2));
								SET cnt = 1;
								WHILE cnt <= pairs DO
									INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create)
										VALUES(userId, 60, userId, CONCAT('pairing_', pairCount + 1), 'upline', NOW());
									SET cnt = cnt + 1;
								END WHILE;
							END IF;
						END IF;
				END IF;
		ELSE
			IF (SELECT COUNT(*) FROM commission WHERE c_user_id = userId and r_user_id = userId) = 0
				THEN
					SET cnt = 1;
					WHILE cnt <= pairs DO
						INSERT INTO commission (c_user_id, c_amount, r_user_id, depth, remarks, date_create)
							VALUES(userId, 60, userId, CONCAT('pairing_', cnt), 'upline', NOW());
						-- SELECT 'count', upCount, newCount, oldCount, 's';
						SET cnt = cnt + 1;
					END WHILE;
				END IF;
		END IF;
        
        SET _depth = _depth + 1;
    END WHILE;
    CALL update_ChildCount(userId);
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `generate_userTree` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `generate_userTree`(IN userId INT)
BEGIN
	DECLARE maxDepth INT(11);
    DECLARE _depth INT(11);

	DROP TEMPORARY TABLE IF EXISTS _userTree;
	CREATE TEMPORARY TABLE IF NOT EXISTS _userTree AS (
	SELECT *, get_DescendantCount(child, depth) AS child_count FROM _selectedhierarchy);
    
    SET _depth = 0;
    SET maxDepth = (SELECT MAX(depth) FROM _userTree);
    
    SELECT  child, depth, full_name, parent, position, m_position FROM _userTree ORDER BY depth ASC, position ASC;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_AvailableDownline` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_AvailableDownline`(IN userId INT, IN isPosition VARCHAR(2))
BEGIN
	  CALL get_Hierarchy(userId);
      
      DROP TABLE IF EXISTS availableDownline;
      IF isPosition = 'L' THEN
      
                  CREATE TEMPORARY TABLE IF NOT EXISTS availableDownline AS 
					(SELECT u.user_id as u_user_id,u.first_name as u_first_name,u.last_name as u_last_name,u.contact as u_contact,u.address as u_address,u.email as u_email,u.gender as u_gender,u.sponsor_by as u_sponsor,
					  p.position_id as p_position_id, p.user_id as p_user_id, p.sponsor_by as p_sponsor_by, pl.child AS p_left, pr.child AS p_right
						FROM users u
								LEFT JOIN position p ON p.position_left = u.user_id
								LEFT JOIN users up ON up.user_id = p.upline
								LEFT JOIN _selectedhierarchy sh ON sh.child = u.user_id
								LEFT JOIN _selectedhierarchy pl ON pl.parent = sh.child AND pl.position = 'left'
								LEFT JOIN _selectedhierarchy pr ON pr.parent = sh.child AND pr.position = 'right'
						WHERE sh.f_position = 'left');
      ELSE
					CREATE TEMPORARY TABLE IF NOT EXISTS availableDownline AS
					(SELECT u.user_id as u_user_id,u.first_name as u_first_name,u.last_name as u_last_name,u.contact as u_contact,u.address as u_address,u.email as u_email,u.gender as u_gender,u.sponsor_by as u_sponsor,
					  p.position_id as p_position_id, p.user_id as p_user_id, p.sponsor_by as p_sponsor_by, pl.child AS p_left, pr.child AS p_right
						FROM users u
								LEFT JOIN position p ON p.position_left = u.user_id
								LEFT JOIN users up ON up.user_id = p.upline
								LEFT JOIN _selectedhierarchy sh ON sh.child = u.user_id
								LEFT JOIN _selectedhierarchy pl ON pl.parent = sh.child AND pl.position = 'left'
								LEFT JOIN _selectedhierarchy pr ON pr.parent = sh.child AND pr.position = 'right'
						WHERE sh.f_position = 'right');

      END IF;

      SELECT * FROM availableDownline WHERE (p_left IS NULL OR  p_right IS NULL);
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_childsDepth` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_childsDepth`()
BEGIN
	DECLARE _notEmpty INT(11);

    SET _notEmpty = (SELECT TABLE_ROWS
						FROM information_schema.TABLES
						WHERE TABLE_SCHEMA = 'luyabaya_binary'
						AND TABLE_NAME = '_currentchild'
						AND TABLE_TYPE = 'BASE TABLE');
	IF _notEmpty = 0
		THEN
        INSERT INTO _currentchild (parent, old_count, new_count)
		SELECT DISTINCT c.child, 0, count_childs(child)
			FROM _selectedhierarchy c;
	ELSEIF _notEmpty > 0
		THEN
			INSERT INTO _currentchild (parent, old_count, new_count)
			SELECT DISTINCT s.child, 0, count_childs(child)
			FROM   _selectedhierarchy s
			WHERE NOT EXISTS (SELECT 1 
							   FROM   _currentchild c
							   WHERE  s.child = c.parent);
    END IF;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_downLines` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_downLines`(IN userId INT)
BEGIN
	SELECT u.user_id, CONCAT(u.first_name, ' ', u.last_name) AS full_name, u.user_name,
	(CASE
		WHEN p.position_right = u.user_id THEN 'R'
		WHEN p.position_left = u.user_id THEN 'L'
		ELSE ''
	END) AS isPosition,
	CONCAT(up.first_name, ' ', up.last_name, ' (', up.user_name, ')') AS up_FName,
	up.user_id AS upline_userId, DATE_FORMAT(u.entered_on,'%m-%d-%Y') AS date_joined
	FROM users u
		LEFT JOIN position p ON p.position_left = u.user_id OR p.position_right = u.user_id
		LEFT JOIN users up ON up.user_id = p.upline
	WHERE p.upline = userId;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_Hierarchy` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_Hierarchy`(IN userId INT)
BEGIN
	DROP TEMPORARY TABLE IF EXISTS _hierarchy;
    CREATE TEMPORARY TABLE IF NOT EXISTS _hierarchy AS (
	SELECT
		DISTINCT t.user_id AS child,
		CONCAT(t.first_name, ' ', t.last_name) AS full_name,
		h.depth,
		get_Parent(t.user_id) AS parent,
        h.position,
        h.m_position,
        h.parent AS m_parent
	FROM users t
		JOIN hierarchy h ON t.user_id = h.child
	WHERE h.parent = userId);
    
    TRUNCATE TABLE _selectedhierarchy;
    INSERT INTO _selectedhierarchy (child, depth, full_name, parent, position, m_position, m_parent, f_position)
		SELECT child, depth, full_name, parent, position, m_position, m_parent, NULL FROM _hierarchy;
        
	IF (SELECT COUNT(*) FROM _selectedhierarchy WHERE m_parent = userId AND depth = 0) = 2
	THEN
		DELETE FROM _selectedhierarchy WHERE child = userId AND m_position IS NOT NULL;
	END IF;
        
	DROP TEMPORARY TABLE IF EXISTS __hierarchy;
	CREATE TEMPORARY TABLE IF NOT EXISTS __hierarchy AS
	(SELECT s.*, 
		(CASE 
			WHEN m_position IS NULL THEN 'parent'
			WHEN (SELECT child FROM `luyabaya_binary`.hierarchy WHERE parent = ((SELECT DISTINCT child FROM hierarchy WHERE parent = userId AND depth = 1 AND position = 'left' LIMIT 1)) AND child IN (s.child) LIMIT 1) IS NOT NULL 
				THEN 'left'
            WHEN (SELECT child FROM `luyabaya_binary`.hierarchy WHERE parent = ((SELECT DISTINCT child FROM hierarchy WHERE parent = userId AND depth = 1 AND position = 'right' LIMIT 1)) AND child IN (s.child) LIMIT 1) IS NOT NULL 
				THEN 'right'
		END) AS fin_position FROM _selectedhierarchy s
	ORDER BY s.depth);
    
    TRUNCATE TABLE _selectedhierarchy;
    INSERT INTO _selectedhierarchy (child, depth, full_name, parent, position, m_position, m_parent, f_position)
		SELECT child, depth, full_name, parent, position, m_position, m_parent, fin_position FROM __hierarchy;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_MembersAssigned` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_MembersAssigned`(IN userId INT, IN isPosition VARCHAR(2))
BEGIN

	IF isPosition = 'L' THEN
			SELECT u.user_id, CONCAT(u.first_name, ' ', u.last_name) AS full_name, u.user_name,
				CONCAT(up.first_name, ' ', up.last_name, ' (', up.user_name, ')') AS up_FName,
				up.user_id AS upline_userId, DATE_FORMAT(u.entered_on,'%m-%d-%Y') AS date_joined
				FROM users u
					LEFT JOIN position p ON p.position_left = u.user_id
					LEFT JOIN users up ON up.user_id = p.upline
				WHERE p.upline = userId AND p.position_left >  0;
	ELSE
			SELECT u.user_id, CONCAT(u.first_name, ' ', u.last_name) AS full_name, u.user_name,
				CONCAT(up.first_name, ' ', up.last_name, ' (', up.user_name, ')') AS up_FName,
				up.user_id AS upline_userId, DATE_FORMAT(u.entered_on,'%m-%d-%Y') AS date_joined
				FROM users u
					LEFT JOIN position p ON p.position_right = u.user_id
					LEFT JOIN users up ON up.user_id = p.upline
				WHERE p.upline = userId AND p.position_right > 0;
	END IF;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_selectedHierarchy` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_selectedHierarchy`(IN _val INT, IN _get VARCHAR(11))
BEGIN
	IF _get = 'depth'
		THEN
			SELECT *, get_DescendantCount(child, depth) AS child_count FROM _selectedhierarchy WHERE depth = _val;
	ELSE
			SELECT *, get_DescendantCount(child, depth) AS child_count FROM _selectedhierarchy WHERE parent = _val OR child = _val;
		END IF;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_unAssignedUsers` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_unAssignedUsers`(IN userId INT)
BEGIN
	
	DROP TABLE IF EXISTS unassignedUsers;
	CREATE TEMPORARY TABLE IF NOT EXISTS unassignedUsers AS
	(SELECT 
		u.user_id as u_user_id, u.first_name as u_first_name, u.last_name as u_last_name,
		u.contact as u_contact, u.address as u_address, u.email as u_email, u.gender as u_gender,
		u.sponsor_by as u_sponsor,
		p.position_id as p_position_id,
		p.user_id as p_user_id, p.sponsor_by as p_sponsor_by, p.position_left as p_left, p.position_right as p_right
		FROM users u 
		JOIN position p ON p.user_id = u.user_id
		WHERE
			NOT EXISTS(SELECT pos.position_right 
						  FROM   position pos
						  WHERE  pos.position_right <> '' AND pos.position_right = u.user_id
						 )
			AND
			NOT EXISTS(SELECT pos.position_left 
						  FROM   position pos
						  WHERE  pos.position_left <> '' AND pos.position_left = u.user_id
						 ));

	SELECT * FROM unassignedUsers
		WHERE p_left = '' AND p_right = '' AND p_user_id != userId AND p_sponsor_by = userId;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_userCashOnHand` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_userCashOnHand`(IN userId INT)
BEGIN
	DROP TABLE IF EXISTS totalCashOnHand;

	CREATE TEMPORARY TABLE IF NOT EXISTS totalCashOnHand AS(
	SELECT COALESCE(SUM(c.c_amount), 0) AS total_commissions, (SELECT COALESCE(SUM(w.w_amount), 0) FROM withdrawal w WHERE w.w_user_id = userId ) AS total_withdrawals
		FROM commission c WHERE c.c_user_id = userId);

	SELECT CONVERT(COALESCE((total_commissions - total_withdrawals), 0), DECIMAL(10, 2)) AS totalCashOnHand FROM totalCashOnHand;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_userCommissions` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_userCommissions`(IN userId INT)
BEGIN
	SELECT 
		CONCAT('Php. ', CONVERT(COALESCE(c_amount, 0), DECIMAL(10, 2))) AS c_amount, 
		DATE_FORMAT(date_create,'%m-%d-%Y') AS date_create, 
        (CASE 
			WHEN remarks = 'referral' THEN 'Direct Referral'
            WHEN remarks = 'upline' THEN 'Pairing'
			ELSE ''
		END) AS remarks
	FROM commission WHERE c_user_id = userId;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_userCount` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_userCount`(IN fName VARCHAR(100), IN lName VARCHAR(100))
BEGIN
	
	SELECT COUNT(*) AS counter FROM users WHERE LTRIM(RTRIM(REPLACE(first_name, ' ', ''))) = LTRIM(RTRIM(REPLACE(fName, ' ', '')))
		AND LTRIM(RTRIM(REPLACE(last_name, ' ', ''))) = LTRIM(RTRIM(REPLACE(lName, ' ', '')));
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_userMembers` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_userMembers`(IN userId INT)
BEGIN
	SELECT 
	(CASE WHEN
		position_left = '' THEN 0 ELSE COALESCE(COUNT(position_left), 0) END) + (CASE WHEN
		position_right = '' THEN 0 ELSE COALESCE(COUNT(position_right), 0) END) AS countMembers
	FROM position WHERE user_id = userId;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `get_userWithdrawals` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `get_userWithdrawals`(IN userId INT)
BEGIN
	SELECT 
		CONCAT('Php. ', CONVERT(COALESCE(w_amount, 0), DECIMAL(10, 2))) AS w_amount, 
		DATE_FORMAT(date_create,'%m-%d-%Y') AS date_create 
	FROM withdrawal WHERE w_user_id = userId;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `update_ChildCount` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`admindev_db`@`localhost` PROCEDURE `update_ChildCount`(IN `parentId` INT(11))
BEGIN
    DECLARE _oldCount INT(11);
    DECLARE _newCount INT(11);
    DECLARE count INT(11);

	SET _newCount = (SELECT COUNT(*) AS descendant_count
				FROM _selectedhierarchy AS c
					INNER JOIN users AS u ON (u.user_id = c.child)
				WHERE c.m_parent = parentId AND depth != 0);
                
	SET _oldCount = (SELECT old_count FROM _currentchild WHERE parent = parentId);
	SET count = (SELECT new_count FROM _currentchild WHERE parent = parentId);
    
    IF _oldCount != _newCount
		THEN
			IF _newCount != count
				THEN
					UPDATE _currentchild SET old_count = count, new_count = _newCount WHERE parent = parentId;
			ELSEIF _oldCount < _newCount
				THEN
					UPDATE _currentchild SET old_count = count, new_count = _newCount WHERE parent = parentId;
            END IF;
	END IF;
    
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2017-03-08 19:55:41
