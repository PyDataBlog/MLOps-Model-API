CREATE DATABASE IF NOT EXISTS foodbuddy_db ;
USE foodbuddy_db ;

CREATE TABLE IF NOT EXISTS vendors (
	id INT PRIMARY KEY AUTO_INCREMENT,
	username TEXT,
	password TEXT,
	company TEXT,
	street_number INT,
	city TEXT,
	state TEXT,
	zip_code INT
) ;

CREATE TABLE IF NOT EXISTS members (
	snap_id INT PRIMARY KEY,
	username TEXT,
	password TEXT,
	fname TEXT,
	lname TEXT
) ;

CREATE TABLE IF NOT EXISTS food (
	food_id INT PRIMARY KEY AUTO_INCREMENT,
	name TEXT
) ;