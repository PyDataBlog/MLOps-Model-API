/* This file will create the tables with desired params to run
   the Rails app */
  
CREATE TABLE unitConversions (
	id INTEGER PRIMARY KEY AUTO_INCREMENT NOT NULL,
	name VARCHAR(64),
	toFlOz DECIMAL,
	toGallons DECIMAL,
	toLiters DECIMAL,
	toWtOzs DECIMAL,
	toPounds DECIMAL,
	toGrams DECIMAL
);

INSERT INTO unitConversions(name,toFlOz,toGallons,toLiters,toWtOzs,toPounds,
	toGrams) VALUES 
	('flOz', 1, 0.0078125, 0.0295735, NULL, NULL, NULL),
	('gallons', 128, 1, 3.78541, NULL, NULL, NULL),
	('liters', 33.814, 0.264172, 1, NULL, NULL, NULL),
	('wtOzs', NULL, NULL, NULL, 1, 0.0625, 28.3495),
	('pounds', NULL, NULL, NULL, 16, 1, 453.592),
	('grams', NULL, NULL, NULL, 0.035274, 0.00220462, 1);
	

CREATE TABLE users (
	id INTEGER PRIMARY KEY AUTO_INCREMENT NOT NULL,
	username VARCHAR(32),
	firstname VARCHAR(64),
	lastname VARCHAR(64),
	passwd VARCHAR(256),
	restaurant INTEGER,
	created_at DATETIME,
	updated_at DATETIME
);

INSERT INTO users (username, firstname, lastname, passwd, restaurant) VALUES
	('testUser', 'Some', 'Person', 'securePasswd', 1);

CREATE TABLE restaurants (
	id INTEGER PRIMARY KEY AUTO_INCREMENT NOT NULL,
	name VARCHAR(64),
	city VARCHAR(64),
	state VARCHAR(32),
	created_at DATETIME,
	updated_at DATETIME
);

INSERT INTO restaurants (name, city, state) VALUES
	("Uncle Moe's Family Feedbag", 'Springfield', 'SP');

CREATE TABLE foodProducts (
	id INTEGER PRIMARY KEY AUTO_INCREMENT NOT NULL,
	restaurant INTEGER,
	name VARCHAR(64),
	orderNo VARCHAR(64),
	unitPrice DECIMAL,
	unitType INTEGER,
	unitSize DECIMAL,
	pricePerUnit DECIMAL,
	locations VARCHAR(256),
	created_at DATETIME,
	updated_at DATETIME
);