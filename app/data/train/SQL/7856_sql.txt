CREATE TABLE IF NOT EXISTS `EatCategory` ( 
						    `categoryId`	INTEGER PRIMARY KEY AUTOINCREMENT, 
	                        `categoryName`	TEXT NOT NULL, 
    		                  `categoryType`	INTEGER NOT NULL );
CREATE TABLE IF NOT EXISTS `EatRecord` ( 
                          `recordDate`	TEXT,
	                      `eatCategory`	INTEGER, 
                          `recordUrls`	TEXT, 
				          PRIMARY KEY(recordDate,eatCategory));