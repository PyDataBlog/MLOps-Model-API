-- Run from command line: $ sqlite3 -init thop-prov.sql

.mode column
.header on 

CREATE TABLE hop (S text, T text, P text);

INSERT INTO hop VALUES ("a","a", "p");
INSERT INTO hop VALUES ("a","b", "q");
INSERT INTO hop VALUES ("b","a", "r");
INSERT INTO hop VALUES ("b","c", "s");

CREATE VIEW thop AS 
    SELECT h1.S, h3.T, h1.P||'*'||h2.P||'*'||h3.P AS P
    FROM   hop h1, hop h2, hop h3
    WHERE  h1.T = h2.S AND h2.T = h3.S ;

SELECT   S, T, group_concat(P, ' + ') 
FROM     thop 
GROUP BY S, T;


