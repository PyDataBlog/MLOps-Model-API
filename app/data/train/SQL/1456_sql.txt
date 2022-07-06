DROP TABLE IF EXISTS dayoftheweek
Create Table DayofTheWeek ( dayint INT identity (1,1), days nvarchar(10),   bit_value   AS 
                CONVERT
                (
                    integer, 
                    POWER(2, dayint - 1)
                )
                PERSISTED UNIQUE CLUSTERED)


 INSERT INTO DayofTheWeek (days)
 VALUES('Monday')
  INSERT INTO DayofTheWeek (days)
 VALUES('Tuesday')
  INSERT INTO DayofTheWeek (days)
 VALUES('Wednesday')
  INSERT INTO DayofTheWeek (days)
 VALUES('Thursday')
  INSERT INTO DayofTheWeek (days)
 VALUES('Friday')
  INSERT INTO DayofTheWeek (days)
 VALUES('Saturday')
  INSERT INTO DayofTheWeek (days)
 VALUES('Sunday')

 DROP TABLE IF EXISTS DayScheduled
 CREATE TABLE DayScheduled (daykey INT IDENTITY(1,1), DayOfWeekCombination  NVARCHAR(500))

 --SELECT * FROM DayofTheWeek

 -- Maximum integer we need
-- for all combinations of 'n' bits
DECLARE 
    @max integer = 
    POWER(2,
        (
            SELECT COUNT(*) 
            FROM DayofTheWeek AS s
        )
    ) - 1;


INSERT INTO DayScheduled (DayOfWeekCombination)
SELECT
    DaysOfTheWeekCombo =
        STUFF
        (
            (
                -- Choose items where the bit is set
                -- and concatenate all matches
                SELECT ',' + s.days 
                FROM DayofTheWeek AS s
                WHERE
                    n.n & s.bit_value = s.bit_value
                ORDER BY
                    s.bit_value
                FOR XML 
                    PATH (''),
                    TYPE                    
            ).value('(./text())[1]', 'varchar(8000)'), 1, 1, ''
        )
-- A standard numbers table
-- (single column, integers from 1 to 1048576, indexed)
FROM dbo.Numbers AS N
WHERE
    N.n BETWEEN 1 AND @max;


SELECT * FROM DayScheduled
Order by dayofweekcombination, LEN(dayofweekcombination) ASC