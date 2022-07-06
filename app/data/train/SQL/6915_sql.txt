SELECT [GROUP].group_name, [GROUP].group_day, [GROUP].group_time, COUNT(dbo.STUDENT.group_id) AS 'GroupCount' 
FROM dbo.STUDENT INNER JOIN [GROUP] ON STUDENT.group_id = [GROUP].group_id
GROUP BY [GROUP].group_id, [GROUP].group_name, [GROUP].group_day, [GROUP].group_time
ORDER BY [GROUP].group_id;

SELECT STUDENT.group_id, COUNT(CAST(dbo.STUDENT.group_id AS INTEGER)) AS 'GroupCount'
FROM dbo.STUDENT 
GROUP BY STUDENT.group_id, 

