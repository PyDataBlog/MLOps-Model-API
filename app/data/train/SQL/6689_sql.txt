--Question: https://www.hackerrank.com/challenges/projects/problem
--Algorithm: http://oracleobsession.blogspot.com/2014/09/tabibitosan-method-tabibitosan-is-one.html


;with temps1 as 
(select start_date, end_date, row_number() over (order by start_date) rowNum from projects)
, temps as (select start_date, end_date, datediff(day, rowNum, start_date) as fac from temps1)
select min(start_date), max(end_date), datediff(day, min(start_date), max(end_date))
from temps
group by fac
order by  datediff(day, min(start_date), max(end_date)), min(start_date)