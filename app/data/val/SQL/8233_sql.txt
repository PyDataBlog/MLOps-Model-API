USE [StayHomewithoutPer];
GO

WITH po
    AS ( SELECT AVG ( e.Gehalt ) AS te ,
			 [Branch ID]
		 FROM Mit e
		 GROUP BY e.[Branch ID] ) ,pod
    AS ( SELECT AVG ( r.Gehalt ) AS tr
		 FROM Mit r ) 
    SELECT po.[Branch ID]
	 FROM po ,pod
	 WHERE po.te > pod.tr;
GO


