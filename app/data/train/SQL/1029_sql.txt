	--> Section 00. DB Creation
--CREATE DATABASE WashingMachineService
--GO
USE WashingMachineService
GO
	--> Section 01. DDL (30 pts)
CREATE TABLE Clients (
	ClientId INT PRIMARY KEY IDENTITY,
	FirstName NVARCHAR(50),
	LastName NVARCHAR(50),
	Phone NCHAR(12)
)

CREATE TABLE Mechanics (
	MechanicId INT PRIMARY KEY IDENTITY,
	FirstName NVARCHAR(50),
	LastName NVARCHAR(50),
	Address NVARCHAR(255)
)

CREATE TABLE Models (
	ModelId INT PRIMARY KEY IDENTITY,
	Name NVARCHAR(50) UNIQUE
)

CREATE TABLE Jobs (
	JobId INT PRIMARY KEY IDENTITY,
	ModelId INT,
	Status NVARCHAR(11) CHECK(LEN(Status) < 12 
								AND (Status = 'Pending' OR Status = 'In Progress' OR Status = 'Finished'))
						DEFAULT 'In Progress',
	ClientId INT,
	MechanicId INT,
	IssueDate DATE,
	FinishDate DATE NULL,
	CONSTRAINT FK_Jobs_Models FOREIGN KEY(ModelId)
	REFERENCES Models(ModelId),
	CONSTRAINT FK_Jobs_Clients FOREIGN KEY(ClientId)
	REFERENCES Clients(ClientId),
	CONSTRAINT FK_Jobs_Mechanics FOREIGN KEY(MechanicId)
	REFERENCES Mechanics(MechanicId),
)

CREATE TABLE Vendors(
	VendorId INT PRIMARY KEY IDENTITY,
	Name NVARCHAR(50) UNIQUE
)

CREATE TABLE Parts(
	PartId INT PRIMARY KEY IDENTITY,
	SerialNumber NVARCHAR(50) UNIQUE,
	Description NVARCHAR(255) NULL,
	Price MONEY CHECK(Price > 0 AND Price <= 9999.99),
	VendorId INT,
	StockQty INT CHECK(StockQty >= 0) DEFAULT 0,
	CONSTRAINT FK_Parts_Vendors FOREIGN KEY(VendorId)
	REFERENCES Vendors(VendorId)
)

CREATE TABLE PartsNeeded(
	JobId INT NOT NULL,
	PartId INT NOT NULL,
	Quantity INT CHECK(Quantity > 0) DEFAULT 1,
	CONSTRAINT PK_JobsParts PRIMARY KEY(JobId, PartId),
	CONSTRAINT FK_PartsNeeded_Jobs FOREIGN KEY(JobId)
	REFERENCES Jobs(JobId),
	CONSTRAINT FK_PartsNeeded_Parts FOREIGN KEY(PartId)
	REFERENCES Parts(PartId)
)

CREATE TABLE Orders(
	OrderId INT PRIMARY KEY IDENTITY,
	JobId INT,
	IssueDate DATE NULL,
	Delivered BIT DEFAULT 0,
	CONSTRAINT FK_Orders_Jobs FOREIGN KEY(JobId)
	REFERENCES Jobs(JobId)
)

CREATE TABLE OrderParts(
	OrderId INT NOT NULL,
	PartId INT NOT NULL,
	Quantity INT CHECK(Quantity > 0) DEFAULT 1,
	CONSTRAINT PK_OrdersParts PRIMARY KEY(OrderId, PartId),
	CONSTRAINT FK_OrderParts_Orders FOREIGN KEY(OrderId)
	REFERENCES Orders(OrderId),
	CONSTRAINT FK_OrderParts_Parts FOREIGN KEY(PartId)
	REFERENCES Parts(PartId)
)

	--> Section 2. DML (10 pts)
--> Imported Data.sql
--> 02. Insert

BEGIN TRANSACTION

INSERT INTO Clients(FirstName, LastName, Phone)
VALUES
('Teri', 'Ennaco', '570-889-5187'),
('Merlyn', 'Lawler', '201-588-7810'),
('Georgene', 'Montezuma', '925-615-5185'),
('Jettie', 'Mconnell', '908-802-3564'),
('Lemuel', 'Latzke', '631-748-6479'),
('Melodie', 'Knipp', '805-690-1682'),
('Candida', 'Corbley', '908-275-8357')

ROLLBACK


BEGIN TRANSACTION

INSERT INTO Parts(SerialNumber, Description, Price,  VendorId)
VALUES
('WP8182119', 'Door Boot Seal', 117.86, 2),
('W10780048', 'Suspension Rod', 42.81, 1),
('W10841140', 'Silicone Adhesive', 6.77, 4),
('WPY055980', 'High Temperature Adhesive', 13.94, 3)

ROLLBACK

	--> 03. Update
BEGIN TRANSACTION

UPDATE Jobs
   SET Status = 'In Progress',
       MechanicId = 3
 WHERE JobId IN (SELECT JobId
          FROM Jobs
		 WHERE MechanicId IS NULL)

ROLLBACK

--> 04. Delete
BEGIN TRANSACTION

DELETE FROM OrderParts
 WHERE OrderId IN (SELECT OrderId
                     FROM OrderParts
					WHERE OrderId = 19)

DELETE FROM Orders
 WHERE OrderId = 19

ROLLBACK

	--> Section 3. Querying (45 pts)
	--> 5.
SELECT c.FirstName, c.LastName, c.Phone
  FROM Clients AS c
 ORDER BY c.LastName, c.ClientId DESC

	--> 6.
SELECT j.Status, j.IssueDate
  FROM Jobs AS j
 WHERE j.Status != 'Finished'
 ORDER BY j.IssueDate, j.JobId

	--> 7.
SELECT CONCAT(m.FirstName, ' ', m.LastName) AS Mechanic,
	   j.Status,
	   j.IssueDate 
  FROM Mechanics AS m
 INNER JOIN Jobs AS j
    ON	m.MechanicId = j.MechanicId
 ORDER BY m.MechanicId, j.IssueDate, j.JobId

	--> 8.
SELECT CONCAT(c.FirstName, ' ', c.LastName) AS Client,
       DATEDIFF(DAY, j.IssueDate, '2017-04-23') + 1 AS [Days Going],
       j.Status
  FROM Clients AS c
 INNER JOIN Jobs AS j
    ON j.ClientId = c.ClientId
 WHERE j.Status != 'Finished'
 ORDER BY [Days Going] DESC, c.ClientId

	--> 9.
SELECT CONCAT(FirstName, ' ', LastName) AS Mechanic,
       a.AVGDays AS AverageDays
  FROM Mechanics AS m
  JOIN 
(
	SELECT m.MechanicId AS 'Mechanic',
	       AVG(DATEDIFF(DAY, j.IssueDate, j.FinishDate)) AS 'AVGDays'
	  FROM Jobs AS j
	  JOIN Mechanics AS m
	    ON m.MechanicId = j.MechanicId
	 WHERE STATUS = 'Finished'
	 GROUP BY m.MechanicId
) AS a
    ON m.MechanicId = a.Mechanic 

	--> 10.
SELECT TOP(3) CONCAT(FirstName, ' ', LastName) AS Mechanic,
	   a.CountOfActiveJobs AS Jobs
  FROM Mechanics AS m
 INNER JOIN
(
	SELECT j.MechanicId,
	       COUNT(j.MechanicId) AS 'CountOfActiveJobs'
	  FROM Jobs AS j
	 WHERE j.Status != 'Finished'
	 GROUP BY j.MechanicId
) AS a
    ON a.MechanicId = m.MechanicId
 WHERE a.CountOfActiveJobs > 1
 ORDER BY a.CountOfActiveJobs DESC, m.MechanicId

	--> 11.

SELECT CONCAT(m.FirstName, ' ', m.LastName) AS Available
  FROM Mechanics AS m
 INNER JOIN
(
	SELECT * 
	  FROM Mechanics
	 WHERE MechanicId NOT IN
		(
		SELECT MechanicId FROM Jobs
		 WHERE STATUS != 'Finished' 
		   AND MechanicId IS NOT NULL
		)
) AS s
    ON s.MechanicId = m.MechanicId

	--> 12.
SELECT ISNULL(SUM(p.Price * op.Quantity), 0) AS [Parts Total] 
  FROM Parts AS p
 INNER JOIN OrderParts AS op
    ON op.PartId = p.PartId
 INNER JOIN Orders AS o 
    ON o.OrderId = op.OrderId
 WHERE o.IssueDate > (DATEADD(WEEK, -3 , '2017/04/24'))

	--> 13.
SELECT j.JobId,
       ISNULL(SUM(op.Quantity * p.Price), 0) AS Total
  FROM Jobs AS j
  FULL OUTER JOIN Orders AS o
    ON o.JobId = j.JobId
  FULL OUTER  JOIN OrderParts AS op
    ON op.OrderId = o.OrderId
  FULL OUTER JOIN Parts AS p
    ON p.PartId = op.PartId
 WHERE j.Status = 'Finished'
 GROUP BY j.JobId
 ORDER BY Total DESC, j.JobId ASC

	--> 14.
SELECT m.ModelId, 
	   m.Name, 
	   CAST(AVG(DATEDIFF(DAY, j.IssueDate, j.FinishDate))AS VARCHAR(10)) + ' ' + 'days' AS [Average Service Time]
  FROM Models AS m
 INNER JOIN Jobs AS j
    ON j.ModelId = m.ModelId
 GROUP BY m.ModelId, m.Name
 ORDER BY CAST(AVG(DATEDIFF(DAY, j.IssueDate, j.FinishDate))AS VARCHAR(10)) + ' ' + 'days'

	--> 15.
SELECT m.Name AS [Model],
       a.[Times Serviced],
	   ISNULL(SUM(op.Quantity * p.Price), 0) AS [Parts Total]
  FROM Jobs AS j
  FULL OUTER JOIN Orders AS o
    ON o.JobId = j.JobId
  FULL OUTER JOIN OrderParts AS op
    ON op.OrderId = o.OrderId
  FULL OUTER JOIN Parts AS p
    ON p.PartId = op.PartId
  FULL OUTER JOIN Models AS m
    ON m.ModelId = j.ModelId
  FULL OUTER JOIN
(
	SELECT TOP(1) WITH TIES
	       j.ModelId,
	       COUNT(j.ModelId) AS 'Times Serviced'
	  FROM Jobs AS j
	 GROUP BY j.ModelId
	 ORDER BY COUNT(j.ModelId) DESC
) AS a
    ON a.ModelId = j.ModelId
 WHERE j.ModelId IN (a.ModelId)
 GROUP BY m.Name,
       a.[Times Serviced]

	--> 16.
SELECT p.PartId,
	   p.[Description],
	   SUM(pn.Quantity) AS [Required],
	   AVG(p.StockQty) AS [In Stock],
	   ISNULL(SUM(op.Quantity),0) AS [Ordered]
  FROM Parts AS p
 INNER JOIN PartsNeeded AS pn
    ON pn.PartId = p.PartId
 INNER JOIN Jobs AS j
    ON j.JobId = pn.JobId
  LEFT JOIN Orders AS o
    ON o.JobId = j.JobId
  LEFT JOIN OrderParts AS op
    ON op.OrderId = o.OrderId
 WHERE j.Status <> 'Finished'
 GROUP BY p.PartId, p.Description
HAVING SUM(pn.Quantity) > AVG(p.StockQty) + ISNULL(SUM(op.Quantity),0)
 ORDER BY p.PartId

	--> Section 4. Programmability (15 pts)
	--> 17.
GO
CREATE FUNCTION udf_GetCost(@JobId INT)  
RETURNS DECIMAL(6, 2)   
AS
BEGIN
    DECLARE @totalCostOfParts MONEY;  

	SELECT @totalCostOfParts = SUM(op.Quantity * p.Price)
	  FROM Jobs AS j
	 INNER JOIN Orders AS o
	    ON o.JobId = j.JobId
	 INNER JOIN OrderParts AS op
	    ON op.OrderId = o.OrderId
	 INNER JOIN Parts AS p
	    ON p.PartId = op.PartId
	 WHERE j.JobId = @JobId

	IF (@totalCostOfParts IS NULL)   
		SET @totalCostOfParts = 0;  
    RETURN CAST(@totalCostOfParts as NUMERIC(10,2)); 

END
GO

SELECT dbo.udf_GetCost(1)

	--> 18.
GO
CREATE PROC usp_PlaceOrder(@JobId INT, @SerialNumber NVARCHAR(50), @Quantity INT)
AS
BEGIN

	IF(@Quantity <=0)
	BEGIN
		RAISERROR('Part quantity must be more than zero!', 16, 1)
		RETURN;
	END
	
	DECLARE @JobIdSelect INT = (SELECT JobId FROM Jobs WHERE JobId = @JobId)
	
	IF(@JobIdSelect IS NULL)
	BEGIN
		RAISERROR('Job not found!', 16, 1)
	END
	
	DECLARE @JobStatus VARCHAR(50) = (SELECT Status FROM Jobs WHERE JobId = @JobId)

	IF(@JobStatus = 'Finished')
	BEGIN
		RAISERROR('This job is not active!', 16, 1)
	END
	
	DECLARE @PartId INT = (SELECT PartId FROM Parts WHERE SerialNumber = @SerialNumber)

	IF(@PartId IS NULL)
	BEGIN
		RAISERROR('Part not found!', 16, 1)
		RETURN;
	END
	
	DECLARE @OrderId INT = (SELECT o.OrderId FROM Orders AS o
								JOIN OrderParts AS op ON op.OrderId = o.OrderId
								JOIN Parts AS p ON p.PartId = op.PartId
								WHERE JobId = @JobId AND p.PartId = @PartId AND IssueDate IS NULL)
		
	IF(@OrderId IS NULL)
	BEGIN
		INSERT INTO Orders(JobId, IssueDate) VALUES
		(@JobId, NULL)
	
		INSERT INTO OrderParts(OrderId, PartId, Quantity) VALUES
		(IDENT_CURRENT('Orders'), @PartId, @Quantity)
	END
	
	ELSE
	BEGIN
			DECLARE @PartExistanceOrder INT = (SELECT @@ROWCOUNT FROM OrderParts WHERE OrderId = @OrderId AND PartId = @PartId)
	
			IF(@PartExistanceOrder IS NULL)
			BEGIN
				INSERT INTO OrderParts(OrderId, PartId, Quantity) VALUES
				(@OrderId, @PartId, @Quantity)
			END
	
			ELSE
			BEGIN
				UPDATE OrderParts
				SET Quantity += @Quantity
				WHERE OrderId = @OrderId AND PartId = @PartId
			END
	END

END

	--> 19.
GO
CREATE TRIGGER tr_OrderDeliver ON Orders AFTER UPDATE
AS
BEGIN

	DECLARE @OldStatus INT = (SELECT Delivered from deleted)
	DECLARE @NewStatus INT = (SELECT Delivered from inserted)

	IF(@OldStatus = 0 AND @NewStatus = 1)
	BEGIN
		UPDATE Parts
		   SET StockQty += op.Quantity
		  FROM Parts AS p
		 INNER JOIN OrderParts AS op ON op.PartId = p.PartId
		 INNER JOIN Orders AS o ON o.OrderId = op.OrderId
		 INNER JOIN inserted AS i ON i.OrderId = o.OrderId
		 INNER JOIN deleted AS d ON d.OrderId = i.OrderId
		
	END
END

	--> 20.
WITH cte_JoinedGroupedTables
     AS (
     SELECT m.MechanicId,
            v.VendorId,
            SUM(op.Quantity) AS PartsForMechanicByVendor
     FROM Mechanics AS m
          JOIN jobs AS j ON j.MechanicId = m.MechanicId
          JOIN Orders AS o ON o.JobId = j.JobId
          JOIN OrderParts AS op ON op.OrderId = o.OrderId
          JOIN Parts AS p ON p.PartId = op.PartId
          JOIN Vendors AS v ON v.VendorId = p.VendorId
     GROUP BY m.MechanicId,
              v.VendorId)

     SELECT CONCAT(m.FirstName, ' ', m.LastName) AS Mechanic,
            v.Name AS Vendor,
            cte.PartsForMechanicByVendor AS Parts,
            CONCAT(FLOOR(cte.PartsForMechanicByVendor * 1.0 /
                        (
                            SELECT SUM(PartsForMechanicByVendor)
                            FROM cte_JoinedGroupedTables
                            WHERE MechanicId = m.MechanicId
                        ) * 100), '%') AS Preference
     FROM cte_JoinedGroupedTables AS cte
          JOIN Mechanics AS m ON m.MechanicId = cte.MechanicId
          JOIN Vendors AS v ON v.VendorId = cte.VendorId
     ORDER BY Mechanic,
              Parts DESC,
              Vendor;