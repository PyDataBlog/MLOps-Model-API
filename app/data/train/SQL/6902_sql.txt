SELECT m.firstName, m.LastName FROM Employees m
JOIN Employees e ON e.ManagerID = m.EmployeeID
GROUP BY m.FirstName, m.LastName
HAVING COUNT(e.ManagerID) = 5