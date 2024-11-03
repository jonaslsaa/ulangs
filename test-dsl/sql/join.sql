SELECT Employees.name, Departments.department_name
FROM Employees
JOIN Departments ON Employees.department_id = Departments.id;
