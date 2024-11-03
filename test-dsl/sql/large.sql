-- SQL File: employee_management.sql

-- Create tables
CREATE TABLE Departments (
    id INT PRIMARY KEY AUTO_INCREMENT,
    department_name VARCHAR(50) NOT NULL,
    location VARCHAR(50)
);

CREATE TABLE Employees (
    id INT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(100) NOT NULL,
    position VARCHAR(50) NOT NULL,
    department_id INT,
    hire_date DATE,
    salary DECIMAL(10, 2),
    FOREIGN KEY (department_id) REFERENCES Departments(id)
);

-- Insert data into Departments
INSERT INTO Departments (department_name, location) VALUES 
('Sales', 'New York'),
('Engineering', 'San Francisco'),
('HR', 'Los Angeles'),
('Marketing', 'Chicago');

-- Insert data into Employees
INSERT INTO Employees (name, position, department_id, hire_date, salary) VALUES 
('Alice Johnson', 'Sales Representative', 1, '2022-03-01', 55000.00),
('Bob Smith', 'Software Engineer', 2, '2021-06-15', 80000.00),
('Charlie Brown', 'HR Manager', 3, '2020-08-20', 75000.00),
('Diana Prince', 'Marketing Specialist', 4, '2019-02-10', 60000.00),
('Eve Adams', 'Sales Manager', 1, '2021-05-30', 72000.00);

-- Query to retrieve all employees with their department names
SELECT Employees.name, Employees.position, Departments.department_name 
FROM Employees 
JOIN Departments ON Employees.department_id = Departments.id;

-- Update salary of an employee
UPDATE Employees SET salary = 58000.00 WHERE name = 'Alice Johnson';

-- Delete an employee record
DELETE FROM Employees WHERE name = 'Diana Prince';

-- Query to find average salary by department
SELECT Departments.department_name, AVG(Employees.salary) AS average_salary
FROM Employees
JOIN Departments ON Employees.department_id = Departments.id
GROUP BY Departments.department_name;

-- Query to list all employees hired after January 1, 2020
SELECT * FROM Employees WHERE hire_date > '2020-01-01';
