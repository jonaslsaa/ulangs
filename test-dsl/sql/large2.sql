-- SQL File: library_management.sql

-- Create tables
CREATE TABLE Authors (
    id INT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(100) NOT NULL,
    birth_date DATE
);

CREATE TABLE Books (
    id INT PRIMARY KEY AUTO_INCREMENT,
    title VARCHAR(200) NOT NULL,
    author_id INT,
    published_date DATE,
    genre VARCHAR(50),
    FOREIGN KEY (author_id) REFERENCES Authors(id)
);

CREATE TABLE Members (
    id INT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(100) NOT NULL,
    membership_date DATE
);

CREATE TABLE BorrowedBooks (
    id INT PRIMARY KEY AUTO_INCREMENT,
    book_id INT,
    member_id INT,
    borrow_date DATE,
    return_date DATE,
    FOREIGN KEY (book_id) REFERENCES Books(id),
    FOREIGN KEY (member_id) REFERENCES Members(id)
);

-- Insert data into Authors
INSERT INTO Authors (name, birth_date) VALUES 
('George Orwell', '1903-06-25'),
('Jane Austen', '1775-12-16'),
('Mark Twain', '1835-11-30');

-- Insert data into Books
INSERT INTO Books (title, author_id, published_date, genre) VALUES 
('1984', 1, '1949-06-08', 'Dystopian'),
('Pride and Prejudice', 2, '1813-01-28', 'Romance'),
('Adventures of Huckleberry Finn', 3, '1884-02-18', 'Adventure');

-- Insert data into Members
INSERT INTO Members (name, membership_date) VALUES 
('Alice Williams', '2023-01-15'),
('Bob Johnson', '2022-07-22'),
('Charlie Davis', '2021-11-05');

-- Borrow a book
INSERT INTO BorrowedBooks (book_id, member_id, borrow_date) VALUES 
(1, 1, '2024-01-10'),
(2, 2, '2024-01-15');

-- Query to list all books and their authors
SELECT Books.title, Authors.name AS author_name 
FROM Books 
JOIN Authors ON Books.author_id = Authors.id;

-- Query to find all borrowed books and the members who borrowed them
SELECT Books.title, Members.name AS member_name, BorrowedBooks.borrow_date 
FROM BorrowedBooks 
JOIN Books ON BorrowedBooks.book_id = Books.id 
JOIN Members ON BorrowedBooks.member_id = Members.id;

-- Return a book
UPDATE BorrowedBooks SET return_date = '2024-01-20' WHERE book_id = 1 AND member_id = 1;

-- Query to find members who borrowed books
SELECT DISTINCT Members.name 
FROM Members 
JOIN BorrowedBooks ON Members.id = BorrowedBooks.member_id;
