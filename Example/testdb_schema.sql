
-- This file is public domain.

CREATE TABLE stores (
    store_id      INTEGER NOT NULL,
    street        VARCHAR(50) NOT NULL,
    city          VARCHAR(50) NOT NULL,
    state         CHAR(2) NOT NULL,
    zip           VARCHAR(10) NOT NULL,   
    open_time     VARCHAR(8),
    close_time    VARCHAR(8),
    PRIMARY KEY (store_id)
);

CREATE TABLE employees (
    employee_id    INTEGER NOT NULL,
    last_name      VARCHAR(50) NOT NULL,
    first_name     VARCHAR(50) NOT NULL,
    ssn            CHAR(9),
    phone          VARCHAR(12),
    birth_date     DATE,
    manager_id     INTEGER NOT NULL,
    store_id       INTEGER NOT NULL,
    PRIMARY KEY (employee_id)
);


CREATE TABLE accounts (
    account_id    INTEGER NOT NULL,
    street        VARCHAR(50) NOT NULL,
    city          VARCHAR(50) NOT NULL,
    state         CHAR(2) NOT NULL,
    zip           VARCHAR(10) NOT NULL,   
    balance       DECIMAL(7,2),
    PRIMARY KEY (account_id)
);


CREATE TABLE customers (
    customer_id    INTEGER NOT NULL,
    last_name      VARCHAR(50) NOT NULL,
    first_name     VARCHAR(50) NOT NULL,
    phone          VARCHAR(12),
    birth_date     DATE,
    account_id     INTEGER NOT NULL,
    spendingLimit          DECIMAL(7,2),
    PRIMARY KEY (customer_id)
);


CREATE TABLE discs (
    disc_id              CHAR(8) NOT NULL,
    movie_id             INTEGER NOT NULL,
    rental_duration_days INTEGER,
   PRIMARY KEY (disc_id)   
);

CREATE TABLE movies (
    movie_id        INTEGER NOT NULL,
    title           VARCHAR(80) NOT NULL,
    category_id     INTEGER NOT NULL,                 
    movie_duration  VARCHAR(10),
    rating          VARCHAR(10),
    release_date    DATE,
   PRIMARY KEY (movie_id)   
);

CREATE TABLE categories (
    category_id  INTEGER NOT NULL,
    name         VARCHAR(80) NOT NULL,
   PRIMARY KEY (category_id)   
);

CREATE TABLE rentals (
    rental_id    INTEGER NOT NULL,
    disc_id      CHAR(8) NOT NULL,
    rental_date  DATE NOT NULL,
    return_date  DATE NOT NULL,
    customer_id  INTEGER NOT NULL,
    employee_id  INTEGER NOT NULL,
    store_id     INTEGER NOT NULL,
   PRIMARY KEY (rental_id),
   UNIQUE (disc_id, rental_date)       
);

ALTER TABLE movies    ADD FOREIGN KEY (category_id) REFERENCES categories (category_id) ON DELETE CASCADE;
ALTER TABLE discs     ADD FOREIGN KEY (movie_id)    REFERENCES movies (movie_id)        ON DELETE CASCADE;
ALTER TABLE rentals   ADD FOREIGN KEY (disc_id)     REFERENCES discs (disc_id)          ON DELETE CASCADE;
ALTER TABLE rentals   ADD FOREIGN KEY (customer_id) REFERENCES customers (customer_id)  ON DELETE CASCADE;
ALTER TABLE rentals   ADD FOREIGN KEY (employee_id) REFERENCES employees (employee_id)  ON DELETE CASCADE;
ALTER TABLE rentals   ADD FOREIGN KEY (store_id)    REFERENCES stores (store_id)        ON DELETE CASCADE;
ALTER TABLE employees ADD FOREIGN KEY (manager_id)  REFERENCES employees (employee_id)  ON DELETE CASCADE;
ALTER TABLE employees ADD FOREIGN KEY (store_id)    REFERENCES stores (store_id)        ON DELETE CASCADE;
ALTER TABLE customers ADD FOREIGN KEY (account_id)  REFERENCES accounts (account_id)    ON DELETE CASCADE;

INSERT INTO stores VALUES (1, '9112 North Rodney Parham Road # 121', 'Little Rock', 'AR', '72205-5585', '11:00 AM', '09:00 PM');
INSERT INTO stores VALUES (2, '8521 Geyer Springs Road',             'Little Rock', 'AR', '72209',      '11:00 AM', '09:00 PM');
INSERT INTO stores VALUES (3, '7403 Cantrell Road',                  'Little Rock', 'AR', '72207',      '11:00 AM', '11:00 PM');
INSERT INTO stores VALUES (4, '12111 West Markham Street Suite 100', 'Little Rock', 'AR', '72211',      '11:00 AM', '02:00 AM');
INSERT INTO stores VALUES (5, '1523 West Umbridge Street #3',        'North Little Rock', 'AR', '72115',      '2:00 PM', '01:00 AM');

INSERT INTO accounts VALUES (1, '3612 West Marble Drive', 'Little Rock', 'AR', '72205', 9.00);
INSERT INTO accounts VALUES (2, '3106 Eastway Dr',        'Little Rock', 'AR', '72205', 23.00);
INSERT INTO accounts VALUES (3, '1730 Abbey Pl # 1',      'Little Rock', 'AR', '72116', -14.00);
INSERT INTO accounts VALUES (4, '130 Providence Rd',      'Little Rock', 'AR', '72211', -3.45);

INSERT INTO customers VALUES (3, 'Panky',      'Henry',    '555-1221', '1968-01-21', 4, 0.00);
INSERT INTO customers VALUES (1, 'Jones',      'Henry',    '555-1212', '1970-10-10', 3, 0.00);
INSERT INTO customers VALUES (4, 'Wonderland', 'Alice N.', '555-1122', '1969-03-05', 2, 3.00);
INSERT INTO customers VALUES (2, 'Rubin',      'William',  '555-2211', '1972-07-10', 3, 15.00);

INSERT INTO employees VALUES (1, 'Gibbons',       'Peter',    '431331234', '555-1221', '1968-01-21', 1, 1);
INSERT INTO employees VALUES (2, 'Bolton',        'Michael',  '431351111', '555-1221', '1967-12-21', 2, 2);
INSERT INTO employees VALUES (3, 'Hagheenanajar', 'Samir',    '431351222', '555-1221', '1969-11-05', 3, 3);
INSERT INTO employees VALUES (4, 'Lumbergh',      'Bill',     '431351111', '555-1221', '1961-10-14', 4, 4);
INSERT INTO employees VALUES (5, 'Smykowski',     'Tom',      '431331313', '555-1221', '1953-08-28', 1, 1);
INSERT INTO employees VALUES (6, 'Porter',        'Bob',      '431333211', '555-1221', '1948-07-03', 2, 2);
INSERT INTO employees VALUES (7, 'WentWorth',     'Anne',     '431331212', '555-1221', '1966-02-12', 3, 3);

INSERT INTO categories VALUES (1, 'Drama');
INSERT INTO categories VALUES (2, 'Crime');
INSERT INTO categories VALUES (3, 'Thriller');
INSERT INTO categories VALUES (4, 'Romance');
INSERT INTO categories VALUES (5, 'Mystery');
INSERT INTO categories VALUES (6, 'Action');


INSERT INTO movies VALUES (1, 'The Godfather', 2, '175 min', 'USA:R',  '1972-03-24');
INSERT INTO movies VALUES (2, 'Casablanca',    4, '102 min', 'USA:PG', '1943-01-23');
INSERT INTO movies VALUES (3, 'Citizen Kane',  5, '119 min', 'USA:PG', '1941-03-01');
INSERT INTO movies VALUES (4, 'Rear Window',   5, '112 min', 'USA:PG', '1955-01-14');

INSERT INTO discs VALUES ('AB-12345', 1, 2);
INSERT INTO discs VALUES ('AB-67472', 1, 5);
INSERT INTO discs VALUES ('MC-68873', 2, 2);
INSERT INTO discs VALUES ('OW-41221', 3, 5);
INSERT INTO discs VALUES ('AH-54706', 4, 5);

INSERT INTO rentals VALUES (1, 'AB-12345', '2001-11-25', '2001-11-30', 1, 1, 1);
INSERT INTO rentals VALUES (2, 'AB-67472', '2001-11-25', '2001-11-30', 3, 2, 2);
INSERT INTO rentals VALUES (3, 'OW-41221', '2001-11-25', '2001-11-30', 1, 3, 3);
INSERT INTO rentals VALUES (4, 'MC-68873', '2001-11-20', '2001-11-25', 3, 4, 4);
INSERT INTO rentals VALUES (5, 'MC-68873', '2001-10-1',  '2001-10-6',  2, 5, 1);
INSERT INTO rentals VALUES (6, 'OW-41221', '2002-1-3',   '2002-1-6',   4, 6, 2);

