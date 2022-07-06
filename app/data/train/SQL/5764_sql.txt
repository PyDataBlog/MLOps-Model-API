INSERT INTO dvd (title, production_year) 
    VALUES ('Terminator 1', '1984'), 
           ('Terminator 2', '1994'), 
           ('Terminator 3', '2004');
INSERT INTO customer (first_name, last_name, passport_code, registration_date) 
    VALUES ('Nick', 'Brain', '88 05 678956', '2014-09-01'), 
           ('Donald', 'Duck', '01 ES 938471', '2014-09-15'), 
           ('Carl', 'Lagerfeld', '8892 A 123451', '2014-10-01');
INSERT INTO offer (dvd_id, customer_id, offer_date, return_date) 
    VALUES ('1', '1', '2014-11-01', '2014-11-10'), 
           ('2', '2', '2014-11-02', '2014-11-04'), 
           ('3', '3', '2014-11-03', '2014-11-08');