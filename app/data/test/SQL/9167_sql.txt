-- Alkuperäisen tiedoston muuntaminen regexpeillä
-- Results taulua varten:
-- Etsi: ^(.*);(.*);(.*);(.*);(.*);(.*)$
-- Korvaa: \(\1,'\3'\,'\4'\,'\5'\,'\6'\),
-- Kilpailuvuositaulua varten:
-- Tyhjennetään rivit joilla ei ole kilpailun nimeä:
-- Etsi: ^.*;;.*$
-- Korvaa: tyhjä
-- Poistetaan tyhät rivit:
-- Etsi: \r\n\r\n\r\n
-- Korvaa: \r\n
-- Munneetaan insert-lauseeseen sopivaan muotoon
-- Etsi: ^(.*);(.*);(.*);(.*);(.*);(.*)$
-- Korvaa: \(\1,'\2'\),
-- Year;Event;Athlete;Medal;Country;Result

create database ski50b;
use ski50b;
grant select on ski50b.* to 'ski50b'@'localhost' identified by 'ski50b';

create table events (
    year integer primary key,
    event varchar(255)
);

insert into events values 
(1924,'50km Cross-country Men'),
(1928,'50km Cross-country Men'),
(1932,'50km Cross-country Men'),
(1936,'50km Cross-country Men'),
(1948,'50km Cross-country Men'),
(1952,'50km Cross-country Men'),
(1956,'50km Cross-country Men'),
(1960,'50km Cross-country Men'),
(1964,'50km Cross-country Men'),
(1968,'50km Cross-country Men'),
(1972,'50km Cross-country Men'),
(1976,'50km Cross-country Men'),
(1980,'50km Cross-country Men'),
(1984,'50km Cross-country Men'),
(1988,'50km Cross-country Men'),
(1992,'50km Cross-country Men (Freestyle)'),
(1994,'50km Cross-country Men (Classical)'),
(1998,'50km Cross-country Freestyle Men'),
(2002,'50km Cross-country Freestyle Men'),
(2006,'50km Cross-country Freestyle Men');

create table results(
    resultid integer primary key auto_increment,
    year integer,
    athlete varchar(255),
    medal varchar(255),
    country varchar(255),
    result time,
    foreign key (year) references events(year)
) character set 'utf8';

insert into results (year, athlete, medal, country, result)
values
(1924,'Thorleif Haug','GOLD','NOR','3:44:32'),
(1924,'Thoralf Strømstad','SILVER','NOR','3:46:23'),
(1924,'Johan Grøttumsbråten','BRONZE','NOR','3:47:46'),
(1928,'Per Erik Hedlund','GOLD','SWE','4:52:03'),
(1928,'Gustaf Jonsson','SILVER','SWE','5:05:30'),
(1928,'Volger Andersson','BRONZE','SWE','5:05:46'),
(1932,'Veli Saarinen','GOLD','FIN','4:28:00'),
(1932,'Väinö Liikkanen','SILVER','FIN','4:28:20'),
(1932,'Arne Rustadstuen','BRONZE','NOR','4:31:53'),
(1936,'Elis Wiklund','GOLD','SWE','3:30:11'),
(1936,'Axel Wikström','SILVER','SWE','3:33:20'),
(1936,'Nils-Joel Englund','BRONZE','SWE','3:34:10'),
(1948,'Nils Karlsson','GOLD','SWE','3:47:48'),
(1948,'Harald Eriksson','SILVER','SWE','3:52:20'),
(1948,'Benjamin Vanninen','BRONZE','FIN','3:57:28'),
(1952,'Veikko Hakulinen','GOLD','FIN','3:33:33'),
(1952,'Eero Kolehmainen','SILVER','FIN','3:38:11'),
(1952,'Magnar Estenstad','BRONZE','NOR','3:38:28'),
(1956,'Sixten Jernberg','GOLD','SWE','2:50:27'),
(1956,'Veikko Hakulinen','SILVER','FIN','2:51:45'),
(1956,'Fyodor Terentyev','BRONZE','URS','2:53:32'),
(1960,'Kalevi Hämäläinen','GOLD','FIN','2:59:06.3'),
(1960,'Veikko Hakulinen','SILVER','FIN','2:59:26.7'),
(1960,'Rolf Råmgård','BRONZE','SWE','3:02:46.7'),
(1964,'Sixten Jernberg','GOLD','SWE','2:43:52.6'),
(1964,'Assar Rönnlund','SILVER','SWE','2:44:58.2'),
(1964,'Arto Tiainen','BRONZE','FIN','2:45:30.4'),
(1968,'Olle Ellefsæter','GOLD','NOR','2:28:45.8'),
(1968,'Vyacheslav Vedenin','SILVER','URS','2:29:02.5'),
(1968,'Josef Haas','BRONZE','SUI','2:29:14.8'),
(1972,'Pål Tyldum','GOLD','NOR','2:43:14.75'),
(1972,'Magne Myrmo','SILVER','NOR','2:43:29.45'),
(1972,'Vyacheslav Vedenin','BRONZE','URS','2:44:00.19'),
(1976,'Ivar Formo','GOLD','NOR','2:37:30.05'),
(1976,'Gert-Dietmar Klause','SILVER','GDR','2:38:13.21'),
(1976,'Benny Södergren','BRONZE','SWE','2:39:39.21'),
(1980,'Nikolay Zimyatov','GOLD','URS','2:27:24.60'),
(1980,'Juha Mieto','SILVER','FIN','2:30:20.52'),
(1980,'Aleksandr Zavyalov','BRONZE','URS','2:30:51.52'),
(1984,'Thomas Wassberg','GOLD','SWE','2:15:55.8'),
(1984,'Gunde Svan','SILVER','SWE','2:16:00.7'),
(1984,'Aki Karvonen','BRONZE','FIN','2:17:04.7'),
(1988,'Gunde Svan','GOLD','SWE','2:04:30.9'),
(1988,'Maurilio de Zolt','SILVER','ITA','2:05:36.4'),
(1988,'Andi Grünenfelder','BRONZE','SUI','2:06:01.9'),
(1992,'Bjørn Dæhlie','GOLD','NOR','2:03:41.5'),
(1992,'Maurilio de Zolt','SILVER','ITA','2:04:39.1'),
(1992,'Giorgio Vanzetta','BRONZE','ITA','2:06:42.1'),
(1994,'Vladimir Smirnov','GOLD','KAZ','2:07:20.3'),
(1994,'Mika Myllylä','SILVER','FIN','2:08:41.9'),
(1994,'Sture Sivertsen','BRONZE','NOR','2:08:49.0'),
(1998,'Bjørn Dæhlie','GOLD','NOR','2:05:08.2'),
(1998,'Niklas Jonsson','SILVER','SWE','2:05:16.3'),
(1998,'Christian Hoffmann','BRONZE','AUT','2:06:01.8'),
(2002,'Mikhail Ivanov','GOLD','RUS','2:06:20.8'),
(2002,'Andrus Veerpalu','SILVER','EST','2:06:44.5'),
(2002,'Odd-Björn Hjelmeset','BRONZE','NOR','2:08:41.5'),
(2006,'Giorgio di Centa','GOLD','ITA','2:06:11.8'),
(2006,'Eugeni Dementiev','SILVER','RUS','2:06:12.6'),
(2006,'Mikhail Botvinov','BRONZE','AUT','2:06:12.7');

