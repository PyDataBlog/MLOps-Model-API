/*DROP TABLE IF EXISTS `#__peetcontact`;

CREATE TABLE `#__peetcontact`
(
`id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
`name` VARCHAR(255) NOT NULL,
`date` DATETIME NOT NULL,
`question` TEXT NOT NULL,
`city` VARCHAR(50) NULL,
`email` VARCHAR(50) NOT NULL,
`IP` VARCHAR(15) NOT NULL,
`id_cat` INT NOT NULL,
`published` TINYINT(1) NULL DEFAULT '1',
`expiration_date` DATETIME NULL DEFAULT '0000-00-00 00:00:00',
`senttoexpert` TINYINT(1) NULL DEFAULT '0',
`answer` TEXT NULL DEFAULT '',
`senttoauthor` TINYINT(1) NULL DEFAULT '0'
);

INSERT INTO `joo_my_comp`(`id`, `name`, `date`, `question`, `city`, `email`, `IP`, `id_cat`)
VALUES(NULL, 'Аноним', '2012-01-01 09:00:00', 'Есть ли жизнь на Марсе', 'Москва', 'somebody@mail.ru', '12.345.67.890', '1'); */