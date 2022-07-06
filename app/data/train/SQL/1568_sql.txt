CREATE TABLE `clanovi` (
	`Clanski_broj` INT(11) NOT NULL,
	`Ime` VARCHAR(30) NULL DEFAULT NULL,
	`Prezime` VARCHAR(30) NULL DEFAULT NULL,
	`Adresa` VARCHAR(30) NULL DEFAULT NULL,
	`Telefon` CHAR(15) NULL DEFAULT NULL,
	`Datum_uclanjenja` DATETIME NULL DEFAULT NULL,
	PRIMARY KEY (`Clanski_broj`)
)
COLLATE='latin1_swedish_ci'
ENGINE=InnoDB
;
CREATE TABLE `zanr` (
	`Sifra_zanra` INT(11) NOT NULL,
	`Naziv` VARCHAR(30) NULL DEFAULT NULL,
	PRIMARY KEY (`Sifra_zanra`)
)
COLLATE='latin1_swedish_ci'
ENGINE=InnoDB
;
CREATE TABLE `filmovi` (
	`Sifra_filma` INT(11) NOT NULL,
	`Naziv` VARCHAR(30) NULL DEFAULT NULL,
	`Reziser` VARCHAR(30) NULL DEFAULT NULL,
	`Glavni_glumci` VARCHAR(100) NULL DEFAULT NULL,
	`Godina_izdanja` INT(11) NULL DEFAULT NULL,
	`Kolicina_dvd` INT(11) NULL DEFAULT NULL,
	`Kolicina_vhs` INT(11) NULL DEFAULT NULL,
	`Slika_naslovnice` BLOB NULL,
	`Sifra_zanra` INT(11) NULL DEFAULT NULL,
	PRIMARY KEY (`Sifra_filma`),
	INDEX `Sifra_zanra` (`Sifra_zanra`),
	CONSTRAINT `Filmovi_ibfk_1` FOREIGN KEY (`Sifra_zanra`) REFERENCES `zanr` (`Sifra_zanra`)
)
COLLATE='latin1_swedish_ci'
ENGINE=InnoDB
;
CREATE TABLE `cjenik` (
	`Sifra_cjenika` INT(11) NOT NULL,
	`Kategorija` VARCHAR(30) NULL DEFAULT NULL,
	`Cijena` FLOAT NULL DEFAULT NULL,
	PRIMARY KEY (`Sifra_cjenika`)
)
COLLATE='latin1_swedish_ci'
ENGINE=InnoDB
;
CREATE TABLE `posudba` (
	`Clanski_broj` INT(11) NOT NULL,
	`Sifra_filma` INT(11) NOT NULL,
	`Datum_posudbe` DATETIME NOT NULL,
	`Datum_povratka` DATETIME NULL DEFAULT NULL,
	`Sifra_cjenika` INT(11) NOT NULL,
	PRIMARY KEY (`Clanski_broj`, `Sifra_filma`, `Datum_posudbe`),
	INDEX `Clanski_broj` (`Clanski_broj`),
	INDEX `Sifra_filma` (`Sifra_filma`),
	INDEX `Sifra_cjenika` (`Sifra_cjenika`),
	CONSTRAINT `Posudba_ibfk_1` FOREIGN KEY (`Clanski_broj`) REFERENCES `clanovi` (`Clanski_broj`),
	CONSTRAINT `Posudba_ibfk_2` FOREIGN KEY (`Sifra_filma`) REFERENCES `filmovi` (`Sifra_filma`),
	CONSTRAINT `Posudba_ibfk_3` FOREIGN KEY (`Sifra_cjenika`) REFERENCES `cjenik` (`Sifra_cjenika`)
)
COLLATE='latin1_swedish_ci'
ENGINE=InnoDB
;
