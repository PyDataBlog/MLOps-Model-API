SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL';

CREATE SCHEMA IF NOT EXISTS `libreria` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci ;
USE `libreria` ;

-- -----------------------------------------------------
-- Table `libreria`.`libros`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `libreria`.`libros` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `autor` VARCHAR(100) NOT NULL ,
  `titulo` VARCHAR(100) NULL ,
  `detalles` VARCHAR(256) NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;

