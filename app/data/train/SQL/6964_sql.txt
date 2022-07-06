SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

DROP SCHEMA IF EXISTS `ygc` ;
CREATE SCHEMA IF NOT EXISTS `ygc` DEFAULT CHARACTER SET utf8 COLLATE utf8_hungarian_ci ;
USE `ygc` ;

-- -----------------------------------------------------
-- Table `ygc`.`team`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `ygc`.`team` ;

CREATE  TABLE IF NOT EXISTS `ygc`.`team` (
  `idteam` INT NOT NULL ,
  `username` VARCHAR(45) NOT NULL ,
  `password` VARCHAR(45) NOT NULL ,
  `team_name` VARCHAR(45) NOT NULL ,
  `logo` VARCHAR(500) NULL ,
  PRIMARY KEY (`idteam`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ygc`.`user`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `ygc`.`user` ;

CREATE  TABLE IF NOT EXISTS `ygc`.`user` (
  `iduser` INT NOT NULL ,
  `username` VARCHAR(45) NOT NULL ,
  `password` VARCHAR(45) NOT NULL ,
  `user_type` INT NOT NULL ,
  PRIMARY KEY (`iduser`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ygc`.`candidate`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `ygc`.`candidate` ;

CREATE  TABLE IF NOT EXISTS `ygc`.`candidate` (
  `idcandidate` INT NOT NULL ,
  `candidate_name` VARCHAR(45) NOT NULL ,
  `candidate_email` VARCHAR(45) NOT NULL ,
  `candidate_contactno` VARCHAR(45) NOT NULL ,
  `candidate_institute` VARCHAR(45) NOT NULL ,
  `candidate_major` VARCHAR(45) NOT NULL ,
  `team_idteam` INT NOT NULL ,
  `user_iduser` INT NOT NULL ,
  PRIMARY KEY (`idcandidate`) ,
  INDEX `fk_candidate_team1_idx` (`team_idteam` ASC) ,
  INDEX `fk_candidate_user1_idx` (`user_iduser` ASC) ,
  CONSTRAINT `fk_candidate_team1`
    FOREIGN KEY (`team_idteam` )
    REFERENCES `ygc`.`team` (`idteam` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_candidate_user1`
    FOREIGN KEY (`user_iduser` )
    REFERENCES `ygc`.`user` (`iduser` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ygc`.`mentor`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `ygc`.`mentor` ;

CREATE  TABLE IF NOT EXISTS `ygc`.`mentor` (
  `idmentor` INT NOT NULL ,
  `mentor_name` VARCHAR(45) NOT NULL ,
  `mentor_email` VARCHAR(45) NOT NULL ,
  `mentor_contactno` VARCHAR(45) NOT NULL ,
  `mentor_designation` VARCHAR(45) NOT NULL ,
  `mentor_company` VARCHAR(45) NOT NULL ,
  `mentor_photo` VARCHAR(45) NOT NULL ,
  `user_iduser` INT NOT NULL ,
  PRIMARY KEY (`idmentor`) ,
  INDEX `fk_mentor_user1_idx` (`user_iduser` ASC) ,
  CONSTRAINT `fk_mentor_user1`
    FOREIGN KEY (`user_iduser` )
    REFERENCES `ygc`.`user` (`iduser` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ygc`.`admin`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `ygc`.`admin` ;

CREATE  TABLE IF NOT EXISTS `ygc`.`admin` (
  `idadmin` INT NOT NULL ,
  `admin_name` VARCHAR(45) NOT NULL ,
  `admin_email` VARCHAR(45) NOT NULL ,
  `admin_contactno` VARCHAR(45) NOT NULL ,
  `user_iduser` INT NOT NULL ,
  PRIMARY KEY (`idadmin`) ,
  INDEX `fk_admin_user1_idx` (`user_iduser` ASC) ,
  CONSTRAINT `fk_admin_user1`
    FOREIGN KEY (`user_iduser` )
    REFERENCES `ygc`.`user` (`iduser` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ygc`.`team_has_mentor`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `ygc`.`team_has_mentor` ;

CREATE  TABLE IF NOT EXISTS `ygc`.`team_has_mentor` (
  `team_idteam` INT NOT NULL ,
  `mentor_idmentor` INT NOT NULL ,
  PRIMARY KEY (`team_idteam`, `mentor_idmentor`) ,
  INDEX `fk_team_has_mentor_mentor1_idx` (`mentor_idmentor` ASC) ,
  INDEX `fk_team_has_mentor_team_idx` (`team_idteam` ASC) ,
  CONSTRAINT `fk_team_has_mentor_team`
    FOREIGN KEY (`team_idteam` )
    REFERENCES `ygc`.`team` (`idteam` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_team_has_mentor_mentor1`
    FOREIGN KEY (`mentor_idmentor` )
    REFERENCES `ygc`.`mentor` (`idmentor` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ygc`.`document`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `ygc`.`document` ;

CREATE  TABLE IF NOT EXISTS `ygc`.`document` (
  `iddocument` INT NOT NULL ,
  `document_type` INT NOT NULL ,
  `path` VARCHAR(500) NOT NULL ,
  `date` DATETIME NOT NULL ,
  `team_idteam` INT NOT NULL ,
  `status` INT NOT NULL ,
  PRIMARY KEY (`iddocument`) ,
  INDEX `fk_document_team1_idx` (`team_idteam` ASC) ,
  CONSTRAINT `fk_document_team1`
    FOREIGN KEY (`team_idteam` )
    REFERENCES `ygc`.`team` (`idteam` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ygc`.`post`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `ygc`.`post` ;

CREATE  TABLE IF NOT EXISTS `ygc`.`post` (
  `idpost` INT NOT NULL ,
  `text` TEXT NOT NULL ,
  `time` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP ,
  `image` VARCHAR(500) NOT NULL ,
  `user_iduser` INT NOT NULL ,
  `document_iddocument` INT NULL ,
  `team_idteam` INT NOT NULL ,
  PRIMARY KEY (`idpost`) ,
  INDEX `fk_post_user1_idx` (`user_iduser` ASC) ,
  INDEX `fk_post_document1_idx` (`document_iddocument` ASC) ,
  INDEX `fk_post_team1_idx` (`team_idteam` ASC) ,
  CONSTRAINT `fk_post_user1`
    FOREIGN KEY (`user_iduser` )
    REFERENCES `ygc`.`user` (`iduser` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_post_document1`
    FOREIGN KEY (`document_iddocument` )
    REFERENCES `ygc`.`document` (`iddocument` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_post_team1`
    FOREIGN KEY (`team_idteam` )
    REFERENCES `ygc`.`team` (`idteam` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ygc`.`comment`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `ygc`.`comment` ;

CREATE  TABLE IF NOT EXISTS `ygc`.`comment` (
  `idcomment` INT NOT NULL ,
  `text` TEXT NOT NULL ,
  `time` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP ,
  `photo` VARCHAR(500) NULL ,
  `post_idpost` INT NOT NULL ,
  `user_iduser` INT NOT NULL ,
  PRIMARY KEY (`idcomment`) ,
  INDEX `fk_comment_post1_idx` (`post_idpost` ASC) ,
  INDEX `fk_comment_user1_idx` (`user_iduser` ASC) ,
  CONSTRAINT `fk_comment_post1`
    FOREIGN KEY (`post_idpost` )
    REFERENCES `ygc`.`post` (`idpost` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_comment_user1`
    FOREIGN KEY (`user_iduser` )
    REFERENCES `ygc`.`user` (`iduser` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ygc`.`judge`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `ygc`.`judge` ;

CREATE  TABLE IF NOT EXISTS `ygc`.`judge` (
  `idjudge` INT NOT NULL ,
  `name` VARCHAR(45) NULL ,
  `judge_email` VARCHAR(45) NULL ,
  `judge_contactno` VARCHAR(45) NULL ,
  `photo` VARCHAR(500) NULL ,
  `user_iduser` INT NOT NULL ,
  PRIMARY KEY (`idjudge`) ,
  INDEX `fk_judge_user1_idx` (`user_iduser` ASC) ,
  CONSTRAINT `fk_judge_user1`
    FOREIGN KEY (`user_iduser` )
    REFERENCES `ygc`.`user` (`iduser` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;

USE `ygc` ;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
