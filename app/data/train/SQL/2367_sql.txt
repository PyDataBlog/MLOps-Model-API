CREATE DATABASE `cmsv2`;

USE `cmsv2`;

CREATE TABLE `user_role` (
	`role_id` INT, /* Indexing 0==Admin, 1==Instructor, 3==Student */
	`role` VARCHAR(50) NOT NULL,
	PRIMARY KEY (`role_id`)
);

CREATE TABLE `user` (
	`user_id` INT(7) NOT NULL UNIQUE,
	`role_id` INT NOT NULL,
	`password` VARCHAR(50) NOT NULL,
	`status` INT DEFAULT 1,
	`fname` VARCHAR(50),
	`mname` VARCHAR(50),
	`lname` VARCHAR(50),
	`birthdate` DATE,
	`contact_no` VARCHAR(11),
	PRIMARY KEY (`user_id`),
	FOREIGN KEY (role_id) 
    REFERENCES user_role(role_id)
);

CREATE TABLE `course` (
	`course_id` VARCHAR(5),
	`course_name` VARCHAR(60),
	PRIMARY KEY (`course_id`)
);

CREATE TABLE `student` (
	`student_id` INT(7) NOT NULL UNIQUE,
	`course_id` VARCHAR(5),
	`year_level` INT,
	PRIMARY KEY (`student_id`),
	FOREIGN KEY (student_id) REFERENCES user (user_id) ON DELETE CASCADE,
	FOREIGN KEY (course_id) 
    REFERENCES course(course_id)
);

CREATE TABLE `instructor` (
	`instructor_id` INT(7) NOT NULL UNIQUE,
	`position` VARCHAR(50),
	PRIMARY KEY (`instructor_id`),
	FOREIGN KEY (instructor_id) 
    REFERENCES user(user_id) ON DELETE CASCADE

);

CREATE TABLE `room_type` (
	`room_type_id` INT(1), /* 1==Lecture, 2==Lab */
	`room_type` VARCHAR(50),
	PRIMARY KEY (`room_type_id`)
);

CREATE TABLE `room` (
	`room_id` VARCHAR(6),
	`room_type` INT(1),
	PRIMARY KEY (`room_id`),
	FOREIGN KEY (room_type) 
    REFERENCES room_type(room_type_id)

);

CREATE TABLE `subject` (
	`subject_code` VARCHAR(7), /* ex. ICT141, ICT201n */
	`subject_title` VARCHAR(50) NOT NULL,
	PRIMARY KEY (`subject_code`)
);


CREATE TABLE `academic_year` (
	`academic_year_id` INT AUTO_INCREMENT,
	`academic_year` VARCHAR(9),
	PRIMARY KEY (`academic_year_id`)
);

CREATE TABLE `semester` (
	`semester_id` INT AUTO_INCREMENT,
	`semester` VARCHAR(50),
	`academic_year_id` INT,
	`start_date` DATE,
	`end_date` DATE,
	PRIMARY KEY (`semester_id`),
	FOREIGN KEY (academic_year_id) 
    REFERENCES academic_year(academic_year_id)
);

CREATE TABLE `seat_plan` (
	`seat_id` INT,
	`row` INT(2) NOT NULL,
	`column` INT(2) NOT NULL,
	PRIMARY KEY (`seat_id`)

);

CREATE TABLE `class` (
	`class_id` INT AUTO_INCREMENT,
	`semester_id` INT,
	`subject_code` VARCHAR(7),
	`room_id` VARCHAR(6),
	`instructor_id` INT(7),
	`start_time` TIME,
	`end_time` TIME,
	PRIMARY KEY (`class_id`),
	FOREIGN KEY (semester_id) 
    REFERENCES semester(semester_id),
    FOREIGN KEY (subject_code) 
    REFERENCES subject(subject_code),
    FOREIGN KEY (room_id) 
    REFERENCES room(room_id),
    FOREIGN KEY (instructor_id) 
    REFERENCES instructor(instructor_id)
);

CREATE TABLE `class_member` (
	`student_id` INT(1),
	`class_id` INT NOT NULL,
	`status` VARCHAR(15) NULL,
	`seat_id` INT,
	PRIMARY KEY (`student_id`),
	FOREIGN KEY (student_id) 
    REFERENCES student(student_id) ON DELETE CASCADE,
	FOREIGN KEY (class_id) 
    REFERENCES class(class_id),
    FOREIGN KEY (seat_id) 
    REFERENCES seat_plan(seat_id)
);

CREATE TABLE `attendance_status` (
	`attendance_status` INT(1),
	`definition` VARCHAR(50),
	PRIMARY KEY (`attendance_status`)

);

CREATE TABLE `attendance` (
	`attendance_id` INT AUTO_INCREMENT,
	`class_id` INT NOT NULL,
	`student_id` INT,
	`attendance_datetime` DATETIME,
	`attendance_status` INT,
	PRIMARY KEY (`attendance_id`),
	FOREIGN KEY (attendance_status) 
    REFERENCES attendance_status(attendance_status),
    FOREIGN KEY (class_id) 
    REFERENCES class_member(class_id),
    FOREIGN KEY (student_id) 
    REFERENCES class_member(student_id)
);

CREATE TABLE `class_calendar` (
	`class_calendar_id` INT AUTO_INCREMENT,
	`class_id` INT NOT NULL,
	`event_date` DATETIME,
	`event` TEXT,
	PRIMARY KEY (`class_calendar_id`),
	FOREIGN KEY (class_id) 
    REFERENCES class(class_id)

);

CREATE TABLE `consultation` (
	`consultation_id` INT AUTO_INCREMENT,
	`instructor_id` INT NOT NULL,
	`class_id` INT NOT NULL,
	`datetime_scheduled` DATETIME,
	`time_consumed` FLOAT(4,2),
	`issue_topic` TEXT,
	`remarks` TEXT,
	PRIMARY KEY (`consultation_id`),
	FOREIGN KEY (instructor_id) 
    REFERENCES instructor(instructor_id),
    FOREIGN KEY (class_id) 
    REFERENCES class(class_id)

);

CREATE TABLE `consultee` (
	`consultation_id` INT,
	`student_id` INT NOT NULL,
	PRIMARY KEY (`consultation_id`),
    FOREIGN KEY (consultation_id) 
    REFERENCES consultation(consultation_id),
    FOREIGN KEY (student_id) 
    REFERENCES class_member(student_id)
	
);