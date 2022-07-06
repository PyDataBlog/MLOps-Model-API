-- phpMyAdmin SQL Dump
-- version 4.7.0
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1
-- Generation Time: Jan 12, 2018 at 08:37 AM
-- Server version: 10.1.25-MariaDB
-- PHP Version: 5.6.31

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET AUTOCOMMIT = 0;
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `whitespacedb`
--

-- --------------------------------------------------------

--
-- Table structure for table `class_tbl`
--

CREATE TABLE `class_tbl` (
  `class_id` int(10) UNSIGNED NOT NULL,
  `course` varchar(10) COLLATE utf8mb4_unicode_ci NOT NULL,
  `section` varchar(5) COLLATE utf8mb4_unicode_ci NOT NULL,
  `school_year` varchar(10) COLLATE utf8mb4_unicode_ci NOT NULL,
  `prof_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Dumping data for table `class_tbl`
--

INSERT INTO `class_tbl` (`class_id`, `course`, `section`, `school_year`, `prof_id`) VALUES
(2, 'BSIT', '3A', '2017-2018', 1);

-- --------------------------------------------------------

--
-- Table structure for table `columns_tbl`
--

CREATE TABLE `columns_tbl` (
  `column_id` int(10) UNSIGNED NOT NULL,
  `column_name` varchar(200) COLLATE utf8mb4_unicode_ci NOT NULL,
  `column_type` varchar(50) COLLATE utf8mb4_unicode_ci NOT NULL,
  `column_length` int(11) NOT NULL,
  `column_default` varchar(50) COLLATE utf8mb4_unicode_ci NOT NULL,
  `column_attributes` varchar(50) COLLATE utf8mb4_unicode_ci NOT NULL,
  `column_null` enum('true','false') COLLATE utf8mb4_unicode_ci NOT NULL,
  `column_ai` enum('true','false') COLLATE utf8mb4_unicode_ci NOT NULL,
  `table_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- --------------------------------------------------------

--
-- Table structure for table `database_tbl`
--

CREATE TABLE `database_tbl` (
  `database_id` int(10) UNSIGNED NOT NULL,
  `database_name` varchar(200) COLLATE utf8mb4_unicode_ci NOT NULL,
  `user_acct_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- --------------------------------------------------------

--
-- Table structure for table `entries_tbl`
--

CREATE TABLE `entries_tbl` (
  `entry_id` int(10) UNSIGNED NOT NULL,
  `entry_value` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
  `created_at` timestamp NULL DEFAULT NULL,
  `updated_at` timestamp NULL DEFAULT NULL,
  `column_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- --------------------------------------------------------

--
-- Table structure for table `files_tbl`
--

CREATE TABLE `files_tbl` (
  `file_id` int(10) UNSIGNED NOT NULL,
  `file_name` varchar(300) COLLATE utf8mb4_unicode_ci NOT NULL,
  `uploaded_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `prof_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Dumping data for table `files_tbl`
--

INSERT INTO `files_tbl` (`file_id`, `file_name`, `uploaded_at`, `prof_id`) VALUES
(1, 'Whitespace-10-12-17.docx', '2017-12-01 19:45:01', 1),
(2, 'BSIT-3A-2017-2018.xlsx', '2017-12-04 22:17:53', 1),
(3, 'IMG_20161216_202753_194.jpg', '2017-12-12 19:08:26', 1);

-- --------------------------------------------------------

--
-- Table structure for table `foreign_tbl`
--

CREATE TABLE `foreign_tbl` (
  `foreign_id` int(10) UNSIGNED NOT NULL,
  `foreign_col_id` int(10) UNSIGNED DEFAULT NULL,
  `reference_pri_col_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- --------------------------------------------------------

--
-- Table structure for table `history_tbl`
--

CREATE TABLE `history_tbl` (
  `history_id` int(10) UNSIGNED NOT NULL,
  `query_text` varchar(250) COLLATE utf8mb4_unicode_ci NOT NULL,
  `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `student_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Dumping data for table `history_tbl`
--

INSERT INTO `history_tbl` (`history_id`, `query_text`, `created_at`, `student_id`) VALUES
(1, 'SELECT * FROM sample_tbl WHERE 1=1', '2017-11-24 18:50:00', 4),
(2, 'SELECT sample_col FROM sample_tbl', '2017-11-25 22:40:17', 4),
(3, 'SELECT * from sample_tbl WHERE 1=1', '2017-11-27 13:00:23', 4),
(4, 'INSERT DATA', '2017-11-27 13:24:42', 4),
(5, 'SELECT * FROM information_schema', '2017-11-27 14:12:34', 4),
(6, 'SELECT * FROM information_schema WHERE 1=1', '2017-11-27 14:13:15', 4),
(7, 'SELECT * from sample_tbl WHERE 1=1', '2018-01-09 10:13:53', 4),
(8, 'SELECT * from sample_tbl', '2018-01-09 10:14:07', 4),
(9, 'SELECT * FROM sample_tbl WHERE 1=1', '2018-01-09 10:39:10', 4);

-- --------------------------------------------------------

--
-- Table structure for table `migrations`
--

CREATE TABLE `migrations` (
  `id` int(10) UNSIGNED NOT NULL,
  `migration` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  `batch` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Dumping data for table `migrations`
--

INSERT INTO `migrations` (`id`, `migration`, `batch`) VALUES
(1, '2014_10_12_100000_create_password_resets_table', 1),
(2, '2017_10_18_045333_create_prof_table', 1),
(3, '2017_11_20_014703_create_class_tbl', 1),
(4, '2017_11_20_015019_create_student_tbl', 1),
(5, '2017_11_20_015131_create_quiz_tbl', 1),
(6, '2017_11_20_015207_create_question_tbl', 1),
(7, '2017_11_20_015241_create_history_tbl', 1),
(8, '2017_11_24_064018_create_quiz_sched_tbl', 1),
(9, '2017_12_01_011407_create_files_table', 1),
(10, '2017_12_22_065438_user_acct_tbl', 1),
(11, '2017_12_22_071606_database_tbl', 1),
(12, '2017_12_22_072031_tables_tbl', 1),
(13, '2017_12_22_072109_columns_tbl', 1),
(14, '2017_12_22_072345_foreign_keys_tbl', 1),
(15, '2017_12_22_072533_entries_tbl', 1);

-- --------------------------------------------------------

--
-- Table structure for table `password_resets`
--

CREATE TABLE `password_resets` (
  `email` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  `token` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  `created_at` timestamp NULL DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- --------------------------------------------------------

--
-- Table structure for table `professor`
--

CREATE TABLE `professor` (
  `prof_id` int(10) UNSIGNED NOT NULL,
  `prof_fName` varchar(50) COLLATE utf8mb4_unicode_ci NOT NULL,
  `prof_mName` varchar(50) COLLATE utf8mb4_unicode_ci NOT NULL,
  `prof_lName` varchar(50) COLLATE utf8mb4_unicode_ci NOT NULL,
  `prof_email` varchar(50) COLLATE utf8mb4_unicode_ci NOT NULL,
  `password` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL,
  `remember_token` varchar(100) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT NULL,
  `updated_at` timestamp NULL DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Dumping data for table `professor`
--

INSERT INTO `professor` (`prof_id`, `prof_fName`, `prof_mName`, `prof_lName`, `prof_email`, `password`, `remember_token`, `created_at`, `updated_at`) VALUES
(1, 'Jazelene Mae', 'Mercado', 'De Vera', 'jazelenemae@gmail.com', '$2y$10$w9SjxN7htF2txqX9Kp3ss.X4rEdDW5XSj9VUoHkf9NZJgtm4l5qHu', NULL, '2017-10-17 18:03:34', '2017-10-17 18:03:34');

-- --------------------------------------------------------

--
-- Table structure for table `question_tbl`
--

CREATE TABLE `question_tbl` (
  `question_id` int(10) UNSIGNED NOT NULL,
  `question` varchar(500) COLLATE utf8mb4_unicode_ci NOT NULL,
  `choice_A` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL,
  `choice_B` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL,
  `choice_C` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL,
  `choice_D` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL,
  `correct_ans` varchar(50) COLLATE utf8mb4_unicode_ci NOT NULL,
  `quiz_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Dumping data for table `question_tbl`
--

INSERT INTO `question_tbl` (`question_id`, `question`, `choice_A`, `choice_B`, `choice_C`, `choice_D`, `correct_ans`, `quiz_id`) VALUES
(3, 'Which of the following gives a logical structure of a database graphically?', 'Entity-Relationship Diagram', 'Entity Diagram', 'Database Diagram', 'Architectural Representation', 'choice_A', 2),
(4, 'The entity relationship set is represented in E-R diagram as?', 'Double diamonds', 'Undivided rectangles', 'Dashed lines', 'Diamond', 'choice_D', 2),
(5, 'If you were collecting and storing information about your music collection, an album would be considered a/an?', 'Relation', 'Entity', 'Instance', 'Attribute', 'choice_B', 2),
(6, 'What is a relationship called when it is maintained between two entities?', 'Unary', 'Binary', 'Ternary', 'Quaternary', 'choice_B', 2),
(7, 'A _________ is a collection of related fields.', 'Column', 'Field', 'Record', 'Table', 'choice_C', 2),
(8, 'San tayo kakain?', 'Jollibee', 'Mcdonald', 'KFC', 'Chowking', 'C', 3),
(9, 'San tayo kakain?', 'Jollibee', 'Mcdonald', 'KFC', 'Chowking', 'C', 4);

-- --------------------------------------------------------

--
-- Table structure for table `quiz_sched_tbl`
--

CREATE TABLE `quiz_sched_tbl` (
  `quiz_sched_id` int(10) UNSIGNED NOT NULL,
  `quiz_date` date NOT NULL,
  `time_start` varchar(10) COLLATE utf8mb4_unicode_ci NOT NULL,
  `time_end` varchar(10) COLLATE utf8mb4_unicode_ci NOT NULL,
  `status` int(10) UNSIGNED DEFAULT NULL,
  `class_id` int(10) UNSIGNED DEFAULT NULL,
  `quiz_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Dumping data for table `quiz_sched_tbl`
--

INSERT INTO `quiz_sched_tbl` (`quiz_sched_id`, `quiz_date`, `time_start`, `time_end`, `status`, `class_id`, `quiz_id`) VALUES
(1, '2017-11-27', '9:50AM', '10:20AM', 0, 2, 2);

-- --------------------------------------------------------

--
-- Table structure for table `quiz_tbl`
--

CREATE TABLE `quiz_tbl` (
  `quiz_id` int(10) UNSIGNED NOT NULL,
  `quiz_title` varchar(50) COLLATE utf8mb4_unicode_ci NOT NULL,
  `quiz_desc` varchar(150) COLLATE utf8mb4_unicode_ci NOT NULL,
  `created_at` timestamp NULL DEFAULT NULL,
  `updated_at` timestamp NULL DEFAULT NULL,
  `prof_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Dumping data for table `quiz_tbl`
--

INSERT INTO `quiz_tbl` (`quiz_id`, `quiz_title`, `quiz_desc`, `created_at`, `updated_at`, `prof_id`) VALUES
(2, 'Sample SQL Quiz', 'Choose the correct answer.', '2017-11-26 17:19:52', NULL, 1),
(3, 'Eating place', 'Food', '2018-01-09 18:17:15', NULL, 1),
(4, 'Eating place', 'Food', '2018-01-09 18:17:33', NULL, 1);

-- --------------------------------------------------------

--
-- Table structure for table `student`
--

CREATE TABLE `student` (
  `student_id` int(10) UNSIGNED NOT NULL,
  `student_name` varchar(200) COLLATE utf8mb4_unicode_ci NOT NULL,
  `student_email` varchar(50) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `class_id` int(10) UNSIGNED DEFAULT NULL,
  `password` varchar(100) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `remember_token` varchar(100) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT NULL,
  `updated_at` timestamp NULL DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Dumping data for table `student`
--

INSERT INTO `student` (`student_id`, `student_name`, `student_email`, `class_id`, `password`, `remember_token`, `created_at`, `updated_at`) VALUES
(4, 'Donald Patrick C. George', 'donpatrick15@gmail.com', 2, '$2y$10$j9gI3QIlQtrQFvPbE5qF9uZZ9GeGt/RFUx6mJfNHRBt6S3PqYKFRS', 'wi2A5DUV5KdBAoxrH8fl6BAklW9t2MhkhqlO5i1k3sYQoM5aLdm6oyMs4Sca', '2017-10-17 10:17:57', '2017-10-17 10:17:57'),
(5, 'Jazelene Mae M. De Vera', 'jazelenemae@gmail.com', 2, '$2y$10$vTSYOQ4svmkc6SZDHIKdD.AwPKZvWL/TosssnxdeJfw2N./vpgBsG', NULL, '2017-10-17 17:36:01', '2017-10-17 17:36:01'),
(6, 'Aimee Lou D. Cruz', 'aimeecruz107@gmail.com', 2, '$2y$10$v7Ce89aOhIMf3cFOmkHBVutTWnx9rI5Xe2eSBpJT/Hk9Tn1zD0iBa', 'pQpflVX4J9lH2zlMKatQsBMwDtPFTXfIlrrIAeku1SGWTYPdMFzVqF4mYIcU', '2017-10-17 17:52:54', '2017-10-17 17:52:54');

-- --------------------------------------------------------

--
-- Table structure for table `table_tbl`
--

CREATE TABLE `table_tbl` (
  `table_id` int(10) UNSIGNED NOT NULL,
  `table_name` varchar(200) COLLATE utf8mb4_unicode_ci NOT NULL,
  `database_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- --------------------------------------------------------

--
-- Table structure for table `user_acct_tbl`
--

CREATE TABLE `user_acct_tbl` (
  `user_acct_id` int(10) UNSIGNED NOT NULL,
  `createDB` enum('true','false') COLLATE utf8mb4_unicode_ci NOT NULL,
  `dropDB` enum('true','false') COLLATE utf8mb4_unicode_ci NOT NULL,
  `createTBL` enum('true','false') COLLATE utf8mb4_unicode_ci NOT NULL,
  `dropTBL` enum('true','false') COLLATE utf8mb4_unicode_ci NOT NULL,
  `alterTBL` enum('true','false') COLLATE utf8mb4_unicode_ci NOT NULL,
  `student_id` int(10) UNSIGNED DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Indexes for dumped tables
--

--
-- Indexes for table `class_tbl`
--
ALTER TABLE `class_tbl`
  ADD PRIMARY KEY (`class_id`),
  ADD KEY `class_tbl_prof_id_foreign` (`prof_id`);

--
-- Indexes for table `columns_tbl`
--
ALTER TABLE `columns_tbl`
  ADD PRIMARY KEY (`column_id`),
  ADD KEY `columns_tbl_table_id_foreign` (`table_id`);

--
-- Indexes for table `database_tbl`
--
ALTER TABLE `database_tbl`
  ADD PRIMARY KEY (`database_id`),
  ADD KEY `database_tbl_user_acct_id_foreign` (`user_acct_id`);

--
-- Indexes for table `entries_tbl`
--
ALTER TABLE `entries_tbl`
  ADD PRIMARY KEY (`entry_id`),
  ADD KEY `entries_tbl_column_id_foreign` (`column_id`);

--
-- Indexes for table `files_tbl`
--
ALTER TABLE `files_tbl`
  ADD PRIMARY KEY (`file_id`),
  ADD KEY `files_tbl_prof_id_foreign` (`prof_id`);

--
-- Indexes for table `foreign_tbl`
--
ALTER TABLE `foreign_tbl`
  ADD PRIMARY KEY (`foreign_id`),
  ADD KEY `foreign_tbl_foreign_col_id_foreign` (`foreign_col_id`),
  ADD KEY `foreign_tbl_reference_pri_col_id_foreign` (`reference_pri_col_id`);

--
-- Indexes for table `history_tbl`
--
ALTER TABLE `history_tbl`
  ADD PRIMARY KEY (`history_id`),
  ADD KEY `history_tbl_student_id_foreign` (`student_id`);

--
-- Indexes for table `migrations`
--
ALTER TABLE `migrations`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `password_resets`
--
ALTER TABLE `password_resets`
  ADD KEY `password_resets_email_index` (`email`);

--
-- Indexes for table `professor`
--
ALTER TABLE `professor`
  ADD PRIMARY KEY (`prof_id`);

--
-- Indexes for table `question_tbl`
--
ALTER TABLE `question_tbl`
  ADD PRIMARY KEY (`question_id`),
  ADD KEY `question_tbl_quiz_id_foreign` (`quiz_id`);

--
-- Indexes for table `quiz_sched_tbl`
--
ALTER TABLE `quiz_sched_tbl`
  ADD PRIMARY KEY (`quiz_sched_id`),
  ADD KEY `quiz_sched_tbl_class_id_foreign` (`class_id`),
  ADD KEY `quiz_sched_tbl_quiz_id_foreign` (`quiz_id`);

--
-- Indexes for table `quiz_tbl`
--
ALTER TABLE `quiz_tbl`
  ADD PRIMARY KEY (`quiz_id`),
  ADD KEY `quiz_tbl_prof_id_foreign` (`prof_id`);

--
-- Indexes for table `student`
--
ALTER TABLE `student`
  ADD PRIMARY KEY (`student_id`),
  ADD KEY `student_class_id_foreign` (`class_id`);

--
-- Indexes for table `table_tbl`
--
ALTER TABLE `table_tbl`
  ADD PRIMARY KEY (`table_id`),
  ADD KEY `table_tbl_database_id_foreign` (`database_id`);

--
-- Indexes for table `user_acct_tbl`
--
ALTER TABLE `user_acct_tbl`
  ADD PRIMARY KEY (`user_acct_id`),
  ADD KEY `user_acct_tbl_student_id_foreign` (`student_id`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `class_tbl`
--
ALTER TABLE `class_tbl`
  MODIFY `class_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=3;
--
-- AUTO_INCREMENT for table `columns_tbl`
--
ALTER TABLE `columns_tbl`
  MODIFY `column_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `database_tbl`
--
ALTER TABLE `database_tbl`
  MODIFY `database_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `entries_tbl`
--
ALTER TABLE `entries_tbl`
  MODIFY `entry_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `files_tbl`
--
ALTER TABLE `files_tbl`
  MODIFY `file_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=4;
--
-- AUTO_INCREMENT for table `foreign_tbl`
--
ALTER TABLE `foreign_tbl`
  MODIFY `foreign_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `history_tbl`
--
ALTER TABLE `history_tbl`
  MODIFY `history_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=10;
--
-- AUTO_INCREMENT for table `migrations`
--
ALTER TABLE `migrations`
  MODIFY `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=16;
--
-- AUTO_INCREMENT for table `professor`
--
ALTER TABLE `professor`
  MODIFY `prof_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=2;
--
-- AUTO_INCREMENT for table `question_tbl`
--
ALTER TABLE `question_tbl`
  MODIFY `question_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=10;
--
-- AUTO_INCREMENT for table `quiz_sched_tbl`
--
ALTER TABLE `quiz_sched_tbl`
  MODIFY `quiz_sched_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=2;
--
-- AUTO_INCREMENT for table `quiz_tbl`
--
ALTER TABLE `quiz_tbl`
  MODIFY `quiz_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=5;
--
-- AUTO_INCREMENT for table `student`
--
ALTER TABLE `student`
  MODIFY `student_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=7;
--
-- AUTO_INCREMENT for table `table_tbl`
--
ALTER TABLE `table_tbl`
  MODIFY `table_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `user_acct_tbl`
--
ALTER TABLE `user_acct_tbl`
  MODIFY `user_acct_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT;
--
-- Constraints for dumped tables
--

--
-- Constraints for table `class_tbl`
--
ALTER TABLE `class_tbl`
  ADD CONSTRAINT `class_tbl_prof_id_foreign` FOREIGN KEY (`prof_id`) REFERENCES `professor` (`prof_id`) ON DELETE CASCADE;

--
-- Constraints for table `columns_tbl`
--
ALTER TABLE `columns_tbl`
  ADD CONSTRAINT `columns_tbl_table_id_foreign` FOREIGN KEY (`table_id`) REFERENCES `table_tbl` (`table_id`);

--
-- Constraints for table `database_tbl`
--
ALTER TABLE `database_tbl`
  ADD CONSTRAINT `database_tbl_user_acct_id_foreign` FOREIGN KEY (`user_acct_id`) REFERENCES `user_acct_tbl` (`user_acct_id`);

--
-- Constraints for table `entries_tbl`
--
ALTER TABLE `entries_tbl`
  ADD CONSTRAINT `entries_tbl_column_id_foreign` FOREIGN KEY (`column_id`) REFERENCES `columns_tbl` (`column_id`);

--
-- Constraints for table `files_tbl`
--
ALTER TABLE `files_tbl`
  ADD CONSTRAINT `files_tbl_prof_id_foreign` FOREIGN KEY (`prof_id`) REFERENCES `professor` (`prof_id`);

--
-- Constraints for table `foreign_tbl`
--
ALTER TABLE `foreign_tbl`
  ADD CONSTRAINT `foreign_tbl_foreign_col_id_foreign` FOREIGN KEY (`foreign_col_id`) REFERENCES `columns_tbl` (`column_id`),
  ADD CONSTRAINT `foreign_tbl_reference_pri_col_id_foreign` FOREIGN KEY (`reference_pri_col_id`) REFERENCES `columns_tbl` (`column_id`);

--
-- Constraints for table `history_tbl`
--
ALTER TABLE `history_tbl`
  ADD CONSTRAINT `history_tbl_student_id_foreign` FOREIGN KEY (`student_id`) REFERENCES `student` (`student_id`);

--
-- Constraints for table `question_tbl`
--
ALTER TABLE `question_tbl`
  ADD CONSTRAINT `question_tbl_quiz_id_foreign` FOREIGN KEY (`quiz_id`) REFERENCES `quiz_tbl` (`quiz_id`) ON DELETE CASCADE;

--
-- Constraints for table `quiz_sched_tbl`
--
ALTER TABLE `quiz_sched_tbl`
  ADD CONSTRAINT `quiz_sched_tbl_class_id_foreign` FOREIGN KEY (`class_id`) REFERENCES `class_tbl` (`class_id`),
  ADD CONSTRAINT `quiz_sched_tbl_quiz_id_foreign` FOREIGN KEY (`quiz_id`) REFERENCES `quiz_tbl` (`quiz_id`);

--
-- Constraints for table `quiz_tbl`
--
ALTER TABLE `quiz_tbl`
  ADD CONSTRAINT `quiz_tbl_prof_id_foreign` FOREIGN KEY (`prof_id`) REFERENCES `professor` (`prof_id`);

--
-- Constraints for table `student`
--
ALTER TABLE `student`
  ADD CONSTRAINT `student_class_id_foreign` FOREIGN KEY (`class_id`) REFERENCES `class_tbl` (`class_id`) ON DELETE CASCADE;

--
-- Constraints for table `table_tbl`
--
ALTER TABLE `table_tbl`
  ADD CONSTRAINT `table_tbl_database_id_foreign` FOREIGN KEY (`database_id`) REFERENCES `database_tbl` (`database_id`);

--
-- Constraints for table `user_acct_tbl`
--
ALTER TABLE `user_acct_tbl`
  ADD CONSTRAINT `user_acct_tbl_student_id_foreign` FOREIGN KEY (`student_id`) REFERENCES `student` (`student_id`);
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
