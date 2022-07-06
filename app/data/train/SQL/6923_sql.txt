-- phpMyAdmin SQL Dump
-- version 4.6.5.2
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1
-- Generation Time: Feb 22, 2017 at 02:44 PM
-- Server version: 10.1.21-MariaDB
-- PHP Version: 5.6.30

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `mtp`
--

-- --------------------------------------------------------

--
-- Table structure for table `admin`
--

CREATE TABLE `admin` (
  `adminId` int(4) NOT NULL,
  `adminName` char(30) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `password` char(30) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `name` char(50) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `admin`
--

INSERT INTO `admin` (`adminId`, `adminName`, `password`, `name`) VALUES
(1, 'anhtu', 'anhtu123', 'Đoàn Anh Tú');

-- --------------------------------------------------------

--
-- Table structure for table `catalog`
--

CREATE TABLE `catalog` (
  `catalogId` int(11) NOT NULL,
  `name` char(50) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `parent` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `catalog`
--

INSERT INTO `catalog` (`catalogId`, `name`, `parent`) VALUES
(1, 'Áo Khoác Nam', NULL),
(2, 'Áo Len Dài Tay', NULL),
(3, 'Áo Sơ Mi Nam', NULL),
(4, 'Áo Thun Nam', NULL),
(5, 'Quần Jeans Nam', NULL),
(6, 'Quần Kaki Nam', NULL),
(7, 'Quần Shorts Nam', NULL),
(8, 'Phụ Kiện Nam', NULL),
(9, 'Cardigan', 1),
(10, 'Khoác Da', 1),
(11, 'Khoác Jeans', 1),
(12, 'Khoác Nỉ', 1),
(13, 'Khoác Vest', 1),
(14, 'Sơ Mi Jeans - Denim', 3),
(15, 'Sơ Mi Nhập', 3),
(16, 'Sơ Mi Thiết Kế', 3),
(17, 'Thun Body Trơn', 4),
(18, 'Thun Hàn Quốc', 4),
(19, 'Thun Hồng Kông', 4),
(20, 'Jean Rách – Xước', 5),
(21, 'Jean Thun- Skinny', 5),
(22, 'Jeans Ống Đỉa', 5),
(23, 'Thắt Lưng', 8),
(24, 'Túi Ipad', 8),
(25, 'Bóp - Ví', 8),
(26, 'Nón', 88),
(27, 'Mắt Kính', 8);

-- --------------------------------------------------------

--
-- Table structure for table `orders`
--

CREATE TABLE `orders` (
  `orderId` int(11) NOT NULL,
  `transactionId` int(11) NOT NULL,
  `productId` int(11) NOT NULL,
  `name` char(8) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `qty` int(11) NOT NULL,
  `amount` decimal(10,0) NOT NULL,
  `size` char(1) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `product`
--

CREATE TABLE `product` (
  `productId` int(11) NOT NULL,
  `catalogId` int(11) NOT NULL,
  `name` char(100) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `price` int(11) NOT NULL,
  `description` text CHARACTER SET utf8 COLLATE utf8_unicode_ci,
  `images` char(200) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `product`
--

INSERT INTO `product` (`productId`, `catalogId`, `name`, `price`, `description`, `images`) VALUES
(1, 9, 'AK-007', 500000, '<p>New Products</p>\r\n', 'http://localhost/mtp/asset/images/products/AK-007.jpg'),
(2, 9, 'AK-008', 500000, '<p>New Product</p>\r\n', 'http://localhost/mtp/asset/images/products/AK-008.jpg');

-- --------------------------------------------------------

--
-- Table structure for table `transaction`
--

CREATE TABLE `transaction` (
  `transactionId` int(11) NOT NULL,
  `status` int(11) NOT NULL,
  `userId` int(11) NOT NULL,
  `fullname` char(50) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `email` char(100) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `phone` char(15) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `address` char(150) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `amount` decimal(10,0) NOT NULL,
  `message` text CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `created` date NOT NULL,
  `createt` char(12) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `transaction`
--

INSERT INTO `transaction` (`transactionId`, `status`, `userId`, `fullname`, `email`, `phone`, `address`, `amount`, `message`, `created`, `createt`) VALUES
(1, 0, 1, 'Đoàn Anh Tú', 'doananhtu13395@gmail.com', '0963287143', 'Ngõ 15 Phương Mai, Đống Đa, Hà Nội', '500000', 'nhanh lên nhé', '2017-02-22', '04:43:23pm'),
(2, 0, 1, 'Đoàn Anh Tú', 'doananhtu13395@gmail.com', '0963287143', 'Ngõ 15 Phương Mai, Đống Đa, Hà Nội', '500000', 'nhanh lên nhé', '2017-02-22', '04:44:56pm'),
(3, 0, 1, 'Đoàn Anh Tú', 'trantien13395@gmail.com', '0963287143', 'Ngõ 15 Phương Mai, Đống Đa, Hà Nội', '2500000', 'ahihi', '2017-02-22', '08:38:41pm');

-- --------------------------------------------------------

--
-- Table structure for table `users`
--

CREATE TABLE `users` (
  `userId` int(11) NOT NULL,
  `name` char(30) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `email` char(50) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `password` char(30) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `phone` char(12) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `address` char(150) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `users`
--

INSERT INTO `users` (`userId`, `name`, `email`, `password`, `phone`, `address`) VALUES
(1, 'anhtu', 'anhtu@gmail.com', '123456', '0963287143', 'Hà Nội');

--
-- Indexes for dumped tables
--

--
-- Indexes for table `admin`
--
ALTER TABLE `admin`
  ADD PRIMARY KEY (`adminId`);

--
-- Indexes for table `catalog`
--
ALTER TABLE `catalog`
  ADD PRIMARY KEY (`catalogId`);

--
-- Indexes for table `orders`
--
ALTER TABLE `orders`
  ADD PRIMARY KEY (`orderId`),
  ADD KEY `KN1` (`transactionId`),
  ADD KEY `KN2` (`productId`);

--
-- Indexes for table `product`
--
ALTER TABLE `product`
  ADD PRIMARY KEY (`productId`),
  ADD KEY `KN` (`catalogId`);

--
-- Indexes for table `transaction`
--
ALTER TABLE `transaction`
  ADD PRIMARY KEY (`transactionId`),
  ADD KEY `KNTransaction` (`userId`);

--
-- Indexes for table `users`
--
ALTER TABLE `users`
  ADD PRIMARY KEY (`userId`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `admin`
--
ALTER TABLE `admin`
  MODIFY `adminId` int(4) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=2;
--
-- AUTO_INCREMENT for table `orders`
--
ALTER TABLE `orders`
  MODIFY `orderId` int(11) NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `product`
--
ALTER TABLE `product`
  MODIFY `productId` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=3;
--
-- AUTO_INCREMENT for table `transaction`
--
ALTER TABLE `transaction`
  MODIFY `transactionId` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=4;
--
-- AUTO_INCREMENT for table `users`
--
ALTER TABLE `users`
  MODIFY `userId` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=2;
--
-- Constraints for dumped tables
--

--
-- Constraints for table `orders`
--
ALTER TABLE `orders`
  ADD CONSTRAINT `KN1` FOREIGN KEY (`transactionId`) REFERENCES `transaction` (`transactionId`),
  ADD CONSTRAINT `KN2` FOREIGN KEY (`productId`) REFERENCES `product` (`productId`);

--
-- Constraints for table `product`
--
ALTER TABLE `product`
  ADD CONSTRAINT `KN` FOREIGN KEY (`catalogId`) REFERENCES `catalog` (`catalogId`);

--
-- Constraints for table `transaction`
--
ALTER TABLE `transaction`
  ADD CONSTRAINT `KNTransaction` FOREIGN KEY (`userId`) REFERENCES `users` (`userId`);

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
