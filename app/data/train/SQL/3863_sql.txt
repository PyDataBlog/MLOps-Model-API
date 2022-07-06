-- phpMyAdmin SQL Dump
-- version 4.4.14
-- http://www.phpmyadmin.net
--
-- Host: 127.0.0.1
-- Generation Time: 08 Jun 2016 pada 15.38
-- Versi Server: 5.6.26
-- PHP Version: 5.6.12

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `laundry_db`
--

-- --------------------------------------------------------

--
-- Struktur dari tabel `customer`
--

CREATE TABLE IF NOT EXISTS `customer` (
  `id` int(11) NOT NULL,
  `nama_customer` varchar(30) NOT NULL,
  `alamat` varchar(50) NOT NULL,
  `no_hp` int(15) NOT NULL,
  `email` varchar(30) NOT NULL,
  `catatan` varchar(50) NOT NULL,
  `status` varchar(10) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Struktur dari tabel `file`
--

CREATE TABLE IF NOT EXISTS `file` (
  `id` varchar(255) NOT NULL,
  `table_name` varchar(255) DEFAULT NULL,
  `table_field` varchar(255) DEFAULT NULL,
  `field_value` varchar(255) DEFAULT NULL,
  `file_name` varchar(255) DEFAULT NULL,
  `url` varchar(255) DEFAULT NULL,
  `input_by` varchar(255) DEFAULT NULL,
  `input_date` datetime DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `file`
--

INSERT INTO `file` (`id`, `table_name`, `table_field`, `field_value`, `file_name`, `url`, `input_by`, `input_date`) VALUES
('c60d98aaedb65c8ba5e023cbacb41874eb4d22ad', 'user', 'id', 'c4cab7526d9d124d79b30c818d2fcc37a1f63c73', 'Tamako.Market.600.1416434.jpg', '2016/05/c60d98aaedb65c8ba5e023cbacb41874eb4d22ad.jpg', 'c4cab7526d9d124d79b30c818d2fcc37a1f63c73', '2016-05-12 11:39:53'),
('da914eaac18d19433fe9c5b3eb8788c7b9457650', 'user', 'id', 'c4cab7526d9d124d79b30c818d2fcc37a1f63c73', 'Emoticons-icon.png', '2016/06/da914eaac18d19433fe9c5b3eb8788c7b9457650.png', 'c4cab7526d9d124d79b30c818d2fcc37a1f63c73', '2016-06-08 06:20:56'),
('eaad7968553d65eed16bdf5ddcfd516a33858b58', 'user', 'id', 'c4cab7526d9d124d79b30c818d2fcc37a1f63c73', 'user2-160x160.jpg', '2016/06/eaad7968553d65eed16bdf5ddcfd516a33858b58.jpg', 'c4cab7526d9d124d79b30c818d2fcc37a1f63c73', '2016-06-03 05:51:08');

-- --------------------------------------------------------

--
-- Struktur dari tabel `group`
--

CREATE TABLE IF NOT EXISTS `group` (
  `id` varchar(255) NOT NULL DEFAULT '',
  `name` varchar(255) DEFAULT NULL,
  `create_date` datetime DEFAULT NULL,
  `create_by` int(11) DEFAULT '0',
  `update_date` datetime DEFAULT NULL,
  `update_by` varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `group`
--

INSERT INTO `group` (`id`, `name`, `create_date`, `create_by`, `update_date`, `update_by`) VALUES
('54345880d047bf8496e0df28c4a3db5359083240', 'Operator', '2015-04-28 10:58:29', 0, '2015-07-14 10:35:49', 'c4cab7526d9d124d79b30c818d2fcc37a1f63c73'),
('67544603076af2daf15675402647dcbe85c629ec', 'Administrator', '2015-04-28 10:57:53', 0, '2015-07-02 08:20:31', 'c4cab7526d9d124d79b30c818d2fcc37a1f63c73'),
('da51c984897cd84a051074fe171d5a2a74757e45', 'Auditor', '2015-04-28 10:58:39', 0, '2015-07-02 08:22:16', 'c4cab7526d9d124d79b30c818d2fcc37a1f63c73');

-- --------------------------------------------------------

--
-- Struktur dari tabel `layanan`
--

CREATE TABLE IF NOT EXISTS `layanan` (
  `id` int(11) NOT NULL,
  `nama_layanan` varchar(30) NOT NULL,
  `unit` varchar(10) NOT NULL,
  `harga_normal` int(10) NOT NULL,
  `harga_express` int(10) NOT NULL
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `layanan`
--

INSERT INTO `layanan` (`id`, `nama_layanan`, `unit`, `harga_normal`, `harga_express`) VALUES
(1, 'Cuci Setrika', 'KG', 4000, 6000),
(2, 'Cuci', 'KG', 3000, 4500),
(3, 'Setrika', 'KG', 3000, 4500);

-- --------------------------------------------------------

--
-- Struktur dari tabel `menu`
--

CREATE TABLE IF NOT EXISTS `menu` (
  `id` varchar(255) NOT NULL DEFAULT '',
  `name` varchar(255) DEFAULT NULL,
  `url` varchar(255) DEFAULT NULL,
  `icon` varchar(255) DEFAULT NULL COMMENT 'full class icon template',
  `status` int(1) DEFAULT NULL,
  `order` int(11) DEFAULT NULL,
  `parent` varchar(255) DEFAULT NULL,
  `state` varchar(255) DEFAULT 'view',
  `create_date` datetime DEFAULT NULL,
  `create_by` int(11) DEFAULT '0',
  `update_date` datetime DEFAULT NULL,
  `update_by` varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `menu`
--

INSERT INTO `menu` (`id`, `name`, `url`, `icon`, `status`, `order`, `parent`, `state`, `create_date`, `create_by`, `update_date`, `update_by`) VALUES
('1', 'TRANSAKSI', 'transaksi', 'fa fa-shopping-cart', 0, 7, '0', 'view', '2016-06-04 00:00:00', 1, NULL, '0'),
('10', 'Rekap Transaksi', 'rekapitulasi', 'fa fa-calendar', 1, 1, '4', 'view', '2016-06-07 00:00:00', 1, NULL, NULL),
('11', 'Rekap Lunas', 'rekapitulasi/rekap_lunas', 'fa fa-calendar-check-o', 1, 2, '4', 'view', '2016-06-07 00:00:00', 1, NULL, NULL),
('12', 'Rekap Belum Lunas', 'rekapitulasi/rekap_belum_lunas', 'fa fa-calendar-times-o', 1, 3, '4', 'view', '2016-06-07 00:00:00', 1, NULL, NULL),
('13', 'Tambah Status', 'status/form', 'fa fa-exchange', 1, 1, '5', 'view', '2016-06-07 00:00:00', 1, NULL, NULL),
('14', 'Status Laundry', 'status', 'fa fa-calendar-check-o', 1, 2, '5', 'view', '2016-06-07 00:00:00', 1, NULL, NULL),
('16', 'Layanan & Harga', 'pengaturan/layanan', 'fa fa-money', 1, 4, 'e0fdbf7f46ee7595818fc4f42a8c91f86c455d69', 'view', '2016-06-07 00:00:00', 1, NULL, NULL),
('3', 'CUSTOMER', '#', 'fa fa-users', 1, 3, '0', 'view', '2016-06-04 00:00:00', 1, NULL, NULL),
('4', 'REKAPITULASI', '#', 'fa fa-file-text-o', 1, 4, '0', 'view', '2016-06-04 00:00:00', 1, NULL, NULL),
('44413d5ea0d9d0d5220be1591110373b3ca72651', 'TRANSAKSI', 'transaksi', 'fa fa-shopping-cart', 1, 2, '0', 'view', NULL, 1, NULL, '0'),
('5', 'STATUS LAUNDRY', '#', 'fa fa-info-circle', 1, 5, '0', 'view', '2016-06-04 00:00:00', 1, NULL, NULL),
('6', 'PENGATURAN', '#', 'fa fa-cog', 1, 6, '0', 'view', '2016-06-04 00:00:00', 1, NULL, NULL),
('8', 'Tambah Customer', 'customer/form', 'fa fa-user-plus', 1, 1, '3', 'view', '2016-06-07 00:00:00', 1, NULL, NULL),
('9', 'Data Customer', 'customer', 'fa fa-list', 1, 2, '3', 'view', '2016-06-07 00:00:00', 1, NULL, NULL),
('acf9b498cbaa8de7ae8df387458ebe3a91c27c94', 'Group', 'group', 'fa fa-users', 0, 1, 'e0fdbf7f46ee7595818fc4f42a8c91f86c455d69', 'view\r\n', '2015-05-06 18:27:31', 1, NULL, '0'),
('e0fdbf7f46ee7595818fc4f42a8c91f86c455d69', 'PENGATURAN', '#', 'fa fa-cogs', 1, 6, '0', 'view', NULL, 0, NULL, '0'),
('ec43ea6c5b44b24687a3801c69aa91d563730174', 'Profile', 'user/edit?id=c4cab7526d9d124d79b30c818d2fcc37a1f63c73', 'fa fa-user', 1, 2, 'e0fdbf7f46ee7595818fc4f42a8c91f86c455d69', 'view', '2015-04-23 16:35:09', 1, NULL, '0'),
('ec55fc1a3ef2ed87f37dc222ab5a49f29b47262d', 'DASHBOARD', 'dashboard', 'fa fa-dashboard', 1, 1, '0', 'view', '2015-04-22 12:49:30', 1, NULL, '0');

-- --------------------------------------------------------

--
-- Struktur dari tabel `menugroup`
--

CREATE TABLE IF NOT EXISTS `menugroup` (
  `group_id` varchar(255) NOT NULL DEFAULT '',
  `menu_id` varchar(255) NOT NULL DEFAULT '',
  `view` tinyint(1) DEFAULT NULL,
  `add` tinyint(1) DEFAULT NULL,
  `edit` tinyint(1) DEFAULT NULL,
  `delete` tinyint(1) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `menugroup`
--

INSERT INTO `menugroup` (`group_id`, `menu_id`, `view`, `add`, `edit`, `delete`) VALUES
('67544603076af2daf15675402647dcbe85c629ec', '1', 1, 1, 1, 1),
('67544603076af2daf15675402647dcbe85c629ec', '10', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '11', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '12', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '13', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '14', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '15', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '16', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '3', 1, 1, 1, 1),
('67544603076af2daf15675402647dcbe85c629ec', '4', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '44413d5ea0d9d0d5220be1591110373b3ca72651', 1, 1, 1, 1),
('67544603076af2daf15675402647dcbe85c629ec', '5', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '6', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '7', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '8', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', '9', NULL, NULL, NULL, NULL),
('67544603076af2daf15675402647dcbe85c629ec', 'acf9b498cbaa8de7ae8df387458ebe3a91c27c94', 1, 1, 1, 1),
('67544603076af2daf15675402647dcbe85c629ec', 'e0fdbf7f46ee7595818fc4f42a8c91f86c455d69', 1, 1, 1, 1),
('67544603076af2daf15675402647dcbe85c629ec', 'ec43ea6c5b44b24687a3801c69aa91d563730174', 1, 1, 1, 1),
('67544603076af2daf15675402647dcbe85c629ec', 'ec55fc1a3ef2ed87f37dc222ab5a49f29b47262d', 1, 1, 1, 1);

-- --------------------------------------------------------

--
-- Struktur dari tabel `outlet`
--

CREATE TABLE IF NOT EXISTS `outlet` (
  `id` int(11) NOT NULL,
  `nama_outlet` text NOT NULL,
  `slogan` varchar(100) NOT NULL,
  `logo` varchar(55) NOT NULL,
  `alamat` varchar(70) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Struktur dari tabel `profile`
--

CREATE TABLE IF NOT EXISTS `profile` (
  `id` int(2) NOT NULL,
  `username` varchar(50) NOT NULL,
  `password` varchar(50) NOT NULL,
  `nama` text NOT NULL,
  `foto` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `profile`
--

INSERT INTO `profile` (`id`, `username`, `password`, `nama`, `foto`) VALUES
(0, 'admin', '*4ACFE3202A5FF5CF467898FC58AAB1D615029441', 'Karunia Laundry', '');

-- --------------------------------------------------------

--
-- Struktur dari tabel `setting`
--

CREATE TABLE IF NOT EXISTS `setting` (
  `id` int(11) NOT NULL,
  `name` varchar(255) DEFAULT NULL,
  `value` text,
  `create_date` datetime DEFAULT NULL,
  `create_by` int(11) DEFAULT '0',
  `update_date` datetime DEFAULT NULL,
  `update_by` varchar(255) DEFAULT NULL
) ENGINE=InnoDB AUTO_INCREMENT=16 DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `setting`
--

INSERT INTO `setting` (`id`, `name`, `value`, `create_date`, `create_by`, `update_date`, `update_by`) VALUES
(1, 'site_title', 'SIAPP', '2015-04-24 10:32:01', 1, '2015-04-28 16:14:47', '0'),
(2, 'tagline', '', '2015-04-24 10:32:01', 1, '2015-04-28 16:14:47', '0'),
(3, 'email', 'siapp@setneg.go.id', '2015-04-24 10:32:01', 1, '2015-04-28 16:14:47', '0'),
(4, 'time_zone', 'UTC+7', '2015-04-24 10:32:01', 1, '2015-04-28 16:14:47', '0'),
(5, 'lang', 'en', '2015-04-24 10:32:01', 1, '2015-04-28 16:14:47', '0'),
(6, 'logo', '', '2015-04-24 11:31:45', 1, '2015-04-24 11:41:25', '0'),
(7, 'facebook', '', '2015-04-24 13:32:14', 1, '2015-04-28 16:14:44', '0'),
(8, 'twitter', '', '2015-04-24 13:32:14', 1, '2015-04-28 16:14:44', '0'),
(9, 'youtube', '', '2015-04-24 13:32:14', 1, '2015-04-28 16:14:44', '0'),
(10, 'google-plus', '', '2015-04-24 13:32:14', 1, '2015-04-28 16:14:44', '0'),
(11, 'instagram', '', '2015-04-24 13:32:14', 1, '2015-04-28 16:14:44', '0'),
(12, 'template_admin', 'admin_lte', '2015-04-29 17:58:02', 1, NULL, '0'),
(13, 'template', 'one_page', '2015-04-29 17:58:21', 1, NULL, '0'),
(14, 'app_version', '1.0', '2015-06-21 04:36:00', 0, NULL, '0'),
(15, 'first_login', '0', NULL, 0, '2015-09-03 09:19:02', '0');

-- --------------------------------------------------------

--
-- Struktur dari tabel `status_laundry`
--

CREATE TABLE IF NOT EXISTS `status_laundry` (
  `id` int(11) NOT NULL,
  `nama_status` varchar(30) NOT NULL
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `status_laundry`
--

INSERT INTO `status_laundry` (`id`, `nama_status`) VALUES
(1, 'Proses Order'),
(2, 'Proses Cuci'),
(3, 'Proses Pengeringan'),
(4, 'Selesai');

-- --------------------------------------------------------

--
-- Struktur dari tabel `sys_group_users`
--

CREATE TABLE IF NOT EXISTS `sys_group_users` (
  `id` int(11) NOT NULL,
  `level` varchar(50) DEFAULT NULL,
  `deskripsi` varchar(50) DEFAULT NULL
) ENGINE=InnoDB AUTO_INCREMENT=7 DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `sys_group_users`
--

INSERT INTO `sys_group_users` (`id`, `level`, `deskripsi`) VALUES
(1, 'admin', 'Administrator');

-- --------------------------------------------------------

--
-- Struktur dari tabel `sys_menu`
--

CREATE TABLE IF NOT EXISTS `sys_menu` (
  `id` int(11) NOT NULL,
  `nav_act` varchar(150) DEFAULT NULL,
  `page_name` varchar(150) DEFAULT NULL,
  `url` varchar(100) NOT NULL,
  `main_table` varchar(150) DEFAULT NULL,
  `icon` varchar(150) DEFAULT NULL,
  `urutan_menu` int(11) DEFAULT NULL,
  `parent` int(11) DEFAULT NULL,
  `dt_table` enum('Y','N') NOT NULL,
  `tampil` enum('Y','N') NOT NULL,
  `type_menu` enum('main','page') NOT NULL
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `sys_menu`
--

INSERT INTO `sys_menu` (`id`, `nav_act`, `page_name`, `url`, `main_table`, `icon`, `urutan_menu`, `parent`, `dt_table`, `tampil`, `type_menu`) VALUES
(2, 'transaksi', 'transaksi', 'transaksi', 'transaksi', 'fa-shopping-cart', 2, 0, 'Y', 'Y', 'page');

-- --------------------------------------------------------

--
-- Struktur dari tabel `sys_menu_role`
--

CREATE TABLE IF NOT EXISTS `sys_menu_role` (
  `id` int(11) NOT NULL,
  `id_menu` int(11) DEFAULT NULL,
  `group_id` int(11) DEFAULT NULL,
  `read_act` enum('Y','N') DEFAULT NULL,
  `insert_act` enum('Y','N') DEFAULT NULL,
  `update_act` enum('Y','N') DEFAULT NULL,
  `delete_act` enum('Y','N') DEFAULT NULL
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `sys_menu_role`
--

INSERT INTO `sys_menu_role` (`id`, `id_menu`, `group_id`, `read_act`, `insert_act`, `update_act`, `delete_act`) VALUES
(2, 2, 1, 'Y', 'Y', 'Y', 'Y');

-- --------------------------------------------------------

--
-- Struktur dari tabel `sys_users`
--

CREATE TABLE IF NOT EXISTS `sys_users` (
  `id` int(11) NOT NULL,
  `first_name` varchar(50) NOT NULL DEFAULT '0',
  `last_name` varchar(50) NOT NULL DEFAULT '0',
  `username` varchar(50) DEFAULT NULL,
  `password` varchar(50) DEFAULT NULL,
  `email` varchar(50) DEFAULT NULL,
  `date_created` date DEFAULT NULL,
  `foto_user` varchar(150) DEFAULT NULL,
  `id_group` int(11) DEFAULT NULL,
  `aktif` enum('Y','N') NOT NULL
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `sys_users`
--

INSERT INTO `sys_users` (`id`, `first_name`, `last_name`, `username`, `password`, `email`, `date_created`, `foto_user`, `id_group`, `aktif`) VALUES
(1, 'mohamad ', 'wildannudin', 'admin', '21232f297a57a5a743894a0e4a801fc3', 'wildannudin@gmail.com', '2015-01-26', '10965740_10206190197982755_22114424_n.jpg', 1, 'Y');

-- --------------------------------------------------------

--
-- Struktur dari tabel `template`
--

CREATE TABLE IF NOT EXISTS `template` (
  `id` int(11) NOT NULL,
  `name` varchar(255) DEFAULT NULL,
  `url` varchar(255) DEFAULT NULL,
  `type` varchar(255) DEFAULT NULL,
  `image` varchar(255) DEFAULT NULL
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `template`
--

INSERT INTO `template` (`id`, `name`, `url`, `type`, `image`) VALUES
(1, 'sb_admin2', 'template/sb_admin2/', 'admin', NULL),
(2, 'one_page', 'template/one_page/', 'public', NULL),
(3, 'admin_lte', 'template/admin_lte/', 'admin', NULL);

-- --------------------------------------------------------

--
-- Struktur dari tabel `transaksi`
--

CREATE TABLE IF NOT EXISTS `transaksi` (
  `id` int(11) NOT NULL,
  `id_customer` int(11) NOT NULL,
  `no_invoice` int(11) NOT NULL,
  `jenis_bayar` varchar(15) NOT NULL,
  `sub_total` int(10) NOT NULL,
  `diskon` int(10) NOT NULL,
  `total` int(10) NOT NULL,
  `dibayar` int(10) NOT NULL,
  `sisa` int(10) NOT NULL,
  `status_pembayaran` int(15) NOT NULL,
  `status_order` varchar(15) NOT NULL,
  `tgl_terima` date NOT NULL,
  `tgl_selesai` date NOT NULL,
  `tgl_ambil` date NOT NULL,
  `nama_pengambil` varchar(30) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Struktur dari tabel `user`
--

CREATE TABLE IF NOT EXISTS `user` (
  `id` varchar(255) NOT NULL DEFAULT '',
  `name` varchar(255) DEFAULT NULL,
  `username` varchar(255) DEFAULT NULL,
  `password` varchar(255) DEFAULT NULL,
  `email` varchar(255) DEFAULT NULL,
  `group_id` varchar(255) DEFAULT NULL,
  `userfile` varchar(255) DEFAULT NULL,
  `create_date` datetime DEFAULT NULL,
  `create_by` int(11) DEFAULT '0',
  `update_date` datetime DEFAULT NULL,
  `update_by` varchar(255) DEFAULT NULL,
  `last_login` datetime DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `user`
--

INSERT INTO `user` (`id`, `name`, `username`, `password`, `email`, `group_id`, `userfile`, `create_date`, `create_by`, `update_date`, `update_by`, `last_login`) VALUES
('5dab4804c4f05f11d7e2ac6379a55ed9b3d24fda', 'Admin Dev', 'isaadmin', 'a5039e5b86e2b0588c0af55d1c40e74ad616d5d7', 'hanief@isatechindonesia.com', '54345880d047bf8496e0df28c4a3db5359083240', NULL, '2015-04-28 11:06:47', 0, '2015-04-28 16:29:10', '0', '0000-00-00 00:00:00'),
('c4cab7526d9d124d79b30c818d2fcc37a1f63c73', 'Karunia Laundry', 'admin', '90b9aa7e25f80cf4f64e990b78a9fc5ebd6cecad', 'haniefhan@gmail.com', '67544603076af2daf15675402647dcbe85c629ec', NULL, '2015-04-28 12:48:38', 0, '2016-06-08 10:02:14', 'c4cab7526d9d124d79b30c818d2fcc37a1f63c73', '2016-06-08 10:02:14');

--
-- Indexes for dumped tables
--

--
-- Indexes for table `customer`
--
ALTER TABLE `customer`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `file`
--
ALTER TABLE `file`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `group`
--
ALTER TABLE `group`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `layanan`
--
ALTER TABLE `layanan`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `menu`
--
ALTER TABLE `menu`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `menugroup`
--
ALTER TABLE `menugroup`
  ADD PRIMARY KEY (`group_id`,`menu_id`);

--
-- Indexes for table `outlet`
--
ALTER TABLE `outlet`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `profile`
--
ALTER TABLE `profile`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `setting`
--
ALTER TABLE `setting`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `status_laundry`
--
ALTER TABLE `status_laundry`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `sys_group_users`
--
ALTER TABLE `sys_group_users`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `sys_menu`
--
ALTER TABLE `sys_menu`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `sys_menu_role`
--
ALTER TABLE `sys_menu_role`
  ADD PRIMARY KEY (`id`),
  ADD KEY `FK_sys_menu_role_sys_menu` (`id_menu`),
  ADD KEY `FK_sys_menu_role_sys_users` (`group_id`);

--
-- Indexes for table `sys_users`
--
ALTER TABLE `sys_users`
  ADD PRIMARY KEY (`id`),
  ADD KEY `FK_sys_users_sys_group_users` (`id_group`);

--
-- Indexes for table `template`
--
ALTER TABLE `template`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `transaksi`
--
ALTER TABLE `transaksi`
  ADD PRIMARY KEY (`id`);

--
-- Indexes for table `user`
--
ALTER TABLE `user`
  ADD PRIMARY KEY (`id`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `customer`
--
ALTER TABLE `customer`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `layanan`
--
ALTER TABLE `layanan`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=4;
--
-- AUTO_INCREMENT for table `outlet`
--
ALTER TABLE `outlet`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT;
--
-- AUTO_INCREMENT for table `setting`
--
ALTER TABLE `setting`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=16;
--
-- AUTO_INCREMENT for table `status_laundry`
--
ALTER TABLE `status_laundry`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=5;
--
-- AUTO_INCREMENT for table `sys_group_users`
--
ALTER TABLE `sys_group_users`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=7;
--
-- AUTO_INCREMENT for table `sys_menu`
--
ALTER TABLE `sys_menu`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=3;
--
-- AUTO_INCREMENT for table `sys_menu_role`
--
ALTER TABLE `sys_menu_role`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=3;
--
-- AUTO_INCREMENT for table `sys_users`
--
ALTER TABLE `sys_users`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=6;
--
-- AUTO_INCREMENT for table `template`
--
ALTER TABLE `template`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=4;
--
-- AUTO_INCREMENT for table `transaksi`
--
ALTER TABLE `transaksi`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT;
--
-- Ketidakleluasaan untuk tabel pelimpahan (Dumped Tables)
--

--
-- Ketidakleluasaan untuk tabel `sys_menu_role`
--
ALTER TABLE `sys_menu_role`
  ADD CONSTRAINT `FK_sys_menu_role_sys_group_users` FOREIGN KEY (`group_id`) REFERENCES `sys_group_users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  ADD CONSTRAINT `FK_sys_menu_role_sys_menu` FOREIGN KEY (`id_menu`) REFERENCES `sys_menu` (`id`) ON DELETE CASCADE ON UPDATE CASCADE;

--
-- Ketidakleluasaan untuk tabel `sys_users`
--
ALTER TABLE `sys_users`
  ADD CONSTRAINT `FK_sys_users_sys_group_users` FOREIGN KEY (`id_group`) REFERENCES `sys_group_users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
