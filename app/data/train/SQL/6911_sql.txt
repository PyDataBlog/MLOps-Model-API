-- phpMyAdmin SQL Dump
-- version 4.5.1
-- http://www.phpmyadmin.net
--
-- Host: 127.0.0.1
-- Generation Time: 07 Mei 2017 pada 05.48
-- Versi Server: 10.1.13-MariaDB
-- PHP Version: 5.6.20

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `mocount`
--

-- --------------------------------------------------------

--
-- Struktur dari tabel `keinginan`
--

CREATE TABLE `keinginan` (
  `id_keinginan` int(11) NOT NULL,
  `id_user` int(11) NOT NULL,
  `nominal` int(20) NOT NULL,
  `namapenyimpanan` varchar(20) NOT NULL,
  `cicilan` varchar(20) NOT NULL,
  `saran` text NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `keinginan`
--

INSERT INTO `keinginan` (`id_keinginan`, `id_user`, `nominal`, `namapenyimpanan`, `cicilan`, `saran`) VALUES
(1, 3, 450000, 'Warung Kantin', 'Harian', 'crververf'),
(2, 3, 50000, 'Dompet', 'Mingguan', 'Menabung 7142.8571428571/hari ');

-- --------------------------------------------------------

--
-- Struktur dari tabel `pemasukan`
--

CREATE TABLE `pemasukan` (
  `id_pemasukan` int(11) NOT NULL,
  `id_user` int(11) NOT NULL,
  `nominal` int(30) NOT NULL,
  `kategori` varchar(50) NOT NULL,
  `tanggal` date NOT NULL,
  `namapenyimpanan` varchar(50) NOT NULL,
  `catatan` text NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `pemasukan`
--

INSERT INTO `pemasukan` (`id_pemasukan`, `id_user`, `nominal`, `kategori`, `tanggal`, `namapenyimpanan`, `catatan`) VALUES
(1, 0, 67000, 'Lele', '0000-00-00', 'Dompet', 'wewee'),
(2, 0, 50000, 'Lele', '0000-00-00', '3', 'ini untuk jelong-jelong'),
(3, 3, 67000, 'Patin', '0000-00-00', '2', 'ertygbhb\r\nhfgbjn\r\nughnm'),
(4, 3, 78000, 'Patin', '0000-00-00', '3', 'ini tabungan gais'),
(5, 3, 89000, 'Lele', '2017-04-30', '2', 'ini di dompet');

-- --------------------------------------------------------

--
-- Struktur dari tabel `pengeluaran`
--

CREATE TABLE `pengeluaran` (
  `id_pengeluaran` int(11) NOT NULL,
  `id_user` int(11) NOT NULL,
  `nominal` int(20) NOT NULL,
  `kategori` varchar(25) NOT NULL,
  `tanggal` date NOT NULL,
  `namapenyimpanan` varchar(20) NOT NULL,
  `catatan` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `pengeluaran`
--

INSERT INTO `pengeluaran` (`id_pengeluaran`, `id_user`, `nominal`, `kategori`, `tanggal`, `namapenyimpanan`, `catatan`) VALUES
(1, 3, 50000, 'Patin', '0000-00-00', 'Dompet', 'rwettuyewr wgryewrwrwbrjhewrew'),
(3, 0, 23000, 'Lele', '2017-04-29', 'Warung Kantin', 'srcer'),
(4, 3, 89000, 'Lele', '2017-04-30', 'Warung Kantin', 'srvergtrgrg');

-- --------------------------------------------------------

--
-- Struktur dari tabel `penyimpanan`
--

CREATE TABLE `penyimpanan` (
  `id_penyimpanan` int(11) NOT NULL,
  `id_user` int(11) NOT NULL,
  `namapenyimpanan` varchar(30) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `penyimpanan`
--

INSERT INTO `penyimpanan` (`id_penyimpanan`, `id_user`, `namapenyimpanan`) VALUES
(1, 0, 'dompet'),
(2, 3, 'dompet'),
(3, 3, 'tabungan');

-- --------------------------------------------------------

--
-- Struktur dari tabel `user`
--

CREATE TABLE `user` (
  `id_user` int(11) NOT NULL,
  `namalengkap` varchar(50) NOT NULL,
  `username` varchar(30) NOT NULL,
  `password` varchar(100) NOT NULL,
  `jenisuser` varchar(10) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data untuk tabel `user`
--

INSERT INTO `user` (`id_user`, `namalengkap`, `username`, `password`, `jenisuser`) VALUES
(1, 'user ini ya', 'user', 'ee11cbb19052e40b07aac0ca060c23ee', '0'),
(2, 'andre b', 'andre', '19984dcaea13176bbb694f62ba6b5b35', 'admin'),
(3, 'user lagi', 'user1', '24c9e15e52afc47c225b757e7bee1f9d', 'user'),
(5, 'aditya radotti', 'juple', 'e61ad86ce2888bd03094ef69e6510e8a', 'user');

--
-- Indexes for dumped tables
--

--
-- Indexes for table `keinginan`
--
ALTER TABLE `keinginan`
  ADD PRIMARY KEY (`id_keinginan`);

--
-- Indexes for table `pemasukan`
--
ALTER TABLE `pemasukan`
  ADD PRIMARY KEY (`id_pemasukan`);

--
-- Indexes for table `pengeluaran`
--
ALTER TABLE `pengeluaran`
  ADD PRIMARY KEY (`id_pengeluaran`);

--
-- Indexes for table `penyimpanan`
--
ALTER TABLE `penyimpanan`
  ADD PRIMARY KEY (`id_penyimpanan`);

--
-- Indexes for table `user`
--
ALTER TABLE `user`
  ADD PRIMARY KEY (`id_user`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `keinginan`
--
ALTER TABLE `keinginan`
  MODIFY `id_keinginan` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=3;
--
-- AUTO_INCREMENT for table `pemasukan`
--
ALTER TABLE `pemasukan`
  MODIFY `id_pemasukan` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=6;
--
-- AUTO_INCREMENT for table `pengeluaran`
--
ALTER TABLE `pengeluaran`
  MODIFY `id_pengeluaran` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=5;
--
-- AUTO_INCREMENT for table `penyimpanan`
--
ALTER TABLE `penyimpanan`
  MODIFY `id_penyimpanan` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=4;
--
-- AUTO_INCREMENT for table `user`
--
ALTER TABLE `user`
  MODIFY `id_user` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=6;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
