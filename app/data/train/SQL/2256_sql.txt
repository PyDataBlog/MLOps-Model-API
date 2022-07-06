CREATE TABLE IF NOT EXISTS `zone` (
  `zone_id` int(11) NOT NULL AUTO_INCREMENT,
  `zone_name` varchar(100) NOT NULL,
  PRIMARY KEY (`zone_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

INSERT INTO `zone` (`zone_id`, `zone_name`) VALUES
(1, 'Mechi'),
(2, 'Rapti'),
(3, 'Bagmati'),
(4, 'Karnali'),
(5, 'Sagarmatha'),
(6, 'Koshi'),
(7, 'Narayani'),
(8, 'Mahakali'),
(9, 'Gandaki'),
(10, 'Janakpur'),
(11, 'Lumbini'),
(12, 'Seti'),
(13, 'Bheri'),
(14, 'Dhawalagiri');
