-- phpMyAdmin SQL Dump
-- version 4.4.15.8
-- https://www.phpmyadmin.net
--
-- Host: localhost
-- Creato il: Lug 23, 2017 alle 18:42
-- Versione del server: 5.6.31
-- Versione PHP: 5.5.38

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `tdw`
--

-- --------------------------------------------------------

--
-- Struttura della tabella `Caffetteria`
--

CREATE TABLE IF NOT EXISTS `Caffetteria` (
  `ID_Caffetteria` int(11) NOT NULL,
  `Titolo` varchar(50) DEFAULT NULL,
  `Descrizione` text,
  `Img` varchar(100) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Caffetteria`
--

INSERT INTO `Caffetteria` (`ID_Caffetteria`, `Titolo`, `Descrizione`, `Img`) VALUES
(0, '', '', 'Img/caffetteria.jpg'),
(1, 'UNA CARICA DI ESPRESSO!', 'Lasciati trasportare dal gusto inconfondibile del caffè Faraglia, l''unico in grado di farti iniziare la giornata nel migliore dei modi!\r\nVieni ad assaporare la genuinità!', 'Img/caffe-faraglia.jpg'),
(2, 'FAI COLAZIONE INSIEME A NOI!', 'Non lasciarti sfuggire l''occasione! Qui da noi la colazione sarà un vero piacere! Lasciatevi tentare dal profumo dei nostri cornetti appena sfornati, e dalla vista di un ottimo cappuccino preparato per voi dalla mano esperta dei nostri maestri caffettieri!', 'Img/colazione.jpg'),
(3, 'IL MEGLIO DEVE ANCORA ARRIVARE!', 'Che sia in tarda mattinata o in tardo pomeriggio, un aperitivo in compagnia è sempre una gran festa! Venite a provare la vasta gamma di aperitivi e cocktail che è in grado di preparare il nostro personale altamente qualificato! Ovviamente il tutto accompagnato da buona musica e cibo genuino!', 'Img/slide-aperitivo.jpg');

-- --------------------------------------------------------

--
-- Struttura della tabella `Contatti`
--

CREATE TABLE IF NOT EXISTS `Contatti` (
  `ID_Contatti` int(11) NOT NULL,
  `Titolo` varchar(50) NOT NULL,
  `Indirizzo` varchar(100) NOT NULL,
  `Tel` varchar(20) NOT NULL,
  `Fax` varchar(20) DEFAULT NULL,
  `Email` varchar(100) NOT NULL,
  `Descrizione` text,
  `Img` varchar(100) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Contatti`
--

INSERT INTO `Contatti` (`ID_Contatti`, `Titolo`, `Indirizzo`, `Tel`, `Fax`, `Email`, `Descrizione`, `Img`) VALUES
(1, 'Siamo Felici Di Ascoltarti!', '49 Chigwell Road - South Woodford, London E18 1NG - United Kingdom', '(020) 8989 3831', '(020) 8989 38 32', 'info@toniandtony.it', 'Siamo Aperti Per Voi!', 'Img/contact.jpg');

-- --------------------------------------------------------

--
-- Struttura della tabella `Evento`
--

CREATE TABLE IF NOT EXISTS `Evento` (
  `ID_Evento` int(11) NOT NULL,
  `Data` date DEFAULT NULL,
  `Nome` varchar(50) DEFAULT NULL,
  `Descrizione` text,
  `Img` varchar(100) DEFAULT NULL
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Evento`
--

INSERT INTO `Evento` (`ID_Evento`, `Data`, `Nome`, `Descrizione`, `Img`) VALUES
(0, '0000-00-00', 'ABBIAMO MOLTO IN SERBO PER VOI!!!', 'Ogni anno organizziamo molti eventi di diversa natura. Che siano per una festività, per una giornata importante o anche semplicemente per divertimento, solo una cosa è certa: il tutto sarà accompagnato dalle nostre prelibatezze e dalla buona musica.\r\n', 'Img/evento.jpg'),
(1, '0000-00-00', '', '', 'Img/eventospecial.jpg'),
(2, '2017-07-28', 'Birthday', 'E'' il mio compleanno!', 'Img/cheers.jpg'),
(3, '2017-12-25', 'Natale', 'E'' Natale a casa nostra!', 'Img/natale.jpg'),
(4, '2017-04-16', 'Pasqua', 'Anche da noi è Pasqua!', 'Img/pasqua.jpg'),
(5, '2017-12-25', 'Free Pizza', 'Pizza per tutti!', 'Img/pizza.jpg'),
(6, '2017-10-18', 'Happy Hour!', 'Dalle 18 alle 21!', 'Img/happyhour.jpg'),
(7, '2017-12-31', 'Caapodanno', 'Festeggiamo insieme!', 'Img/capodanno04.jpg'),
(8, '2017-08-15', 'Barbeque', 'Carne alla brace per tutti!', 'Img/For-Barbeque-Story-2_ThinkstockPhotos-531464020-4.jpg');

-- --------------------------------------------------------

--
-- Struttura della tabella `Faq`
--

CREATE TABLE IF NOT EXISTS `Faq` (
  `ID_FAQ` int(11) NOT NULL,
  `Domanda` text,
  `Risposta` text,
  `Img` varchar(100) DEFAULT NULL
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Faq`
--

INSERT INTO `Faq` (`ID_FAQ`, `Domanda`, `Risposta`, `Img`) VALUES
(0, NULL, NULL, 'Img/faq.jpg'),
(2, 'Siete attrezzati per preparare piatti senza glutine?', 'Si, nella nostra cucina abbiamo un locale apposito dedicato alla preparazione dei piatti senza glutine. Ovviamente anche la pizza!', NULL),
(3, 'Fate pizza da asporto?', 'Si, ma ve la venite a prendere!', NULL),
(4, 'Ci sono accessi per le persone disabili?', 'Si, il nostro locale è privo di barriere architettoniche. Siamo preparati per qualsiasi evenienza!', NULL);

-- --------------------------------------------------------

--
-- Struttura della tabella `Footer`
--

CREATE TABLE IF NOT EXISTS `Footer` (
  `ID_Foot` int(11) NOT NULL,
  `Titoletto` varchar(50) DEFAULT NULL,
  `Indirizzo` varchar(100) DEFAULT NULL,
  `Tel` varchar(20) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Footer`
--

INSERT INTO `Footer` (`ID_Foot`, `Titoletto`, `Indirizzo`, `Tel`) VALUES
(1, 'bar, bistro & restaurant', '41126 Oxford Road - england', '44 023 642 124');

-- --------------------------------------------------------

--
-- Struttura della tabella `Gallery`
--

CREATE TABLE IF NOT EXISTS `Gallery` (
  `ID_Gallery` int(11) NOT NULL,
  `Img` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Gallery`
--

INSERT INTO `Gallery` (`ID_Gallery`, `Img`) VALUES
(7, 'Img/Gallery/cappuccino.jpg'),
(9, 'Img/Gallery/cheesecake.jpg'),
(1, 'Img/Gallery/colazione.jpg'),
(15, 'Img/Gallery/dolci.jpg'),
(6, 'Img/Gallery/drink1.jpg'),
(14, 'Img/Gallery/drink2.jpg'),
(0, 'Img/Gallery/gallery.jpg'),
(12, 'Img/Gallery/panna-cotta.jpg'),
(10, 'Img/Gallery/pasta.jpg'),
(3, 'Img/Gallery/pesto.jpg'),
(8, 'Img/Gallery/pizza1.jpg'),
(13, 'Img/Gallery/pizza2.jpg'),
(2, 'Img/Gallery/profitteroles.jpg'),
(5, 'Img/Gallery/tagliata.jpg'),
(11, 'Img/Gallery/tappi.jpg'),
(4, 'Img/Gallery/tortafrutta.jpg');

-- --------------------------------------------------------

--
-- Struttura della tabella `groups`
--

CREATE TABLE IF NOT EXISTS `groups` (
  `id` int(11) NOT NULL,
  `name` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `groups`
--

INSERT INTO `groups` (`id`, `name`) VALUES
(1, 'adminer');

-- --------------------------------------------------------

--
-- Struttura della tabella `groupservices`
--

CREATE TABLE IF NOT EXISTS `groupservices` (
  `script` varchar(255) NOT NULL DEFAULT '',
  `id` int(11) NOT NULL DEFAULT '0'
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `groupservices`
--

INSERT INTO `groupservices` (`script`, `id`) VALUES
('admin.php', 1),
('array_active.php', 1),
('array_add.php', 1),
('array_delete.php', 1),
('array_manager.php', 1),
('caffetteria_manager.php', 1),
('caffetteria_update.php', 1),
('contatti_manager.php', 1),
('contatti_update.php', 1),
('dashboard.php', 1),
('eventi_add.php', 1),
('eventi_manager.php', 1),
('eventi_update.php', 1),
('faq_add.php', 1),
('faq_manager.php', 1),
('faq_update.php', 1),
('footer_update.php', 1),
('gallery_manager.php', 1),
('gallery_update.php', 1),
('header_manager.php', 1),
('header_update.php', 1),
('home_manager.php', 1),
('home_update.php', 1),
('icona_update.php', 1),
('img_grande_update.php', 1),
('lgallery_manager.php', 1),
('lgallery_update.php', 1),
('logo_update.php', 1),
('orario_manager.php', 1),
('orario_update.php', 1),
('piatti_add.php', 1),
('piatti_manager.php', 1),
('piatti_update.php', 1),
('recensioni_manager.php', 1),
('recensioni_update.php', 1),
('reservation_manager.php', 1),
('reservation_update.php', 1),
('ristorante_manager.php', 1),
('ristorante_update.php', 1),
('slider_manager.php', 1),
('slider_update.php', 1),
('story_manager.php', 1),
('story_update.php', 1),
('upload.php', 1),
('upload_in.php', 1),
('utenti_add.php', 1),
('utenti_manager.php', 1),
('utenti_update.php', 1),
('vMessaggi.php', 1),
('vNewsletter.php', 1),
('vPrenotazione.php', 1);

-- --------------------------------------------------------

--
-- Struttura della tabella `Header`
--

CREATE TABLE IF NOT EXISTS `Header` (
  `ID_Header` int(11) NOT NULL,
  `Nome` varchar(20) NOT NULL,
  `Link` varchar(100) NOT NULL,
  `Posizione` varchar(2) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Header`
--

INSERT INTO `Header` (`ID_Header`, `Nome`, `Link`, `Posizione`) VALUES
(1, 'Home', 'home.php', 'Sx'),
(2, 'Storia', 'story.php', 'Sx'),
(3, 'Ristorante', 'rist_pizz.php', 'Sx'),
(4, 'Caffetteria', 'caffetteria.php', 'Sx'),
(5, 'Prenotazione', 'reservation.php', 'Dx'),
(6, 'Eventi', 'events.php', 'Dx'),
(7, 'Gallery', 'photo.php', 'Dx'),
(8, 'Contatti', 'contact.php', 'Dx');

-- --------------------------------------------------------

--
-- Struttura della tabella `Home`
--

CREATE TABLE IF NOT EXISTS `Home` (
  `ID_Home` int(11) NOT NULL,
  `Titolo` varchar(50) DEFAULT NULL,
  `Descrizione` text,
  `Img` varchar(100) DEFAULT NULL,
  `Icona` varchar(100) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Home`
--

INSERT INTO `Home` (`ID_Home`, `Titolo`, `Descrizione`, `Img`, `Icona`) VALUES
(0, '', '', 'Img/aperitivolungo.jpg', NULL),
(1, 'Chi Siamo Noi?', 'Siamo due giovani ragazzi che hanno creduto in un sogno: creare un luogo di ritrovo per tutti, dove poter mangiare, divertirsi e al contempo ascoltare buona musica!', 'Img/siamonoi.jpg', 'Img/Loghi/logopiccolo.ico'),
(2, 'Il Nostro Obiettivo', 'Vogliamo realizzare qualcosa di grande, rivoluzionare il mondo della ristorazione, rimanendo però legati alla cucina tradizionale.', 'Img/lasagne.jpg', 'Img/Loghi/logotony.png');

-- --------------------------------------------------------

--
-- Struttura della tabella `Immagine`
--

CREATE TABLE IF NOT EXISTS `Immagine` (
  `Link` varchar(100) NOT NULL,
  `Attivo` tinyint(4) DEFAULT '0'
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Immagine`
--

INSERT INTO `Immagine` (`Link`, `Attivo`) VALUES
('Img/adrian-gemuztraminer.jpg', 0),
('Img/aperitivo7.jpg', 0),
('Img/aperitivolungo.jpg', 0),
('Img/avatars/avatar-1.jpg', 0),
('Img/avatars/avatar-2.jpg', 0),
('Img/avatars/avatar-3.jpg', 0),
('Img/caffe-faraglia.jpg', 0),
('Img/caffetteria.jpg', 0),
('Img/capodanno04.jpg', 0),
('Img/carote.jpg', 0),
('Img/cheers.jpg', 0),
('Img/colazione.jpg', 0),
('Img/contact.jpg', 0),
('Img/desserts/1.jpg', 1),
('Img/desserts/2.jpg', 0),
('Img/desserts/3.jpg', 0),
('Img/desserts/4.jpg', 0),
('Img/desserts/5.jpg', 0),
('Img/desserts/6.jpg', 0),
('Img/desserts/7.jpg', 0),
('Img/drinks/1.jpg', 0),
('Img/drinks/2.jpg', 0),
('Img/drinks/3.jpg', 0),
('Img/drinks/4.jpg', 0),
('Img/drinks/5.jpg', 0),
('Img/drinks/6.jpg', 1),
('Img/drinks/7.jpg', 0),
('Img/evento.jpg', 0),
('Img/eventospecial.jpg', 0),
('Img/faq.jpg', 0),
('Img/FILE-BLOG-2.jpg', 0),
('Img/food/1.jpg', 0),
('Img/food/2.jpg', 0),
('Img/food/3.jpg', 0),
('Img/food/4.jpg', 0),
('Img/food/5.jpg', 1),
('Img/food/6.jpg', 0),
('Img/food/7.jpg', 0),
('Img/For-Barbeque-Story-2_ThinkstockPhotos-531464020-4.jpg', 0),
('Img/Gallery/cappuccino.jpg', 0),
('Img/Gallery/cheesecake.jpg', 0),
('Img/Gallery/colazione.jpg', 0),
('Img/Gallery/dolci.jpg', 0),
('Img/Gallery/drink1.jpg', 0),
('Img/Gallery/drink2.jpg', 0),
('Img/Gallery/gallery.jpg', 0),
('Img/Gallery/panna-cotta.jpg', 0),
('Img/Gallery/pasta.jpg', 0),
('Img/Gallery/pesto.jpg', 0),
('Img/Gallery/pizza1.jpg', 0),
('Img/Gallery/pizza2.jpg', 0),
('Img/Gallery/profitteroles.jpg', 0),
('Img/Gallery/tagliata.jpg', 0),
('Img/Gallery/tappi.jpg', 0),
('Img/Gallery/tortafrutta.jpg', 0),
('Img/happyhour.jpg', 0),
('Img/hd450x300.jpg', 0),
('Img/impasto.jpg', 0),
('Img/insalatona2.jpg', 0),
('Img/lasagne.jpg', 0),
('Img/LGallery/1.jpg', 0),
('Img/LGallery/2.jpg', 0),
('Img/LGallery/3.jpg', 0),
('Img/LGallery/4.jpg', 0),
('Img/LGallery/5.jpg', 0),
('Img/LGallery/6.jpg', 0),
('Img/Loghi/bestlogo.png', 0),
('Img/Loghi/logopiccolo.ico', 0),
('Img/Loghi/logotony.png', 0),
('Img/natale.jpg', 0),
('Img/pasqua.jpg', 0),
('Img/pesto.jpg', 0),
('Img/pizza bella.jpg', 0),
('Img/pizza.jpg', 0),
('Img/pizza2.jpg', 0),
('Img/reservation.jpg', 0),
('Img/ristpizz.jpg', 0),
('Img/semagna.jpg', 0),
('Img/siamonoi.jpg', 0),
('Img/slide-aperitivo.jpg', 0),
('Img/slidercc.jpg', 0),
('Img/story.jpg', 0),
('Img/story2.jpg', 0),
('Img/story3.jpg', 0),
('Img/tagliata.jpg', 0),
('Img/tortafrutta.jpg', 0),
('Img/tricolore.jpg', 0);

-- --------------------------------------------------------

--
-- Struttura della tabella `Logo`
--

CREATE TABLE IF NOT EXISTS `Logo` (
  `Logo` varchar(100) NOT NULL,
  `Path` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Logo`
--

INSERT INTO `Logo` (`Logo`, `Path`) VALUES
('Img/Loghi/bestlogo.png', 'home.php');

-- --------------------------------------------------------

--
-- Struttura della tabella `Messaggi`
--

CREATE TABLE IF NOT EXISTS `Messaggi` (
  `ID_Messaggio` int(11) NOT NULL,
  `Nome` text NOT NULL,
  `Phone` varchar(20) DEFAULT NULL,
  `Email` varchar(100) NOT NULL,
  `Message` text NOT NULL
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Messaggi`
--

INSERT INTO `Messaggi` (`ID_Messaggio`, `Nome`, `Phone`, `Email`, `Message`) VALUES
(2, 'Alberto Castagna', '0214283526', 'info@toniandtony.it', 'Continuate così!');

-- --------------------------------------------------------

--
-- Struttura della tabella `Newsletter`
--

CREATE TABLE IF NOT EXISTS `Newsletter` (
  `ID_Mail` int(11) NOT NULL,
  `Email` varchar(100) NOT NULL,
  `Data` date NOT NULL
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Newsletter`
--

INSERT INTO `Newsletter` (`ID_Mail`, `Email`, `Data`) VALUES
(1, 'cstagna@alberto.com', '2017-07-23');

-- --------------------------------------------------------

--
-- Struttura della tabella `Orario`
--

CREATE TABLE IF NOT EXISTS `Orario` (
  `ID_Giorno` int(11) NOT NULL,
  `Giorno` varchar(10) NOT NULL,
  `Orario` text NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Orario`
--

INSERT INTO `Orario` (`ID_Giorno`, `Giorno`, `Orario`) VALUES
(1, 'Lunedì', '8:30 - 15:30 / 18:00 - 01:00'),
(2, 'Martedì', '8:30 - 15:30 / 18:00 - 01:00'),
(3, 'Mercoledì', 'Chiuso'),
(4, 'Giovedì', '8:30 - 15:30 / 18:00 - 01:00'),
(5, 'Venerdì', '8:30 - 15:30 / 18:00 - 01:00'),
(6, 'Sabato', '8:30 - 15:30 / 18:00 - 01:00'),
(7, 'Domenica', '10:30 - 15:30 / 18:00 - 01:00');

-- --------------------------------------------------------

--
-- Struttura della tabella `Piatto`
--

CREATE TABLE IF NOT EXISTS `Piatto` (
  `Nome_Piatto` varchar(30) NOT NULL,
  `Descrizione` text NOT NULL,
  `Ingredienti` text NOT NULL,
  `Prezzo` decimal(8,2) NOT NULL,
  `Img` varchar(100) NOT NULL,
  `tipo_piatto` varchar(20) NOT NULL,
  `id` int(11) NOT NULL
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Piatto`
--

INSERT INTO `Piatto` (`Nome_Piatto`, `Descrizione`, `Ingredienti`, `Prezzo`, `Img`, `tipo_piatto`, `id`) VALUES
('Carbonara', 'Classica carbonara', 'Uovo, Guanciale, Pecorino, Pepe', '6.00', 'Img/hd450x300.jpg', 'primo', 1),
('Gewurtztraminer', 'Ottimo vino bianco da gustare con il pesce', 'Uva di Traminer', '15.00', 'Img/adrian-gemuztraminer.jpg', 'drink', 2),
('Insalatona', 'Una bella insalata salutare', 'Insalata, Pomodori, Olive, Cipolla, Cetrioli', '5.00', 'Img/insalatona2.jpg', 'contorno', 3),
('Pasta Al Pesto', 'Pasta con pesto fatto in casa', 'Basilico, Pinoli, Olio', '6.00', 'Img/pesto.jpg', 'primo', 4),
('Pizza Margherita', 'La Classica Immortale', 'Pomodoro, Mozzarella, Basilico', '4.50', 'Img/pizza bella.jpg', 'pizza', 5),
('Tagliata', 'Ottima Tagliata in Agrodolce', 'Tagliata, Rucola, Sciroppo di aceto balsamico', '10.00', 'Img/tagliata.jpg', 'secondo', 6),
('Torta Di Frutta', 'Torta di frutta fresca', 'Kiwi, Fragole, Frutti di bosco', '5.00', 'Img/tortafrutta.jpg', 'dessert', 7);

-- --------------------------------------------------------

--
-- Struttura della tabella `Prenotazione`
--

CREATE TABLE IF NOT EXISTS `Prenotazione` (
  `ID_Prenotazione` int(11) NOT NULL,
  `Data` date NOT NULL,
  `Persone` int(11) NOT NULL,
  `Time` time NOT NULL,
  `Nome_Cl` varchar(50) NOT NULL,
  `Tel` varchar(20) NOT NULL,
  `Email` varchar(100) DEFAULT NULL,
  `Notice` text
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Prenotazione`
--

INSERT INTO `Prenotazione` (`ID_Prenotazione`, `Data`, `Persone`, `Time`, `Nome_Cl`, `Tel`, `Email`, `Notice`) VALUES
(1, '2017-07-12', 10, '20:00:00', 'Alberto Castagna', '0214283526', 'cstagna@alberto.com', 'Veniamo a cena!');

-- --------------------------------------------------------

--
-- Struttura della tabella `Presentazione_Reservation`
--

CREATE TABLE IF NOT EXISTS `Presentazione_Reservation` (
  `Titolo` varchar(50) NOT NULL,
  `Descrizione` text,
  `Img` varchar(100) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Presentazione_Reservation`
--

INSERT INTO `Presentazione_Reservation` (`Titolo`, `Descrizione`, `Img`) VALUES
('Venite e Godete della Nostra Ospitalità!', 'La maggior parte dei nostri tavoli non è prenotata, così noi possiamo sistemarvi nel luogo che preferite. Potreste dover aspettare purtoppo, ma, attraverso una prenotazione, ovvieremmo a questo problema.', 'Img/reservation.jpg');

-- --------------------------------------------------------

--
-- Struttura della tabella `Recensione`
--

CREATE TABLE IF NOT EXISTS `Recensione` (
  `ID_Recensione` int(11) NOT NULL,
  `Nome` varchar(50) NOT NULL,
  `Avatar` varchar(100) NOT NULL,
  `Testo` text NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Recensione`
--

INSERT INTO `Recensione` (`ID_Recensione`, `Nome`, `Avatar`, `Testo`) VALUES
(1, 'Mario Bianchi', 'Img/avatars/avatar-1.jpg', 'Quando sono arrivato in città, la prima cosa che mi è saltata all''occhio è stato questo locale e il suo interessante menù. Questo è il tipo di locale in cui vorresti tornare.'),
(2, 'Marco Rossi', 'Img/avatars/avatar-2.jpg', 'Voglio scrivere per ringraziarvi della magnifica esperienza e per la deliziosa cena di stasera. Avete fatto sentire me e mia moglie come fossimo a casa. I migliori auguri,\r\nMarco Rossi'),
(3, 'Luisa Fiordaponti', 'Img/avatars/avatar-3.jpg', 'Voglio ringraziarvi per il magnifico compleanno passato nel vostro locale. Il vostro staff ha fatto un lavoro eccellente, e il cibo era superbo. Vi raccomanderò ai miei amici. Grazie ancora.');

-- --------------------------------------------------------

--
-- Struttura della tabella `Rist_Pizz`
--

CREATE TABLE IF NOT EXISTS `Rist_Pizz` (
  `ID_Rist_Pizz` int(11) NOT NULL,
  `Titolo` text,
  `Descrizione` text,
  `Img` varchar(100) DEFAULT NULL,
  `Categoria` varchar(20) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Rist_Pizz`
--

INSERT INTO `Rist_Pizz` (`ID_Rist_Pizz`, `Titolo`, `Descrizione`, `Img`, `Categoria`) VALUES
(0, '', '', 'Img/ristpizz.jpg', NULL),
(1, 'Vieni ad assaporare la genuinità!', 'I nostri chef utilizzano solo ingredienti naturali di primissima qualità, forniti da persone affidabili e che mettono il cuore nel loro lavoro!', 'Img/carote.jpg', 'ristorante'),
(2, 'La Cucina', 'I nostri piatti sono ineguagliabili! I nostri chef hanno una fantasia incredibile e creano nuovi piatti sani e genuini ogni giorno, senza però rinunciare a lasciare la clientela senza parole per il sapore squisito!', 'Img/tricolore.jpg', 'ristorante'),
(3, 'La Nostra Pizza', 'Si, facciamo anche la pizza! Ma, soprattutto, la impastiamo a mano! La nostra pizza è preparata soltanto con ingredienti naturali, senza additivi ed è cotta nel forno a legna!', 'Img/impasto.jpg', 'pizzeria'),
(4, 'La Facciamo Come Vuoi Tu!', 'Il nostro personale si prenderà cura di te in ogni singolo istante e i nostri mastri pizzaioli si impegneranno ad esaudire i tuoi desideri!', 'Img/pizza bella.jpg', 'pizzeria');

-- --------------------------------------------------------

--
-- Struttura della tabella `services`
--

CREATE TABLE IF NOT EXISTS `services` (
  `script` varchar(255) NOT NULL,
  `description` text
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `services`
--

INSERT INTO `services` (`script`, `description`) VALUES
('admin.php', 'script dashboard admin'),
('array_active.php', NULL),
('array_add.php', NULL),
('array_delete.php', NULL),
('array_manager.php', NULL),
('caffetteria_manager.php', NULL),
('caffetteria_update.php', NULL),
('contatti_manager.php', NULL),
('contatti_update.php', NULL),
('dashboard.php', NULL),
('eventi_add.php', NULL),
('eventi_manager.php', NULL),
('eventi_update.php', NULL),
('faq_add.php', NULL),
('faq_manager.php', NULL),
('faq_update.php', NULL),
('footer_update.php', NULL),
('gallery_manager.php', NULL),
('gallery_update.php', NULL),
('header_manager.php', NULL),
('header_update.php', NULL),
('home_manager.php', NULL),
('home_update.php', NULL),
('icona_update.php', NULL),
('img_grande_update.php', NULL),
('lgallery_manager.php', NULL),
('lgallery_update.php', NULL),
('logo_update.php', NULL),
('orario_manager.php', NULL),
('orario_update.php', NULL),
('piatti_add.php', NULL),
('piatti_manager.php', NULL),
('piatti_update.php', NULL),
('recensioni_manager.php', NULL),
('recensioni_update.php', NULL),
('reservation_manager.php', NULL),
('reservation_update.php', NULL),
('ristorante_manager.php', NULL),
('ristorante_update.php', NULL),
('slider_manager.php', NULL),
('slider_update.php', NULL),
('story_manager.php', NULL),
('story_update.php', NULL),
('upload.php', NULL),
('upload_in.php', NULL),
('utenti_add.php', NULL),
('utenti_manager.php', NULL),
('utenti_update.php', NULL),
('vMessaggi.php', NULL),
('vNewsletter.php', NULL),
('vPrenotazione.php', NULL);

-- --------------------------------------------------------

--
-- Struttura della tabella `Slider`
--

CREATE TABLE IF NOT EXISTS `Slider` (
  `ID_Slider` int(11) NOT NULL,
  `Img` varchar(100) NOT NULL,
  `SliderTitle` varchar(50) NOT NULL,
  `SliderText` text NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Slider`
--

INSERT INTO `Slider` (`ID_Slider`, `Img`, `SliderTitle`, `SliderText`) VALUES
(1, 'Img/semagna.jpg', 'Benvenuti Nel Nostro Sogno!', 'Qua "se magna, se beve e se canta"!!'),
(2, 'Img/FILE-BLOG-2.jpg', 'Aperitivi In Compagnia!', 'Vieni a gustare i nostri magnifici aperitivi, accompagnati dai migliori snack!'),
(3, 'Img/pizza2.jpg', 'La Nostra Pizza', 'Cotta nel forno a legna!');

-- --------------------------------------------------------

--
-- Struttura della tabella `Storia`
--

CREATE TABLE IF NOT EXISTS `Storia` (
  `ID_Storia` int(11) NOT NULL,
  `Titolo` varchar(50) DEFAULT NULL,
  `Descrizione` text,
  `Img` varchar(100) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `Storia`
--

INSERT INTO `Storia` (`ID_Storia`, `Titolo`, `Descrizione`, `Img`) VALUES
(0, '', '', 'Img/story.jpg'),
(1, 'L''Inizio', 'Due giovani imprenditori vivono in una città spenta e, un giorno, decidono di aprire un loro locale nella speranza di vedere, un giorno, la loro città risorgere.', 'Img/story2.jpg'),
(2, 'Il Sogno Si Avvera', 'Dopo anni di duro lavoro, il locale divenne un punto di riferimento nella città, per incontri di affari, serate tra amici, feste e molto altro!', 'Img/slidercc.jpg'),
(3, 'I Giovani Ragazzi Fanno Esperienza', 'Da quel giorno, i ragazzi si rimboccarono le maniche e crearono un menu su misura di cliente, tentando sempre di migliorare e aggiungere nuove ricette sempre più naturali.', 'Img/story3.jpg'),
(4, 'Oggi', 'Oggi Toni&Tony è un''autorità nel campo della ristorazione a livello nazionale, tanto da essere oggetto di articoli su riviste importanti del settore.', 'Img/aperitivo7.jpg');

-- --------------------------------------------------------

--
-- Struttura della tabella `usergroups`
--

CREATE TABLE IF NOT EXISTS `usergroups` (
  `username` varchar(30) NOT NULL DEFAULT '',
  `id_group` int(11) NOT NULL DEFAULT '0'
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `usergroups`
--

INSERT INTO `usergroups` (`username`, `id_group`) VALUES
('admin', 1);

-- --------------------------------------------------------

--
-- Struttura della tabella `users`
--

CREATE TABLE IF NOT EXISTS `users` (
  `username` varchar(30) NOT NULL,
  `password` varchar(32) NOT NULL,
  `id` int(11) NOT NULL
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;

--
-- Dump dei dati per la tabella `users`
--

INSERT INTO `users` (`username`, `password`, `id`) VALUES
('admin', '21232f297a57a5a743894a0e4a801fc3', 1);

--
-- Indici per le tabelle scaricate
--

--
-- Indici per le tabelle `Caffetteria`
--
ALTER TABLE `Caffetteria`
  ADD PRIMARY KEY (`ID_Caffetteria`),
  ADD KEY `Img` (`Img`);

--
-- Indici per le tabelle `Contatti`
--
ALTER TABLE `Contatti`
  ADD PRIMARY KEY (`ID_Contatti`),
  ADD KEY `Img` (`Img`);

--
-- Indici per le tabelle `Evento`
--
ALTER TABLE `Evento`
  ADD PRIMARY KEY (`ID_Evento`),
  ADD KEY `Img` (`Img`);

--
-- Indici per le tabelle `Faq`
--
ALTER TABLE `Faq`
  ADD PRIMARY KEY (`ID_FAQ`),
  ADD KEY `Img` (`Img`);

--
-- Indici per le tabelle `Footer`
--
ALTER TABLE `Footer`
  ADD PRIMARY KEY (`ID_Foot`);

--
-- Indici per le tabelle `Gallery`
--
ALTER TABLE `Gallery`
  ADD PRIMARY KEY (`ID_Gallery`),
  ADD KEY `Img` (`Img`);

--
-- Indici per le tabelle `groups`
--
ALTER TABLE `groups`
  ADD PRIMARY KEY (`id`);

--
-- Indici per le tabelle `groupservices`
--
ALTER TABLE `groupservices`
  ADD PRIMARY KEY (`script`,`id`),
  ADD KEY `id` (`id`);

--
-- Indici per le tabelle `Header`
--
ALTER TABLE `Header`
  ADD PRIMARY KEY (`ID_Header`);

--
-- Indici per le tabelle `Home`
--
ALTER TABLE `Home`
  ADD PRIMARY KEY (`ID_Home`),
  ADD KEY `Img` (`Img`),
  ADD KEY `Icona` (`Icona`);

--
-- Indici per le tabelle `Immagine`
--
ALTER TABLE `Immagine`
  ADD PRIMARY KEY (`Link`);

--
-- Indici per le tabelle `Logo`
--
ALTER TABLE `Logo`
  ADD PRIMARY KEY (`Logo`);

--
-- Indici per le tabelle `Messaggi`
--
ALTER TABLE `Messaggi`
  ADD PRIMARY KEY (`ID_Messaggio`);

--
-- Indici per le tabelle `Newsletter`
--
ALTER TABLE `Newsletter`
  ADD PRIMARY KEY (`ID_Mail`);

--
-- Indici per le tabelle `Orario`
--
ALTER TABLE `Orario`
  ADD PRIMARY KEY (`ID_Giorno`);

--
-- Indici per le tabelle `Piatto`
--
ALTER TABLE `Piatto`
  ADD PRIMARY KEY (`Nome_Piatto`),
  ADD UNIQUE KEY `id` (`id`),
  ADD KEY `Img` (`Img`);

--
-- Indici per le tabelle `Prenotazione`
--
ALTER TABLE `Prenotazione`
  ADD PRIMARY KEY (`ID_Prenotazione`);

--
-- Indici per le tabelle `Presentazione_Reservation`
--
ALTER TABLE `Presentazione_Reservation`
  ADD PRIMARY KEY (`Titolo`),
  ADD KEY `Img` (`Img`);

--
-- Indici per le tabelle `Recensione`
--
ALTER TABLE `Recensione`
  ADD PRIMARY KEY (`ID_Recensione`),
  ADD KEY `Avatar` (`Avatar`);

--
-- Indici per le tabelle `Rist_Pizz`
--
ALTER TABLE `Rist_Pizz`
  ADD PRIMARY KEY (`ID_Rist_Pizz`),
  ADD KEY `Img` (`Img`);

--
-- Indici per le tabelle `services`
--
ALTER TABLE `services`
  ADD PRIMARY KEY (`script`);

--
-- Indici per le tabelle `Slider`
--
ALTER TABLE `Slider`
  ADD PRIMARY KEY (`ID_Slider`),
  ADD KEY `Img` (`Img`);

--
-- Indici per le tabelle `Storia`
--
ALTER TABLE `Storia`
  ADD PRIMARY KEY (`ID_Storia`),
  ADD KEY `Img` (`Img`);

--
-- Indici per le tabelle `usergroups`
--
ALTER TABLE `usergroups`
  ADD PRIMARY KEY (`username`,`id_group`),
  ADD KEY `id_group` (`id_group`);

--
-- Indici per le tabelle `users`
--
ALTER TABLE `users`
  ADD PRIMARY KEY (`username`),
  ADD UNIQUE KEY `id` (`id`);

--
-- AUTO_INCREMENT per le tabelle scaricate
--

--
-- AUTO_INCREMENT per la tabella `Evento`
--
ALTER TABLE `Evento`
  MODIFY `ID_Evento` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=9;
--
-- AUTO_INCREMENT per la tabella `Faq`
--
ALTER TABLE `Faq`
  MODIFY `ID_FAQ` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=5;
--
-- AUTO_INCREMENT per la tabella `Messaggi`
--
ALTER TABLE `Messaggi`
  MODIFY `ID_Messaggio` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=3;
--
-- AUTO_INCREMENT per la tabella `Newsletter`
--
ALTER TABLE `Newsletter`
  MODIFY `ID_Mail` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=2;
--
-- AUTO_INCREMENT per la tabella `Piatto`
--
ALTER TABLE `Piatto`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=8;
--
-- AUTO_INCREMENT per la tabella `Prenotazione`
--
ALTER TABLE `Prenotazione`
  MODIFY `ID_Prenotazione` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=2;
--
-- AUTO_INCREMENT per la tabella `users`
--
ALTER TABLE `users`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=2;
--
-- Limiti per le tabelle scaricate
--

--
-- Limiti per la tabella `Caffetteria`
--
ALTER TABLE `Caffetteria`
  ADD CONSTRAINT `caffetteria_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Contatti`
--
ALTER TABLE `Contatti`
  ADD CONSTRAINT `contatti_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Evento`
--
ALTER TABLE `Evento`
  ADD CONSTRAINT `evento_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Faq`
--
ALTER TABLE `Faq`
  ADD CONSTRAINT `faq_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Gallery`
--
ALTER TABLE `Gallery`
  ADD CONSTRAINT `gallery_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `groupservices`
--
ALTER TABLE `groupservices`
  ADD CONSTRAINT `groupservices_ibfk_1` FOREIGN KEY (`script`) REFERENCES `services` (`script`),
  ADD CONSTRAINT `groupservices_ibfk_2` FOREIGN KEY (`id`) REFERENCES `groups` (`id`);

--
-- Limiti per la tabella `Home`
--
ALTER TABLE `Home`
  ADD CONSTRAINT `home_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`),
  ADD CONSTRAINT `home_ibfk_2` FOREIGN KEY (`Icona`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Logo`
--
ALTER TABLE `Logo`
  ADD CONSTRAINT `logo_ibfk_1` FOREIGN KEY (`Logo`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Piatto`
--
ALTER TABLE `Piatto`
  ADD CONSTRAINT `piatto_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Presentazione_Reservation`
--
ALTER TABLE `Presentazione_Reservation`
  ADD CONSTRAINT `presentazione_reservation_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Recensione`
--
ALTER TABLE `Recensione`
  ADD CONSTRAINT `recensione_ibfk_1` FOREIGN KEY (`Avatar`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Rist_Pizz`
--
ALTER TABLE `Rist_Pizz`
  ADD CONSTRAINT `rist_pizz_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Slider`
--
ALTER TABLE `Slider`
  ADD CONSTRAINT `slider_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `Storia`
--
ALTER TABLE `Storia`
  ADD CONSTRAINT `storia_ibfk_1` FOREIGN KEY (`Img`) REFERENCES `Immagine` (`Link`);

--
-- Limiti per la tabella `usergroups`
--
ALTER TABLE `usergroups`
  ADD CONSTRAINT `usergroups_ibfk_1` FOREIGN KEY (`username`) REFERENCES `users` (`username`),
  ADD CONSTRAINT `usergroups_ibfk_2` FOREIGN KEY (`id_group`) REFERENCES `groups` (`id`);

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
