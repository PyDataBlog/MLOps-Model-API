<?php
/**
 * La configuration de base de votre installation WordPress.
 *
 * Le script de création wp-config.php utilise ce fichier lors de l'installation.
 * Vous n'avez pas à utiliser l'interface web, vous pouvez directement
 * renommer ce fichier en "wp-config.php" et remplir les variables à la main.
 * 
 * Ce fichier contient les configurations suivantes :
 * 
 * * réglages MySQL ;
 * * clefs secrètes ;
 * * préfixe de tables de la base de données ;
 * * ABSPATH.
 * 
 * @link https://codex.wordpress.org/Editing_wp-config.php 
 * 
 * @package WordPress
 */

// ** Réglages MySQL - Votre hébergeur doit vous fournir ces informations. ** //
/** Nom de la base de données de WordPress. */

// ** RÃ©glages MySQL - Votre hÃ©bergeur doit vous fournir ces informations. ** //
/** Nom de la base de donnÃ©es de WordPress. */
define('DB_NAME', 'wordpress');

/** Utilisateur de la base de donnÃ©es MySQL. */
define('DB_USER', 'root');

/** Mot de passe de la base de donnÃ©es MySQL. */
define('DB_PASSWORD', '');

/** Adresse de l'hébergement MySQL. */
define('DB_HOST', 'localhost');

/** Jeu de caractères à utiliser par la base de données lors de la création des tables. */
define('DB_CHARSET', 'utf8mb4');

/** Type de collation de la base de données. 
  * N'y touchez que si vous savez ce que vous faites. 
  */
define('DB_COLLATE', '');

/**#@+
 * Clefs uniques d'authentification et salage.
 *
 * Remplacez les valeurs par défaut par des phrases uniques !
 * Vous pouvez générer des phrases aléatoires en utilisant 
 * {@link https://api.wordpress.org/secret-key/1.1/salt/ le service de clefs secrètes de WordPress.org}.
 * Vous pouvez modifier ces phrases à n'importe quel moment, afin d'invalider tous les cookies existants.
 * Cela forcera également tous les utilisateurs à se reconnecter.
 *
 * @since 2.6.0
 */
define('AUTH_KEY',         'hX++DsV2}De[KAb^GY*m`&>{W1^=[LbU2lmEY;y*!U~Ev|! <d(>>xYnuD*Y,@ji');
define('SECURE_AUTH_KEY',  'O-IlVnb8_Zm!B+y6bqd,4r./eH4vh_r|rB!$i>k|]@!o/`uZj{Z{C@nTESc7[p>M');
define('LOGGED_IN_KEY',    'FniX: _&j[uz{[U<bV&b2z$j|~63XHUn73:~jo/eGuyo`h|JEaY$~UEtnI-)k(<q');
define('NONCE_KEY',        'x`)e?~+9N/X~6QMY``{j?ZXBx&b(9Jh1<JOq=T:(4cKbq6k5@Z(+CLa!u^bb_,,i');
define('AUTH_SALT',        '{QSn2ZU@1PZf5*zD-ZNU-~MSFqd1-U>CY14t?y_aZe]WpLH+C*X4*`UH+a+RT$K5');
define('SECURE_AUTH_SALT', 'x.HrkV=$9RZaG 3m$MZq:]6F^Om/`|vf2@}xF3?ap2{ugYek^eM-{`N@{&Z?;>H0');
define('LOGGED_IN_SALT',   'LgJ|,gAO]%(r(5-tX?FGAUWUooU,h}Ow;89W!E%jls`|]sW~m<@@1gX$K[ehoBOa');
define('NONCE_SALT',       '<j!Dg{}lhfFCb;`}h<0c4uxd?Uc)K:]&irKB$AL@UM:ew>h1vtAoCp&Q$dej:L4O');
/**#@-*/

/**
 * Préfixe de base de données pour les tables de WordPress.
 *
 * Vous pouvez installer plusieurs WordPress sur une seule base de données
 * si vous leur donnez chacune un préfixe unique. 
 * N'utilisez que des chiffres, des lettres non-accentuées, et des caractères soulignés!
 */
$table_prefix  = 'wp_';

/** 
 * Pour les développeurs : le mode déboguage de WordPress.
 * 
 * En passant la valeur suivante à "true", vous activez l'affichage des
 * notifications d'erreurs pendant votre essais.
 * Il est fortemment recommandé que les développeurs d'extensions et
 * de thèmes se servent de WP_DEBUG dans leur environnement de 
 * développement.
 * 
 * Pour obtenir plus d'information sur les constantes 
 * qui peuvent être utilisée pour le déboguage, consultez le Codex.
 * 
 * @link https://codex.wordpress.org/Debugging_in_WordPress 
 */ 
define('WP_DEBUG', false); 

/* C'est tout, ne touchez pas à ce qui suit ! Bon blogging ! */

/** Chemin absolu vers le dossier de WordPress. */
if ( !defined('ABSPATH') )
	define('ABSPATH', dirname(__FILE__) . '/');

/** Réglage des variables de WordPress et de ses fichiers inclus. */
require_once(ABSPATH . 'wp-settings.php');
