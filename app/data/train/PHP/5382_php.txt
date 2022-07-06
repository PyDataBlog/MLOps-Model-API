<?php
/**
 * The base configurations of the WordPress.
 *
 * This file has the following configurations: MySQL settings, Table Prefix,
 * Secret Keys, WordPress Language, and ABSPATH. You can find more information
 * by visiting {@link http://codex.wordpress.org/Editing_wp-config.php Editing
 * wp-config.php} Codex page. You can get the MySQL settings from your web host.
 *
 * This file is used by the wp-config.php creation script during the
 * installation. You don't have to use the web site, you can just copy this file
 * to "wp-config.php" and fill in the values.
 *
 * @package WordPress
 */

// ** MySQL settings - You can get this info from your web host ** //
/** The name of the database for WordPress */
define('DB_NAME', 'simonvaughanljo');

/** MySQL database username */
define('DB_USER', 'simonvaughanljo');

/** MySQL database password */
define('DB_PASSWORD', 'DNaQvVBM');

/** MySQL hostname */
define('DB_HOST', 'simonvaughanljosmyndari.com.mysql');

/** Database Charset to use in creating database tables. */
define('DB_CHARSET', 'utf8');

/** The Database Collate type. Don't change this if in doubt. */
define('DB_COLLATE', '');

/**#@+
 * Authentication Unique Keys and Salts.
 *
 * Change these to different unique phrases!
 * You can generate these using the {@link https://api.wordpress.org/secret-key/1.1/salt/ WordPress.org secret-key service}
 * You can change these at any point in time to invalidate all existing cookies. This will force all users to have to log in again.
 *
 * @since 2.6.0
 */
define('AUTH_KEY',         'H%e|Vxr;L+d8~MFYBJ`RfyzDMT8|t+0tham^`rBX<+Bn0F(gcRnA)>eD.GZ}(u32');
define('SECURE_AUTH_KEY',  'LF7Q|3|#D8#e&d-(WPdN!,-bV,S@Y:2f9+LwQ~G^}&p2n*ud>RRnn!C5=%PZT^}+');
define('LOGGED_IN_KEY',    '&^$Q-<u,:5RF(|uHyH7a1D1~qzR{|C8^ +B/|1w{jYwD=YenbOrVfWD>8ZX~hj+a');
define('NONCE_KEY',        'I0)IW)pWJ8+ySkOq_B,F3O{f.pxHus~KgXC^pItkT@>jR@-WaJW!@Q,m*/suliJN');
define('AUTH_SALT',        'y;T_sA90AJnwoXga|VQ+/PV+0u+ZP9S.mstZ{#dG~mJR94Q[ATwQ,h84q[Jwu9qj');
define('SECURE_AUTH_SALT', 'K:=%mKL,-fHT3YOfn!F@I@MLe/9ey*~SuQhaZb&R$YtE7=Y~q-$SwaRnRXEN4YZ ');
define('LOGGED_IN_SALT',   ';!rW(tlQ-9a8QEYXt5ehWPysGN^5n%>2M_:i{qjj|[[<K&QR(zd{@Wu!b6o5[9$(');
define('NONCE_SALT',       'q2qK-kj3efLLN6|lYP.V69HY_~3@32~>4~+25&WrwpHXuPL@8Z[A0lS 8:v=L<s{');

/**#@-*/

/**
 * WordPress Database Table prefix.
 *
 * You can have multiple installations in one database if you give each a unique
 * prefix. Only numbers, letters, and underscores please!
 */
$table_prefix  = 'wp_';

/**
 * WordPress Localized Language, defaults to English.
 *
 * Change this to localize WordPress. A corresponding MO file for the chosen
 * language must be installed to wp-content/languages. For example, install
 * de_DE.mo to wp-content/languages and set WPLANG to 'de_DE' to enable German
 * language support.
 */
define('WPLANG', '');

/**
 * For developers: WordPress debugging mode.
 *
 * Change this to true to enable the display of notices during development.
 * It is strongly recommended that plugin and theme developers use WP_DEBUG
 * in their development environments.
 */
define('WP_DEBUG', false);

/* That's all, stop editing! Happy blogging. */

/** Absolute path to the WordPress directory. */
if ( !defined('ABSPATH') )
	define('ABSPATH', dirname(__FILE__) . '/');

/** Sets up WordPress vars and included files. */
require_once(ABSPATH . 'wp-settings.php');
