<?php
# Database Configuration
define( 'DB_NAME', 'wp_promedia' );
define( 'DB_USER', 'promedia' );
define( 'DB_PASSWORD', 'Hgcjr1nz4UG6M7kD4a2W' );
define( 'DB_HOST', '127.0.0.1' );
define( 'DB_HOST_SLAVE', '127.0.0.1' );
define('DB_CHARSET', 'utf8');
define('DB_COLLATE', 'utf8_unicode_ci');
$table_prefix = 'wp_';

# Security Salts, Keys, Etc
define('AUTH_KEY',         'a`F4guyG[^ Y}d c{idj5&MhTD&|JP[[]~@3:[oUZ0C+8}dUu9hqiW9ZJ-5c}6+|');
define('SECURE_AUTH_KEY',  'eU7MN1?7~|AT,pN|!qQ$3BhT%iYHi~}Uf%`R_eH#$I_XJy3utwjOa}-`Z8rP;eZ;');
define('LOGGED_IN_KEY',    'szAvG,^<>/so:#:-(6RKz~caq+*)lRek+o{44r$2?}?Qd.)taRY0+rd+d<6|nb>s');
define('NONCE_KEY',        '0Sd#vgoYj|3 _{zHx+O!@bT*l13wu1=N+fNV]P7Cx|JzL_&=_5Kjs$y^P7?IEss+');
define('AUTH_SALT',        'vrQ2560/7/rdC)gXpr+&2;`w-RD%VyZwu+a5sV)!X<5s_Wq,7}S*7Q~vR|K(Lf*B');
define('SECURE_AUTH_SALT', 'wV^Lf;y6[zqv4!Bm8eZuE!u]k||b!mF]vAx|/)5,aQP`,Mav3SFC;2g`gL4*0F{R');
define('LOGGED_IN_SALT',   '?|e&TXiXjP$H5#)*!6I+2]^w#?iL? G3H%pG[MvLgr|kT8+0?w&4BTX+nWnp57f`');
define('NONCE_SALT',       '4~7piruf+MjyI%%H(U>r|GPuZDtb#EbJ|@ISBwf+V5+nzEzGNv>ihd#?#wpa+~/|');


# Localized Language Stuff

define( 'WP_CACHE', TRUE );

define( 'WP_AUTO_UPDATE_CORE', false );

define( 'PWP_NAME', 'promedia' );

define( 'FS_METHOD', 'direct' );

define( 'FS_CHMOD_DIR', 0775 );

define( 'FS_CHMOD_FILE', 0664 );

define( 'PWP_ROOT_DIR', '/nas/wp' );

define( 'WPE_APIKEY', 'dfdfa423dd9708ad4a65f5e571606be13b075046' );

define( 'WPE_FOOTER_HTML', "" );

define( 'WPE_CLUSTER_ID', '1986' );

define( 'WPE_CLUSTER_TYPE', 'pod' );

define( 'WPE_ISP', true );

define( 'WPE_BPOD', false );

define( 'WPE_RO_FILESYSTEM', false );

define( 'WPE_LARGEFS_BUCKET', 'largefs.wpengine' );

define( 'WPE_LBMASTER_IP', '212.71.255.152' );

define( 'WPE_CDN_DISABLE_ALLOWED', true );

define( 'DISALLOW_FILE_EDIT', FALSE );

define( 'DISALLOW_FILE_MODS', FALSE );

define( 'DISABLE_WP_CRON', false );

define( 'WPE_FORCE_SSL_LOGIN', false );

define( 'FORCE_SSL_LOGIN', false );

/*SSLSTART*/ if ( isset($_SERVER['HTTP_X_WPE_SSL']) && $_SERVER['HTTP_X_WPE_SSL'] ) $_SERVER['HTTPS'] = 'on'; /*SSLEND*/

define( 'WPE_EXTERNAL_URL', false );

define( 'WP_POST_REVISIONS', FALSE );

define( 'WPE_WHITELABEL', 'wpengine' );

define( 'WP_TURN_OFF_ADMIN_BAR', false );

define( 'WPE_BETA_TESTER', false );

umask(0002);

$wpe_cdn_uris=array ( );

$wpe_no_cdn_uris=array ( );

$wpe_content_regexs=array ( );

$wpe_all_domains=array ( 0 => 'promedia.wpengine.com', );

$wpe_varnish_servers=array ( 0 => 'pod-1986', );

$wpe_special_ips=array ( 0 => '212.71.255.152', );

$wpe_ec_servers=array ( );

$wpe_largefs=array ( );

$wpe_netdna_domains=array ( );

$wpe_netdna_domains_secure=array ( );

$wpe_netdna_push_domains=array ( );

$wpe_domain_mappings=array ( );

$memcached_servers=array ( );

define( 'WPE_SFTP_PORT', 22 );
define('WPLANG','');

# WP Engine ID


# WP Engine Settings






# That's It. Pencils down
if ( !defined('ABSPATH') )
	define('ABSPATH', dirname(__FILE__) . '/');
require_once(ABSPATH . 'wp-settings.php');

$_wpe_preamble_path = null; if(false){}
