package SSR::Model::DB;
use Modern::Perl;
use base qw/Rose::DB/;

__PACKAGE__->use_private_registry;
__PACKAGE__->default_connect_options( sqlite_unicode => 1 ); 
 
__PACKAGE__->register_db(

	driver		=> 'sqlite',
	type		=> 'default',
	domain		=> 'default',
	database	=> 'db/ssr.db',

);
	
1;
