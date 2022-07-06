<?php namespace Know_The_Code_Show\Episode9;
/**
 * Know the Code Show - Episode 9
 *
 * @package         Know_The_Code_Show\Episode9
 * @author          WPDevelopersClub and hellofromTonya
 * @license         GPL-2.0+
 * @link            http://wpdevelopersclub.com/
 * @copyright       2015 WP Developers Club
 *
 * @wordpress-plugin
 * Plugin Name:     Know the Code Show - Episode 9 Q3
 * Plugin URI:      http://wpdevelopersclub.com/
 * Description:     Supporting code for the episode
 * Version:         1.0.0
 * Author:          WP Developers Club and Tonya
 * Author URI:      http://wpdevelopersclub.com
 * Text Domain:     wpdevsclub
 * Requires WP:     3.5
 * Requires PHP:    5.3
 */


add_action( 'loop_start', __NAMESPACE__ . '\\simulate_apply_filters' );
function simulate_apply_filters() {
	$event_registry_mock = array(
		'foo'                           	=> array(
			9                           	=> array(
				'foo_9'                 => array(
					'function'      => 'foo_9',
					'accepted_args' => 2,
				),
			),
			10                          	=> array(
				'foo_10'                => array(
					'function'      => 'foo_10',
					'accepted_args' => 2,
				),
				'bar_10'                => array(
					'function'      => 'bar_10',
					'accepted_args' => 2,
				),
				'baz_10'                => array(
					'function'      => 'baz_10',
					'accepted_args' => 2,
				),
			),
			15                          	=> array(
				'foo_15'                => array(
					'function'	=> 'foo_15',
					'accepted_args'	=> 2,
				),
			),
		),
	);
	d( $event_registry_mock );
	$tag = 'foo';
	
	reset( $event_registry_mock[ $tag ] );
	do {
		foreach( (array) current( $event_registry_mock[ $tag ] ) as $priority => $the_ ) {
			echo $priority;
			d( $the_ );
		}
	} while ( false !== next( $event_registry_mock[ $tag ] ) );
} 