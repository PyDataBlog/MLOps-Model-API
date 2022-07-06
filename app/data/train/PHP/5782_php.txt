<?php
/*
 * Social Widget
 */
class Custom_Search_Widget extends WP_Widget {

	function Custom_Search_Widget() {
		parent::__construct(
			'custom_search', // Base ID
			__('Custom Search', 'denta-lite'), // Name
			array( 'description' => __( 'Custom Search widget.', 'denta-lite' ), ) // Args
		);
	}

	function widget( $args, $instance ) {
		$title = apply_filters( 'widget_title', $instance['title'] );

		echo $args['before_widget'];
		if ( ! empty( $title ) )
			echo $args['before_title'] . $title . $args['after_title'];
		?>

		<form role="search" method="get" id="custom-searchform" class="custom-form-search" action="<?php echo esc_url( home_url( '/' ) ); ?>">
			<fieldset>
				<input type="submit" class="custom-input-submit" value="" />
				<input type="text" value="<?php echo get_search_query(); ?>" name="s" class="custom-input-text" placeholder="Search now..." />
			</fieldset>
		</form><!--/#custom-searchform .custom-form-search-->

		<?php
		echo $args['after_widget'];
	}

	function update( $new_instance, $old_instance ) {
		$instance = $old_instance;
		$instance['title'] = ( ! empty( $new_instance['title'] ) ) ? strip_tags( $new_instance['title'] ) : '';

		return $instance;
	}

	function form( $instance ) {
		if ( isset( $instance[ 'title' ] ) ) {
			$title = $instance[ 'title' ];
		}
		else {
			$title = __( 'Custom Search', 'denta-lite' );
		}
		?>
			<p>
				<label for="<?php echo $this->get_field_id( 'title' ); ?>"><?php _e( 'Title:', 'denta-lite' ); ?></label>
				<input class="widefat" id="<?php echo $this->get_field_id( 'title' ); ?>" name="<?php echo $this->get_field_name( 'title' ); ?>" type="text" value="<?php echo esc_attr( $title ); ?>">
			</p>

		<?php
	}
}

function social_widget_register() {
	register_widget( 'Custom_Search_Widget' );
}

add_action( 'widgets_init', 'social_widget_register' );

?>