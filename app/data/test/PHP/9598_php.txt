<?php

namespace Bartender\Outputter;

/**
 * This outputs a barcode as a PNG
 *
 * @author  Josh McMillan <josh@joshmcmillan.co.uk>
 */
class PNGOutputter extends \Bartender\Outputter
{
	
	/**
	 * Default options
	 * @var array
	 */
	protected $defaultOptions = array(

		'barHeight'       => 60,
		'barWidth'        => 1,
		'backgroundColor' => array(255, 255, 255),
		'barColor'        => array(0, 0, 0)

	);

	/**
	 * Renders out a PNG using the GD library
	 */
	public function render()
	{

		$booleans = $this->booleans();

		$width    = count($booleans) * $this->option('barWidth');
		$height   = $this->option('barHeight');

		$image    = imagecreate($width, $height);

		$backgroundColor = $this->option('backgroundColor');
		$backgroundColor = imagecolorallocate($image, $backgroundColor[0], $backgroundColor[1], $backgroundColor[2]);

		$barColor = $this->option('barColor');
		$barColor = imagecolorallocate($image, $barColor[0], $barColor[1], $barColor[2]);

		$x       = 0;

		foreach($booleans as $line)
		{

			if($line)
			{

				imagefilledrectangle($image, $x, 0, $x + $this->option('barWidth') - 1, $height - 1, $barColor);

			}

			$x += $this->option('barWidth');

		}

		imagepng($image);

	}

	/**
	 * Returns string version of PNG
	 * @return string PNG
	 */
	public function __toString() {

		ob_start();

		$this->render();

		return ob_get_clean();

	}

}