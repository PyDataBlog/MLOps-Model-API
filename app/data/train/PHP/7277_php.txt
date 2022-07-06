<?php namespace Ritey\LibPhoneNumberLaravel;
/**
* Libphonenumber-for-php service provider package for Laravel by David Wright.
*
* NOTICE OF LICENSE
*
* Licensed under the terms from David Wright
*
* @package LibPhoneNumberLaravel
* @version 1.0.0
* @author @ritey
* @license MIT
* @copyright (c) 2015, David Wright
* @link http://twitter.com/ritey
*/

use Illuminate\Support\Facades\Facade;

class LibPhoneNumberLaravelFacade extends Facade {

	/**
     * Get the registered name of the component.
     *
     * @return string
     */
    protected static function getFacadeAccessor() { return 'libphonenumber'; }
}