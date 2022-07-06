/**
 * The Furnace namespace
 *
 * @module furnace
 * @class Furnace
 * @static
 */

import Validation from 'furnace/packages/furnace-validation';
import I18n from 'furnace/packages/furnace-i18n';
import Forms from 'furnace/packages/furnace-forms';

export default {
	/**
	 * 
	 * @property Forms
	 * @type Furnace.Forms
	 */
	Forms : Forms,
	
	/**
	 * @property I18n
	 * @type Furnace.I18n
	 */
	I18n : I18n,
	
	/**
	 * @property Validation
	 * @type Furnace.Validation
	 */
	Validation: Validation
};
