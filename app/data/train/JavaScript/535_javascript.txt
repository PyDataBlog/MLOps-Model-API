/** 
 * StaticText.js
 * Text that cannot be changed after loaded by the game
 */
 import GamePiece from './GamePiece.js';
 import Info from './Info.js';
 import Text from './Text.js';

export default class StaticText extends Text {

	constructor (config) {
		super(config);

		this.static = true;

	}

}
