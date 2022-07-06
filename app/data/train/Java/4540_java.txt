package fr.lelouet.rpg.model;

import fr.lelouet.rpg.model.character.CharStats;

public class Character extends CharStats {

	public Character(RPGGame system) {
		super(system);
	}

	/**
	 *
	 * @return true if this character is an avatar
	 */
	public boolean isPlayer() {
		return true;
	}

	public int lvl;

}
