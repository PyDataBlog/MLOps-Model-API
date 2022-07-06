/******************************************************************************
 * AUTHOR:      Alexander Casal
 * FILE:        game.cpp
 * DESCRIPTION: Demonstrate the use of the Strategy Design Pattern through
 *              a very simple game which allows the user to select different
 *				weapons. Each weapon has a different characteristic, changing
 *				the way the player attacks.
 *****************************************************************************/
#include "game.h"
#include "largesword.h"
#include "smallsword.h"
#include "bow.h"
#include <memory>
#include <limits>
#include <iostream>

 /**
 * Constructor
 *
 * When we create the game we initialize playing to true so our game
 * loop runs.
 */
Game::Game()
{
	playing = true;
}

/**
* play
*
* Control the game loop allowing the player to select various
* options. The function changeWeapon allows us to use the strategy
* pattern to change which strategy we are using at runtime.
*/
void Game::play()
{
	std::cout << "Welcome to the Strategy Pattern Game!\n";
	showOptions();

	while (playing)
	{
		switch (getOption())
		{
		case 1:
			player.changeWeapon(std::make_unique<LargeSword>());
			break;
		case 2:
			player.changeWeapon(std::make_unique<SmallSword>());
			break;
		case 3:
			player.changeWeapon(std::make_unique<Bow>());
			break;
		case 4:
			player.attack();
			break;
		case 5:
			playing = false;
			break;
		}
	}
}

/**
* getOption
*
* Prompt the user to enter an option and retrieve it. We do some simple
* error checking to validate the user has entered a value between 1 and 5
* inclusive.
*/
int Game::getOption()
{
	bool valid = false;
	int input = 0;

	do
	{
		std::cout << "> ";

		// Validate the user has entered valid data
		if (std::cin >> input)
		{
			valid = true;

			if (input <= 0 || input > 5)
			{
				std::cout << "Please enter an option between 1 and 5\n";
				valid = false;
			}
		}
		else 
		{
			std::cin.clear();
			std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
			std::cout << "Plese enter an option between 1 and 5\n";
		}
	} while (!valid);

	return input;
}

/**
* showOptions
*
* Display a list of possible options the user can input for their next
* action.
*/
void Game::showOptions()
{
	std::cout << "\nPlease type an option from the menu (ex: > 4)\n";
	std::cout << "1.) Select large sword\n2.) Select small sword\n";
	std::cout << "3.) Select bow\n4.) Attack\n5.) Quit\n";
}