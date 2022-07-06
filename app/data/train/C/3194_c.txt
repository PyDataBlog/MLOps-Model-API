/**
* @file PrintableButton.h
* @brief Contains PrintableButton class declaration
* @author Khalin Yevhen
* @version 0.0.1
* @date 07.09.17
*/

#pragma once

#include "..\khalin03\Button.h"
#include <StorageInterface.h>
#include "..\khalin01\Printable.h"
#include <string>

/**
* @brief It's just a printable version of Button class.
*
* Instances of this class are able to be printed into file and be represented in the string.
* @author Khalin Yevhen
*/
class PrintableButton :
	public Button, public MStorageInterface, public Printable {
public:
	PrintableButton(ButtonForm form);
	
	PrintableButton();

	virtual ~PrintableButton();

	// inherited method
	virtual std::string toString();

	// inherited method
	virtual void OnStore(std::ostream& aStream);

	// inherited method
	virtual void OnLoad(std::istream& aStream);
};

typedef PrintableButton PButton;