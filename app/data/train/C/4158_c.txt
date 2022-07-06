#ifndef __CHARACTER_H__
#define __CHARACTER_H__

#include <vector>
#include <string>
#include <map>
#include "system.h"
#include "inventory.h"
#include "money.h"

std::string nameList();

typedef enum {
	OUTGOINGS,
	INCOME,
	CHECK
} UseType;

class Character : public Inventory, public Money {
public:
	std::string name;
	int wallet(UseType type);
	void wallet(UseType type, int aMoney);
	void setID(int id);
	int retID();
protected:
private:
	int thisId;
};

#endif