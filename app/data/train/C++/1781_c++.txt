/*
 * ObjectReference2.cpp
 *
 *  Created on: Oct 16, 2015
 *      Author: kfulks
 */

#include "../../Concept/Reference/ObjectReference2.h"

#include <iostream>


namespace gitux {

using namespace std;

ObjectReference2::ObjectReference2() :
	myRef() {
	// TODO Auto-generated constructor stub
	cout << "ObjectReference2 constructor" << endl;
}

ObjectReference2::~ObjectReference2() {
	// TODO Auto-generated destructor stub
	cout << "ObjectReference2 destructor" << endl;
}

void ObjectReference2::SetReference(int &testRef) {
	myRef = &testRef;
}

void ObjectReference2::ChangeReference(int &testRef) {
	testRef = 2;
}

void ObjectReference2::PrintReference() {
	cout << "ObjectReference2 value is " << *myRef << endl;
}

} /* namespace gitux */
