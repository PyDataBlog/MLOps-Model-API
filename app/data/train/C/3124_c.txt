/**
* @file KeyboardButtonTest.h
* @brief Contains KeyboardButtonTest fixture
* @author Khalin Yevhen
* @version 0.0.1
* @date 28.09.17
*/

#pragma once

#include <gtest\gtest.h>
#include "..\khalin04\KeyboardButton.h"
#include "..\khalin04\KeyboardButton.cpp"

/**
* @brief Test fixture for using the same data
* @author Khalin Yevhen
*/
class KeyboardButtonTest : public ::testing::Test {
protected:
	virtual void SetUp() {
		btnForm = ButtonForm::RECTANGULAR;
		btnName = "ABC";
		btnCode = 123;
		static KeyboardButton src(btnForm, btnCode, btnName);
		btn = &src;
	}

	ButtonForm btnForm;
	string btnName;
	int btnCode;
	KeyboardButton *btn;
};