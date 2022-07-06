/*
 * Books.cpp
 * Author: suyashd95
 */

#include "Books.h"

#include <iostream>
#include <cstring>

using namespace std;

Books::Books() {

	stock = new int[size];
	price = new float[size];
	author = new const char*[size];
	publisher = new const char*[size];
	title = new const char*[size];

	populateData();
}

void Books::populateData() {

	for(int i = 0; i < size; i++) {

		switch(i) {

		case 0:
			stock[i] = 10;
			price[i] = 500.00;
			author[i] = "JK Rowling";
			publisher[i] = "Bloomsbury";
			title[i] = "Harry Potter";
			break;

		case 1:
			stock[i] = 0;
			price[i] = 200.00;
			author[i] = "Jeffrey Archer";
			publisher[i] = "Penguin";
			title[i] = "A Quiver Full Of Arrows";
			break;

		case 2:
			stock[i] = 3;
			price[i] = 1000.00;
			author[i] = "JKR Tolkien";
			publisher[i] = "Penguin";
			title[i] = "The Lord Of The Rings";
			break;

		case 3:
			stock[i] = 0;
			price[i] = 80.00;
			author[i] = "Danielle Steele";
			publisher[i] = "Penguin";
			title[i] = "A Perfect Stranger";
			break;

		case 4:
			stock[i] = 7;
			price[i] = 300.00;
			author[i] = "Jane Austen";
			publisher[i] = "Bloomsbury";
			title[i] = "Pride & Prejudice";
			break;
		}
	}
}

void Books::bookPurchase() {

	string author, title;
	cout << "Enter the author of the book: " << flush;
	getline(cin, author, '\n');
	cout << "Enter the title of the book: " << flush;
	getline(cin, title, '\n');

	char found = 'n';
	for(int i = 0; i < size; i++) {

		int condition1 = strcmp(this->author[i], author.c_str());
		int condition2 = strcmp(this->title[i], title.c_str());

		if(condition1 == 0 &&  condition2 == 0) {

			found = 'y';
			cout << endl;
			cout << "Search Successful..." << endl;
			cout << "Details of the Book:\n" << endl;
			cout << "   Author: " << this->author[i] << endl;
			cout << "    Title: " << this->title[i] << endl;
			cout << "Publisher: " << this->publisher[i] << endl;
			cout << "    Price: " << this->price[i] << endl;

			unsigned int requiredCopies;
			cout << "Please enter the number of copies required for purchase: " << flush;
			cin >> requiredCopies;

			cout << endl;
			if(requiredCopies <= this->stock[i])
				cout << "Total Cost: " << this->price[i] * requiredCopies << endl;
			else
				cout << "Required copies not in stock." << endl;

			break;
		}
	}

	if(found == 'n')
		cout << "\nSearch Unsuccessful...\nBook is not available." << endl;
}

Books::~Books() {

	delete [] stock;
	delete [] price;
	delete [] author;
	delete [] publisher;
	delete [] title;
}
