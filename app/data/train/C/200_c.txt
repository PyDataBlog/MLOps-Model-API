//
//  rbLinkedList.h
//  rbLinkedList
//
//  Created by Ryan Bemowski on 4/6/15.
//

#pragma once

#include <string>

struct Node 
{
    int key;
    Node *next;
};

class rbLinkedList 
{
public:
    /** Constructor for the rbLinkedList class.
    */
    rbLinkedList();
    /** Deconstructor for the rbLinkedList class.
    */
    ~rbLinkedList();
    /** Determine if the collection is empty or not.
    * @return: A bool that will be true if the collection is empty and false
    * otherwise.
    */
    bool IsEmpty();
    /** Determine the number of nodes within the linked list.
    * @return: An integer that represents the number of nodes between the head
    * and tail nodes.
    */
    int Size();

    void AddKey(int key);
    void RemoveKey(int key);
    std::string ToString();

private:
    Node *h_;
    Node *z_;
};
