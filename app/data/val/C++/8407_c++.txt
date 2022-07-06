#include "recursive.hpp"

#include <iostream>

template <typename T> bool RecSLL<T>::del(const T& comparable)
{
  bool found = false;
  if (this->head && comparable == this->head->value) {
    found = true;
    this->head = this->head->next;

    // Special case of the last element
    if (!this->head) {
      this->tail = this->head;
      // Terminal...
      return found;
    }

    // In case the new head is bad also...
    this->del(comparable);
    return true;
  }

  return del_int(comparable, this->head, this->tail);
}


template <typename T> bool
RecSLL<T>::del_int(const T& comparable,
		   std::shared_ptr<typename SinglyLinkedList<T>::element>& last,
		   std::shared_ptr<typename SinglyLinkedList<T>::element>& curr)
{
  if (curr) {
    if (comparable == curr->value) {
      last->next = curr->next;
      this->del_int(comparable, last, last->next);
      return true;
    } else {
      return this->del_int(comparable, curr, curr->next);
    }
  } else {
    // Set the tail to be sure!
    this->tail = last;
    return false;
  }
}


template <typename T> bool RecSLL<T>::contains(const T& comparable)
{
  return this->contains_int(comparable, this->head);
}


template <typename T> bool
RecSLL<T>::contains_int(const T& comparable,
			const std::shared_ptr<typename SinglyLinkedList<T>::element>& curr)
{
  if (curr) {
    if (comparable == curr->value) {
      return true;
    } else {
      return this->contains_int(comparable, curr->next);
    }
  } else {
    return false;
  }
}


// So that we get the templates generated
template class RecSLL<int>;
