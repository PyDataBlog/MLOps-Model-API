package main

import (
 "fmt"
 "errors"
)
type value int
type Node struct{
	value
	pre,next *Node
}

type List struct{
	head, tail *Node
}

func (li *List) PushBack(elem value) *List{
	newOne := &Node{value:elem}
	if li.tail == nil {
		li.head = newOne
		li.tail = li.head
	}else  {
		li.tail.next = newOne
		newOne.pre = li.tail
		li.tail =newOne
	}
	return li
}	

var emptyError = errors.New("empty list")

func (li *List) Pop() (val int, err error) {
	if li.tail == nil {
		return -1, emptyError
	}else {
		val := li.tail.value
		li.tail = li.tail.pre
		return int(val),err
	}
}

func (li *List) Next() *Node{
	if li.tail == nil {
		return nil
	}else {
		val := li.tail
		li.tail = li.tail.pre
		return val
	}
}

func main() {
 li := new(List)

 li.PushBack(1).PushBack(2).PushBack(4)
 for val := li.head; val != nil; val = val.next{
	fmt.Printf("%d",val.value)
 }
}
