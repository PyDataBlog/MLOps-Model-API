#include <queue>
#include <stack>
#include <iostream>
#include <limits.h>
#include "pathfinder.h"
/**
* Author: Jonathon Powell
* More info at https://github.com/jonathonwpowell/pathfinder
*/
struct node
{
	int xCorr; 		//x corrdinate of node
	int yCorr; 		//y corrdinate of node
	int dist;  		//distance from source
	int prev;  		//prev node in optimal path
	int isPassable; //1 if passable, 0 if not
};

int FindPath(const int nStartX, const int nStartY, const int nTargetX,
	const int nTargetY, const unsigned char* pMap,
	const int nMapWidth, const int nMapHeight, int* pOutBuffer,
	const int nOutBufferSize)
{
	//make sure assumtions are true
	if (nMapHeight < 1 || nMapWidth < 1 || nStartX < 0 || nStartX >= nMapWidth
		|| nStartY < 0 || nStartY >= nMapHeight || nOutBufferSize < 0
		|| pMap[nTargetY * nMapWidth + nTargetX] != 1)
	{
		//return -2 to signify error in input
		return(-2);
	}

	//check if the destination is the start
	if(nStartX == nTargetX && nStartY == nTargetY)
		return 0;

	//create array of nodes, one for each spot on the map
	int arraySize = nMapWidth * nMapHeight;
	struct node nodes[arraySize];

	//initialize each node
	for(int i = 0; i < arraySize; i++){
		nodes[i].xCorr = i % nMapWidth;
		nodes[i].yCorr = i / nMapWidth;
		nodes[i].dist = INT_MAX; 
		nodes[i].prev = -1;
	}
	
	//create a priority queue for storing nodes to process
	std::queue<int> queue;

	//push the start location onto the queue
	//only pushes the location in the map array
	queue.push(nStartY * nMapWidth + nStartX);
	
	//set the distance for the start node to 0
	nodes[nStartY * nMapWidth + nStartX].dist = 0;

	while(!queue.empty()){
		//get the next node in the queue
		int curr = queue.front();
	    queue.pop();	

		//if the current node is the target, break while loop
		if (curr == nTargetY * nMapWidth + nTargetX)
			break;

		//check if there is a node to the left
		if (curr >= 0 && nodes[curr].yCorr == nodes[curr-1].yCorr)
		{
			//if it is possibly part of a shorter path and passable
			if (nodes[curr-1].dist > nodes[curr].dist + 1
					&& pMap[curr-1] == 1)
			{
				//update the distance from source to the new value
				nodes[curr-1].dist = nodes[curr].dist + 1;
				//update the previous node to curr
				nodes[curr-1].prev = curr;
				//puh onto the queue
				queue.push(curr-1);
			}
		}
		//check if there is a node to the right
		if (curr + 1 <  arraySize  && nodes[curr].yCorr == nodes[curr+1].yCorr)
		{
			//if it is possibly part of a shorter path and passable
			if (nodes[curr+1].dist > nodes[curr].dist + 1
					&& pMap[curr+1] == 1)
			{
				//update the distance from source to the new value
				nodes[curr+1].dist = nodes[curr].dist + 1;
				//update the previous node to curr
				nodes[curr+1].prev = curr;
				//puh onto the queue
				queue.push(curr+1);
			}
		}
		//check if there is a node above
		if (curr - nMapWidth >= 0)
		{
			//if it is possibly part of a shorter path and passable
			if (nodes[curr-nMapWidth].dist > nodes[curr].dist + 1 
					&& pMap[curr-nMapWidth] == 1)
			{
				//update the distance from source to the new value
				nodes[curr-nMapWidth].dist = nodes[curr].dist + 1;
				//update the previous node to curr
				nodes[curr-nMapWidth].prev = curr;
				//puh onto the queue
				queue.push(curr-nMapWidth);
			}
		}
		//check if there is a node to the below
		if (curr + nMapWidth + 1 <= arraySize)
		{
			//if it is possibly part of a shorter path and passable
			if (nodes[curr+nMapWidth].dist > nodes[curr].dist + 1
				   	&& pMap[curr+nMapWidth] == 1)
			{
				//update the distance from source to the new value
				nodes[curr+nMapWidth].dist = nodes[curr].dist + 1;
				//update the previous node to curr
				nodes[curr+nMapWidth].prev = curr;
				//puh onto the queue
				queue.push(curr+nMapWidth);
			}
		}
	}
		
	//return -1 if no path
	if (nodes[nTargetY * nMapWidth + nTargetX].prev == -1)
		return -1;

	//stack for finding the path between the source and dest
	std::stack<int> pathStack;
	
	//current node in finding path back
	int curr = nTargetY * nMapWidth + nTargetX;
	//while the previous node is not the source
	while(curr != nStartY * nMapWidth + nStartX)
	{
		//change curr to the prev node, and prev node to the stack
		pathStack.push(curr);
		curr = nodes[curr].prev;
	}

	int numAdded = 0;  //used to make sure the buffer size is not exceeded

	//add the values in the stack to the pOutBuffer
	while(!pathStack.empty() && numAdded < nOutBufferSize)
	{
		pOutBuffer[numAdded] = pathStack.top();
		pathStack.pop();
		numAdded++;
	}


	//return the distance if there is a path
	return nodes[nTargetY * nMapWidth + nTargetX].dist;
}




