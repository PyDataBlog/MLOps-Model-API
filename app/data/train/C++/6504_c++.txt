#include "ros/ros.h"
#include "std_msgs/String.h"

/**
* This tutorial demonstrates simple receipt of messages over the ROS system
*/

void chatterCallback(const std_msgs::String::ConstPtr& msg)
{
	ROS_INFO("I heard: [%s]", msg->data.c_str());
}

int main(int argc, char **argv)
{
	/**
	*The ros::init() function needs to see argc and argv so that it can perform 
	*any ROS arguments and name remapping that were provided at the command line. 
	*For programmatic remappings you can use a different version of init() which takes remappings directly
	*but for most command-line programs, passing argc adn argv si the easiest way to do it.
	*The third argument to init() is the name of the node. 
	*
	*You must call on of the versions of ros::init() before using any other part of the ROS system
	*/
	ros::init(argc,argv, "listener");

	/**
	*NodeHandle is teh main access point to communications with the ROS system. 
	*the first nodehandle construtect will fully initialize this node, and the last
	*nodehandle desctructed will close down the node.
	*/
	ros::NodeHandle n;

	/**
	*The subscribe() call is how you tell ROS that you want ot recieve messages on a given
	*topic. This invokes a call to the ROS master node, which keeps a registry fo who is publishing
	*and who is subscribing. messages are passed to a callback function, here called chatterCallback.
	*subscribe() returns a subscriber object that you must hod on to until you want ot unsubscribe. 
	*When all copies of the subscriber object go out of scope, this callback will automatically be unsubscribed
	*from this topic
	*
	*The second parameter to the subscribe() function is the size of the message queue. If messages are 
	*arriving faster then they are being processed, this is teh number of messages that will be buffered
	*up before beginning to throw away the oldest ones.
	*/
	ros::Subscriber sub = n.subscribe("chatter",1000,chatterCallback);

	/**
	* ros::spin() will enter a loop, pumping callbacks. With this version, all callbacks will be called from within
	*this thread (the main one). ros::spin() will exit when Ctrl-C is pressed, or the node is shutdown by the master.
	*/
   
   ros::spin();

    return 0;
}