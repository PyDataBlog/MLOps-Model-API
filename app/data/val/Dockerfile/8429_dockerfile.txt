FROM ros:jade-ros-core
RUN sudo apt-get -y update
RUN sudo apt-get -y install ros-jade-rtabmap-ros
CMD ["roslaunch", "rtabmap_ros", "rgbd_mapping.launch", "rtabmap_args:=\"--delete_db_on_start\""]
