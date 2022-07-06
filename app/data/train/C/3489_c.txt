#ifndef MYHEADER_H
#define MYHEADER_H

#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
typedef struct  {
  char* requestRaw;
} HTTPRequest;

  

//needs to be called before working with the webcore
int initWebcore();

int registerApplication(int (*function)(HTTPRequest) );

#endif
