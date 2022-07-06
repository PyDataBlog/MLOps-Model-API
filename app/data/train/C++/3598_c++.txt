/*
  Kiduino.cpp - Kiduino library
  Copyright (c) 2014 Adithya Selvaprithiviraj.  All right reserved.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/******************************************************************************
 * Includes
 ******************************************************************************/

#include "Kiduino.h"
#include "Arduino.h"




/** LED class begins**/

 /******************************************************************************
 * Definitions
 ******************************************************************************/

/******************************************************************************
 * Constructors
 ******************************************************************************/

Led::Led(int pin)
{
	pinMode(pin,OUTPUT);
	_pin = pin;
	ledstate=0;
	long int current;
	long int previous=0;
	brightness = 0; 
	fadeAmount = 5; 
}

/******************************************************************************
 * User API
 ******************************************************************************/


void Led::blink(int a)
{
	current=millis();
  if(current-previous>=a)
  {
    ledstate=!ledstate;
    previous=current;
  }
  digitalWrite(_pin,ledstate);

}

void Led::on()
{
	digitalWrite(_pin,HIGH);
}

void Led::off()
{
	digitalWrite(_pin,LOW);
} 

void Led::fade()
{
	analogWrite(_pin, brightness);     
  brightness = brightness + fadeAmount;

  if (brightness == 0 || brightness == 255) {
    fadeAmount = -fadeAmount ; 
  }     
    delay(30);
}


/**LED class ends**/


/**Ping class function starts**/

 /******************************************************************************
 * Definitions
 ******************************************************************************/

/******************************************************************************
 * Constructors
 ******************************************************************************/

Ping::Ping(int pin)
{
	int _pin = pin;
}

/******************************************************************************
 * User API
 ******************************************************************************/


long int Ping::getcm()
{
	pinMode(_pin,OUTPUT);
 	digitalWrite(_pin,1);
 	delayMicroseconds(10);
 	pinMode(_pin,INPUT);
 	long int x=pulseIn(_pin,HIGH);//microseconds
 	long int dist=(330*x/20000);
 	return (dist);
}

long int Ping::getin()
{
	pinMode(_pin,OUTPUT);
 	digitalWrite(_pin,1);
 	delayMicroseconds(10);
 	pinMode(_pin,INPUT);
 	long int x=pulseIn(_pin,HIGH);//microseconds
 	long int dist=(330*x/20000);
 	return (dist*0.393701);
}

/**Ping class ends**/


/**Sled class begins**/

 /******************************************************************************
 * Definitions
 ******************************************************************************/

/******************************************************************************
 * Constructors
 ******************************************************************************/

SLed::SLed()
{
	boolean number[10]={B111111,B000110,B01011011,B01001111,B01100110,B01101101,B01111101,B111,B1111111,B1101111};
	DDRD=255;
}

/******************************************************************************
 * User API
 ******************************************************************************/

void SLed::display(int a)
{
	PORTD=number[a];
} 

/**SLed class ends**/

/**RGB class begins**/

/******************************************************************************
 * Definitions
 ******************************************************************************/

/******************************************************************************
 * Constructors
 ******************************************************************************/

RGB::RGB(int pin1,int pin2,int pin3)
{
	_pin1=pin1;
	_pin2=pin2;
	_pin3=pin3;
	int x;
	int y;
	int z;
}

/******************************************************************************
 * User API
 ******************************************************************************/

void RGB::write(int a,int b,int c)
{
  	analogWrite(_pin1,a);
 	  analogWrite(_pin2,b);
  	analogWrite(_pin3,c);
}

void RGB::rand()
{
	  x=random(0,255);
  	y=random(0,255);
  	z=random(0,255);
  	analogWrite(_pin1,x);
  	analogWrite(_pin2,y);
  	analogWrite(_pin3,z);
}

/**RGB class ends**/

/**SIRC class begins**/

/******************************************************************************
 * Definitions
 ******************************************************************************/

/******************************************************************************
 * Constructors
 ******************************************************************************/


SIRC::SIRC(int pin,int mode)
{
	if(mode==1)
	{
		pinMode(pin,OUTPUT);
	}
	if(mode==2)
	{
		pinMode(pin,INPUT);
	}
	int _pin=pin;
}


/******************************************************************************
 * User API
 ******************************************************************************/
 
int SIRC::read()
{

  if(pulseIn(_pin,LOW)>2000)
  {
    val=0;
    for(int i=0;i<12;i++)
    {
      	if(pulseIn(_pin,LOW)>1000)
      	data[i]=1;
      	else
      	data[i]=0;
    }
    for(int i=0;i<12;i++)
    {
      	if(data[i]==1)
      	val=val+(1<<i);
    }
    return (val);
  }

}

void SIRC::write(int value)
{

	sendCode(96);
  	for(int i=0;i<12;i++)
  	{
    	int val=value%2;
    	value = value/2;
    	if(val==1)
    		sendCode(48);
    	else
    		sendCode(24);
  	}
}

void SIRC::sendCode(int number)
{
	for(int i=0;i<number;i++)
  	{
    	digitalWrite(14,HIGH);
    	delayMicroseconds(8);
    	digitalWrite(14,LOW);
    	delayMicroseconds(7);
  	}
  	delayMicroseconds(600);
}

/**SIRC class ends **/


/**PIN class begins**/

/******************************************************************************
 * Definitions
 ******************************************************************************/

/******************************************************************************
 * Constructors
 ******************************************************************************/


PIN::PIN(int pin,int mode)
{
  if(mode==1)
  {
    pinMode(pin,INPUT);
  }
  if(mode==2)
  {
    pinMode(pin,OUTPUT);
  }
  int _pin=pin;
  int analog;
}

/******************************************************************************
 * User API
 ******************************************************************************/

 void PIN::on()
 {
  digitalWrite(_pin,HIGH);
 }

 void PIN::off()
 {
  digitalWrite(_pin,LOW);
 }

 void PIN::awrite(int a)
 {
  analogWrite(_pin,a);
 }

 int PIN::aread()
 {
  analog=analogRead(_pin);
  return analog;
 }

 /**PIN class ends**/

 /**GP120 class begins**/

 /******************************************************************************
 * Definitions
 ******************************************************************************/

/******************************************************************************
 * Constructors
 ******************************************************************************/

GP120::GP120(int pin)
{
  int volts;
  int range;
  int _pin=pin;
}

/******************************************************************************
 * User API
 ******************************************************************************/

 int GP120::read()
 {
  int volts=analogRead(_pin);
  int range=(2914/(volts+5))-1;
  return range;
 }

 