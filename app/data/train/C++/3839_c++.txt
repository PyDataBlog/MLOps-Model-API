/*
  Car.cpp - Library for IWM Rocket Avoidance Robot Car.
  Created by Steve Mullarkey, November 18, 2013.
  
  Copyright 2013 Steve Mullarkey
  Licensed under the Apache License, Version 2.0
*/

#include "Arduino.h"
#include <Servo.h>
#include "Car.h"


Car::Car(int frontServoPin,
    int backServoPin,
    int motorPin,
    int sonarTriggerPin,
    int sonarEchoPin)
 :    _sonar(sonarTriggerPin, sonarEchoPin),
      _motor(motorPin)

{
  _frontServoPin 	= frontServoPin;
	_backServoPin 	= backServoPin;
	_motorPin 		  = motorPin;
	_sonarTriggerPin = sonarTriggerPin;
	_sonarEchoPin 	= sonarEchoPin;

//   randomSeed(analogRead(0));  // pin 0 unconnected - use random noise as seed
 
  _motorSpeed = 150;
  _motor.setSpeed(_motorSpeed); // set speed for drive motor - 0 to 255
  _motor.run(RELEASE);  

}

// ----------------------------------------------------------------//
//              F O R W A R D   C O M M A N D                      //
// ----------------------------------------------------------------//
void Car::forward()
{
  _motor.setSpeed(_motorSpeed); 
  _motor.run(FORWARD);
  Serial.println("Car : forward called ");
  delay(MOTOR_DELAY);
}

// ----------------------------------------------------------------//
//              B A C K   C O M M A N D                            //
// ----------------------------------------------------------------//
void Car::back()
{
  _motor.setSpeed(_motorSpeed); 
  _motor.run(BACKWARD);
  Serial.println("Car : back called ");
  delay(MOTOR_DELAY);
}

// ----------------------------------------------------------------//
//              S T O P   C O M M A N D                            //
// ----------------------------------------------------------------//
void Car::stop()
{
  _motor.run(RELEASE);
  Serial.println("Car : stop called ");
  delay(MOTOR_DELAY);
}




// ----------------------------------------------------------------//
//              S T R A I G H T   C O M M A N D                    //
// ----------------------------------------------------------------//
void Car::straight()
{
  checkServoMotors();
  _backServo.write(ANGLE_TURN_AHEAD + ANGLE_TURN_OFFSET);
  Serial.println("Car : Straight called ");
  delay(SERVO_DELAY);
}

// ----------------------------------------------------------------//
//              L E F T   C O M M A N D                            //
// ----------------------------------------------------------------//
void Car::left()
{
  checkServoMotors();
  _backServo.write(ANGLE_TURN_LEFT + ANGLE_TURN_OFFSET);
  Serial.println("Car : Left called ");
  delay(SERVO_DELAY);
}

// ----------------------------------------------------------------//
//              R I G H T   C O M M A N D                          //
// ----------------------------------------------------------------//
void Car::right()
{
  checkServoMotors();  
  _backServo.write(ANGLE_TURN_RIGHT + ANGLE_TURN_OFFSET);
  Serial.println("Car : Right called ");
  delay(SERVO_DELAY);
}




// ----------------------------------------------------------------//
//              S E T   M O T O R   S P E E D                      //
// ----------------------------------------------------------------//
void Car::setMotorSpeed(int theMotorSpeed)
{
  _motorSpeed = theMotorSpeed;
  _motor.setSpeed(_motorSpeed); 
  Serial.print("Car : setMotorSpeed called with value of ");
  Serial.println(_motorSpeed);
}

// ----------------------------------------------------------------//
//              P A U S E                                          //
// ----------------------------------------------------------------//
void Car::pause(float pauseTime)
{
  int i = int(pauseTime * 1000);
  delay(i);
  Serial.print("Car : pause called with a value of ");
  Serial.println(pauseTime);
}



// ----------------------------------------------------------------//
//              P I N G   C O M M A N D                            //
// ----------------------------------------------------------------//
int Car::ping()
{
  checkServoMotors();  
  _frontServo.write(ANGLE_SCAN_OFFSET + ANGLE_SCAN_AHEAD); 
  delay(SERVO_DELAY); 
  unsigned int _distance = _sonar.ping_cm(); 
  Serial.print("Car : Ping called and returned ");
  Serial.print(_distance);
  Serial.println(" cm.");
  return _distance;
}

// ----------------------------------------------------------------//
//              P I N G   L E F T   C O M M A N D                  //
// ----------------------------------------------------------------//
int Car::pingLeft()
{
  checkServoMotors();  
  _frontServo.write(ANGLE_SCAN_OFFSET + ANGLE_SCAN_LEFT); 
  delay(SERVO_DELAY);  
  unsigned int _distance = _sonar.ping_cm(); 
  Serial.print("Car : PingLeft called and returned ");
  Serial.print(_distance);
  Serial.println(" cm.");
  return _distance;
}

// ----------------------------------------------------------------//
//              P I N G   R I G H T   C O M M A N D                //
// ----------------------------------------------------------------//
int Car::pingRight()
{
  checkServoMotors();  
  _frontServo.write(ANGLE_SCAN_OFFSET + ANGLE_SCAN_RIGHT); 
  delay(SERVO_DELAY);  
  unsigned int _distance = _sonar.ping_cm(); 
  Serial.print("Car : PingRight called and returned ");
  Serial.print(_distance);
  Serial.println(" cm.");
  return _distance;
}




// ----------------------------------------------------------------//
//              C H E C K   S E R V O S   A T T A C H E D          //
// ----------------------------------------------------------------//
void Car::checkServoMotors()
{
  if (_frontServo.attached()) {
//    Serial.print("Car : diag -> _frontServo still attached");
  } else {
    Serial.print("Car : diag -> _frontServo not attached");
     _frontServo.attach(_frontServoPin);
  }

  if (_backServo.attached()) {
//    Serial.println("Car : diag -> _backServo still attached");
  } else {
    Serial.println("Car : diag -> _backServo not attached");
     _backServo.attach(_backServoPin);
  }

}

// ----------------------------------------------------------------//
//              T E S T   S E R V O   M O T O R S                  //
// ----------------------------------------------------------------//
void Car::test() 
{

  delay(5000);

  checkServoMotors();

  int pos = 0;
  for(pos = 0; pos < 180; pos += 1)  // goes from 0 degrees to 180 degrees 
  {                                  // in steps of 1 degree 
    _frontServo.write(pos);              // tell servo to go to position in variable 'pos' 
    delay(15);                       // waits 15ms for the servo to reach the position 
  } 
  for(pos = 180; pos >= 0; pos-=1)     // goes from 180 degrees to 0 degrees 
  {                                
    _frontServo.write(pos);              // tell servo to go to position in variable 'pos' 
    delay(15);                       // waits 15ms for the servo to reach the position 
  } 
  _frontServo.write(90);


  for(pos = 0; pos < 180; pos += 1)  // goes from 0 degrees to 180 degrees 
  {                                  // in steps of 1 degree 
    _backServo.write(pos);              // tell servo to go to position in variable 'pos' 
    delay(15);                       // waits 15ms for the servo to reach the position 
  } 
  for(pos = 180; pos >= 0; pos-=1)     // goes from 180 degrees to 0 degrees 
  {                                
    _backServo.write(pos);              // tell servo to go to position in variable 'pos' 
    delay(15);                       // waits 15ms for the servo to reach the position 
  } 
  _backServo.write(90);
  delay(5000);

}

