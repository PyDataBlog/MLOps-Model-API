# All-Terrain-Life-Vest

All Terrain Life Vest- IEA Raspverry Pi Competition Entry

# Description 


import RPi.GPIO as GPIO
import time
import os
GPIO.setmode (GPIO.BCM)
GPIO.cleanup()
GPIO.setwarnings(False)
GPIO.setup(17,GPIO.OUT)
GPIO.setup(04,GPIO.OUT)
GPIO.setup(22, GPIO.IN)
print("---------------")
print("Button+GPIO")
print("---------------")

print GPIO.input(22)
while True:
    if(GPIO.input(22)==False):
        GPIO.output(17,GPIO.HIGH)
        GPIO.output(04,GPIO.HIGH)
        print("air bag activated")
        os.system('date')
        print GPIO.input(22)
        time.sleep(1)
        GPIO.output(17,GPIO.LOW)
        GPIO.output(04,GPIO.LOW)
    else:
        os.system('clear')
        print("air bag NOT activated")
        time.sleep(1)
