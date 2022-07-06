Module SelFunc
    '    #-------------------------------------------------------------------------------
    '# Name: selFunc
    '# Purpose: Contains a selection of simple functions
    '#
    '# Author: Petter
    '#
    '# Created: 14.05.2012
    '# Copyright: (c) Petter 2012
    '# Licence: <your licence>
    '#-------------------------------------------------------------------------------

    'import numpy as np
    'import re
    'from datetime import datetime
    '#from pylab import *


    'figureNumber = 1
    'paraMeterArray = {}

    'Returns the argument in degrees
    Function deg(ByVal a As Double)
        deg = a * 180 / Math.PI
    End Function

    'Returns the argument in radians
    Function rad(ByVal a As Double) As Double
        rad = a / 180 * Math.PI
    End Function

    'Returns the argument in rpm
    Function rpm(ByVal a As Double) As Double

        rpm = a * 30 / Math.PI
    End Function
Function  radPerS(a):
    """
    Returns the argument in rad/s
    """
        Return float(a) * np.pi / 30

Function  temperature(y,T0):
        Return T0 - 0.0065 * y

Function  pressure(p0,T0,y):
    return p0*(1-0.0065*y/T0)**(g()/(287.058*0.0065))

Function  density(p0,T0,y):
        Return pressure(p0, T0, y) / (287.058 * temperature(y, T0))

Function  g():
        Return 9.80665

Function  p0ISA():
        Return 101325

Function  T0ISA():
        Return 288.15

Function  pSat(T):
    return (6.1078*10**(7.5*(T-273.15)/((T-273.15)+237.3)))*100

Function  pVapour(T0,y,humidity):
        Return humidity * pSat(temperature(y, T0))

Function  pDry(humidity,p0,T0,y):
        Return pressure(p0, T0, y) - pVapour(T0, y, humidity)

Function  densityWithHumidity(humidity,p0,T0,y):
        Return (pressure(p0, T0, y) / (287.058 * temperature(y, T0))) * (1 + humidity) / (1 + humidity * 461.495 / 287.058)


Function  dynamicViscosity(T):
    return 	1.51204129*T**3/2/(T+120)

Function  kinematicViscocity(my,rho):
        Return my / rho

Function  reynoldsNumber(nu,velocity,c):
        Return c * velocity / nu

Function  setFigureNumber():
    global figureNumber
        figureNumber = figureNumber + 1

Function  getFigureNumber():
        Return figureNumber

Function  getDateString():
        collection = datetime.now()
    myString = collection.isoformat() #collection.year + collection.month +collection.day
        myString = re.sub("\:", "-", myString)
        myString = re.sub("\.", "MS", myString)
        Return myString

Function  writeDict(_dictonary,_fileName):
    f = open(_fileName,'w')
        temp = str(_dictonary)
        temp = re.sub(",", "\n", temp)
        f.write(str(temp))
        f.close()

Function  readDict(_dictonary,_fileName):
    f = open(_fileName,'r')
        my_dict = eval1(f.read())
        f.close()

Function  eval1(x):
        Try : Return eval(x)
except:     Return x
    A = { eval1(y[0]) : eval1(y[1]) for y in [x.split() for x in open(filename).readlines()] }



Function  vThermic(height,heightThermicCeil,vThermicCeil):
    """
    Returns the thermic windspeed
    """
        Return vThermicCeil / heightThermicCeil * height


Function  lengthToPlaneFromPulley(x,y):
    """
    Returns the actual length of the line
    """
    return (x**2+y**2)**0.5



End Module
