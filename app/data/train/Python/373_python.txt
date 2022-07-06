import urllib2


def sumaDos():
    print 10*20
def division(a,b):
    result=a/b
    print result

def areatriangulo(base,altura):
    result2=(base*altura)/2
    print result2

def cast():
    lista=[1,2,3,"hola"]
    tupla=(1,2,3)
    diccinario={"key1":"Diego","key2":"Piqui","key3":"Chuy"}
    for k,v  in diccionario:
        print "%s %s" % (k,v)
        
    


class Estudiante(object):
def __init__(self, nombre, edad):
        self.nombre=nombre
        self.edad=edad

def hola(self):
    return self.nombre
def esMayor(self):
    if self.edad>=18:
        return true
    else:
        return false
def EXCEPTION():
    try:
        3/0
        except Exception:
            print "error"
def main():
    e=Estudiante("Diego",22)
    print"Hola %s" % e.hola()
    if e.esMayor():
        print"Es mayor de edad"
        else:
            print"Es menor de edad"

    contador = 0
    while contador <=10:
        print contador
        contador +=1

EXCEPTION():
def getWeb():
    try:
        web=urllib2.urlopen("http://itjiquilpan.edu.mx/")
        print web.read()
        web.close()
        except urllib2.HTTPError, e:
            print e

        except urllib2.URLError as e:
            print e
def main():
   
    cast()
    

if __name__=="__main__":
    main()
    


    


