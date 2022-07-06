class Vehicle(object):

    def __init__(self, make, model, year):

        self.make = make
        self.model = model
        self.year = year

    def foo(self, a, b, c):
        print 'does some work'
        print a, b, c


class Car(Vehicle):

    def __init__(self, doors, *args):

        Vehicle.__init__(self, *args)
        self.doors = doors

    def foo(self, *args, **kw):
        print args, kw
        super(Car, self).foo(*args)


class Boat(Vehicle):

    def __init__(self, power, *args):

        Vehicle.__init__(self, *args)
        if power not in ('propeller', 'sail'):
            print 'warning: drive type not acceptable'
            raise TypeError

        self.power = power


class Airplane(Vehicle):

    def __init__(self):
        pass

if __name__ == '__main__':
    car = Car('honda', 'civic', '2002', '2')
    car.foo(1, 2, 3)

# ============================ EOF =============================
