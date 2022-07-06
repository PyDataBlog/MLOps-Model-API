#!/usr/bin/python

import unittest

class FooTest(unittest.TestCase):
    '''Sample test case -- FooTest()'''
    
    def setUp(self):
        '''Set up for testing...'''
        print 'FooTest:setUp_:begin'
         
        testName = self.shortDescription()
        if (testName == 'Test routine A'):
            print 'setting up for test A'
        elif (testName == 'Test routine B'):
            print 'setting up for test B'
        else:
            print 'UNKNOWN TEST ROUTINE'
             
        print 'FooTest:setUp_:end'
        
    def testA(self):
        '''Test routine A'''
        print 'FooTest: running testA...'
        
    def testB(self):
        '''Test routine B'''
        print 'FooTest: running testB...'
        
    def tearDown(self):
        '''Tear down from testing...'''
        print 'FooTest:tearDown_:begin'
         
        testName = self.shortDescription()
        if (testName == 'Test routine A'):
            print 'cleaning up after test A'
        elif (testName == 'Test routine B'):
            print 'cleaning up after test B'
        else:
            print 'UNKNOWN TEST ROUTINE'
             
        print 'FooTest:tearDown_:end'
        
class BarTest(unittest.TestCase):
    '''Sample test case -- BarTest()'''
    
    def setUp(self):
        '''Set up for testing...'''
        print 'BarTest:setUp_:begin'
         
        testName = self.shortDescription()
        if (testName == 'Test routine A'):
            print 'setting up for test A'
        elif (testName == 'Test routine B'):
            print 'setting up for test B'
        else:
            print 'UNKNOWN TEST ROUTINE'
             
        print 'BarTest:setUp_:end'
        
    def testA(self):
        '''Test routine A'''
        print 'BarTest: running testA...'
        
    def testB(self):
        '''Test routine B'''
        print 'BarTest: running testB...'
        
    def tearDown(self):
        '''Tear down from testing...'''
        print 'BarTest:tearDown_:begin'
         
        testName = self.shortDescription()
        if (testName == 'Test routine A'):
            print 'cleaning up after test A'
        elif (testName == 'Test routine B'):
            print 'cleaning up after test B'
        else:
            print 'UNKNOWN TEST ROUTINE'
             
        print 'BarTest:tearDown_:end'
        


if __name__ == '__main__':
    unittest.main()