__author__ = 'bruno'
import unittest

import algorithms.math.abacus as Abacus


class TestAbacus(unittest.TestCase):
    def setUp(self):
        pass

    def test_abacus1(self):
        abacus = Abacus.generate_abacus(0)
        self.assertEqual(['|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |'], abacus)

    def test_abacus2(self):
        abacus = Abacus.generate_abacus(8)
        self.assertEqual(['|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00   000*****|'], abacus)

    def test_abacus3(self):
        abacus = Abacus.generate_abacus(32)
        self.assertEqual(['|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000**   ***|',
                          '|00000***   **|'], abacus)

    def test_abacus4(self):
        abacus = Abacus.generate_abacus(147)
        self.assertEqual(['|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000****   *|',
                          '|00000*   ****|',
                          '|000   00*****|'], abacus)

    def test_abacus5(self):
        abacus = Abacus.generate_abacus(986)
        self.assertEqual(['|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|0   0000*****|',
                          '|00   000*****|',
                          '|0000   0*****|'], abacus)

    def test_abacus6(self):
        abacus = Abacus.generate_abacus(5821)
        self.assertEqual(['|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000   *****|',
                          '|00   000*****|',
                          '|00000***   **|',
                          '|00000****   *|'], abacus)

    def test_abacus7(self):
        abacus = Abacus.generate_abacus(1234)
        self.assertEqual(['|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000****   *|',
                          '|00000***   **|',
                          '|00000**   ***|',
                          '|00000*   ****|'], abacus)

    def test_abacus8(self):
        abacus = Abacus.generate_abacus(999)
        self.assertEqual(['|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|0   0000*****|',
                          '|0   0000*****|',
                          '|0   0000*****|'], abacus)

    def test_abacus9(self):
        abacus = Abacus.generate_abacus(13)
        self.assertEqual(['|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000****   *|',
                          '|00000**   ***|'], abacus)

    def test_abacus10(self):
        abacus = Abacus.generate_abacus(49)
        self.assertEqual(['|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*****   |',
                          '|00000*   ****|',
                          '|0   0000*****|'], abacus)