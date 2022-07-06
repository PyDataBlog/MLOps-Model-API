#! /home/nsanthony/miniconda3/bin/python
import inventory.inventory_class as inv
import weapons.weapon_class as wp



class people:
    """This is the people class with attributes:"""
    def name():
        n = ''
        return n
    def health():
        hp = 0
        return hp
    def descript():
        d = 'Description of the person or creature'
        return d
    def equiped():
        e = inv.inventory()
        e.weapon = wp.weapon()
        e.armor = 0
        return e
    def bag():
        b = {}
        return b
    def hostile():
        h = 0
        return h