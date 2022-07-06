#!/usr/bin/python

import json 

f = file('treasures.json', 'r')
try:
    foo = json.load(f)
    json_contents = foo
except ValueError:
    json_contents = dict()
f.close()

print 'Type \'q\' to [q]uit'
while True:
    name = raw_input('Treasure Name: ')
    if name == 'q':
        break
    print 'Type \'n\' to stop entering heroes and go to [n]ext treasure'
    set_contents = dict()
    hero = ''
    while True:
        hero = raw_input('Hero name: ')
        if hero == 'n' or hero == 'q':
            break
        else:
            bundle_rating = raw_input('Item set rating [1-3]: ')
            set_contents[hero] = bundle_rating
    json_contents[name] = set_contents
    if hero == 'q':
        break

f = open('treasures.json', 'w')
json.dump(json_contents, f, indent=4)
f.close()