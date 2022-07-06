#!/Users/pjjokine/anaconda/bin/python3

import curses
import os
import signal
from time import sleep
stdscr = curses.initscr()

curses.noecho()

begin_x = 20; begin_y = 7
height = 5; width = 40
win = curses.newwin(height, width, begin_y, begin_x)

count = 5

for x in range(0,20):
  for y in range (0,5):
    win.addstr(y, x, "a", curses.A_BLINK)
  win.refresh()
sleep(3)

#signal.pause()

curses.nocbreak()
stdscr.keypad(False)
curses.echo()

curses.endwin()
