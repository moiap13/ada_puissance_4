#!/bin/bash
#/usr/local/gnat/bin/gcc -c ncurses.c
#/usr/local/gnat/bin/gnatmake -c puissance_4_gui.adb
#/usr/local/gnat/bin/gnatbind puissance_4_gui.ali
#/usr/local/gnat/bin/gnatlink puissance_4_gui.ali ncurses.o -lncurses
#./puissance_4_gui


gcc -c ncurses.c
gnatmake -c puissance_4_gui.adb
gnatbind puissance_4_gui.ali
gnatlink puissance_4_gui.ali ncurses.o -lncurses
./puissance_4_gui