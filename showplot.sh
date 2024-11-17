#!/bin/sh
filename=$1
/c/Tools/gnuplot/bin/gnuplot -e filename=\'${filename}\' work.gnuplot
