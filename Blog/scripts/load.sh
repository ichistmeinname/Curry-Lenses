#!/bin/sh
# shell script to load the complete application into the interactive
# Curry environment in order to test individual functions.

# Definition of Curry installation to be used:
CURRYEXEC=/Users/ichistmeinname/Documents/programming/kics2/bin/kics2

LENSPATH=Users/ichistmeinname/Documents/programming/Curry-Lenses
ORIGDIR=`pwd`                          # original directory (builtin)
PROGNAME=`type $0 | awk '{print $3}'`  # search for executable on path
PROGDIR=`dirname $PROGNAME`            # extract directory of program
PROGNAME=`basename $PROGNAME`          # base name of program
ABSPROGDIR="`cd \"$PROGDIR\" 2>/dev/null && pwd || echo \"$PROGDIR\"`"  # get absolute path

CURRYPATH="$ORIGDIR/views:$ORIGDIR/controllers:$ORIGDIR/models:$ORIGDIR/system:$ORIGDIR/config:$LENSPATH"
export CURRYPATH

exec $CURRYEXEC :l Main
