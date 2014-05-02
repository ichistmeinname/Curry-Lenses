#!/bin/sh

# Definition of Curry installation to be used:
CURRYEXEC=/Users/ichistmeinname/Documents/programming/kics2/bin/kics2

LENSBIB=/Users/ichistmeinname/Documents/programming/Curry-Lenses
LENSPATH=$LENSBIB:$LENSBIB/CombinatorialLenses
ORIGDIR=`pwd`                          # original directory (builtin)
PROGNAME=`type $0 | awk '{print $3}'`  # search for executable on path
PROGDIR=`dirname $PROGNAME`            # extract directory of program
ABSPROGDIR="`cd \"$PROGDIR\" 2>/dev/null && pwd || echo \"$PROGDIR\"`"  # get absolute path

CURRYPATH=$ORIGDIR/views:$ORIGDIR/controllers:$ORIGDIR/models:$ORIGDIR/system:$ORIGDIR/config:$LENSPATH:$CURRYPATH
export CURRYPATH

exec $CURRYEXEC :l Main :quit
