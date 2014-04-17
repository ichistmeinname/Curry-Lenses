#!/bin/sh

# target directory where the compiled cgi program should be stored:
WEBSERVERDIR=/Applications/MAMP/htdocs/blog
# makecurrycgi script to deploy the system (included in the PAKCS distribution):
MAKECURRYPATH=/Users/ichistmeinname/Documents/programming/kics2/bin/makecurrycgi

##########################################################################
# here starts the standard script:

# create web directory if necessary:
if [ ! -d $WEBSERVERDIR ] ; then
  echo "Creating web directory '$WEBSERVERDIR'..."
  mkdir $WEBSERVERDIR
  chmod 755 $WEBSERVERDIR
fi

LENSPATH=/Users/ichistmeinname/Documents/programming/Curry-Lenses
ORIGDIR=`pwd`                          # original directory (builtin)
CODEPATH=$ORIGDIR/views:$ORIGDIR/controllers:$ORIGDIR/models:$ORIGDIR/system:$ORIGDIR/config:$LENSPATH

CURRYPATH=$CODEPATH
export CURRYPATH

$MAKECURRYPATH -m main -o $WEBSERVERDIR/spicey.cgi Main.curry

# copy other files (Stylesheets, Images...)
cp -r $ORIGDIR/public/* $WEBSERVERDIR
chmod -R go+rX $WEBSERVERDIR
