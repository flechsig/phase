#!/bin/sh
#   File      : /afs/psi.ch/user/f/flechsig/bin/apps_home.sh
#   Date      : <12 Dec 03 16:06:14 flechsig> 
#   Time-stamp: <12 Dec 03 16:24:11 flechsig> 

#   $Source$ 
#   $Date$
#   $Revision$ 
#   $Author$ 

#
# Determine APPS_HOME
#
if [ -z "$APPS_HOME" ] ; then
    PRG=`type -p $1` >/dev/null 2>&1
    # If PRG is a symlink, trace it to the real home directory

    while [ -L "$PRG" ]
    do
        newprg=`expr "\`/bin/ls -l "$PRG"\`" : ".*$PRG -> \(.*\)"`
        expr "$newprg" : / >/dev/null || newprg="`dirname $PRG`/$newprg"
        PRG="$newprg"
    done

    APPS_HOME=`dirname $PRG`
fi

if [ ! -d "$APPS_HOME" ] ; then
    echo "Invalid APPS_HOME: $APPS_HOME" 1>&2
    exit 1
fi

echo $APPS_HOME
#export APPS_HOME

# end /afs/psi.ch/user/f/flechsig/bin/apps_home.sh