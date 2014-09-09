#!/bin/sh
#   File      : /afs/psi.ch/user/f/flechsig/bin/apps_home.sh
#   Date      : <12 Dec 03 16:06:14 flechsig> 
#   Time-stamp: <09 Sep 14 12:14:35 flechsig> 

#   $Source$ 
#   $Date$
#   $Revision$ 
#   $Author$ 

# ******************************************************************************
#
#   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
#                      Paul Scherrer Institut Villigen, Switzerland
#   
#   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
#          Uwe Flechsig,    uwe.flechsig@psi.ch
#
# ------------------------------------------------------------------------------
#
#   This file is part of PHASE.
#
#   PHASE is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, version 3 of the License, or
#   (at your option) any later version.
#
#   PHASE is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
#
# ******************************************************************************


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