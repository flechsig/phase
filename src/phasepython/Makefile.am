# File      : /afs/psi.ch/user/f/flechsig/phase/src/phasepython/Makefile.am
# Date      : <18 Aug 17 12:13:21 flechsig> 
# Time-stamp: <18 Aug 17 12:26:30 flechsig> 
# Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

# $Source$ 
# $Date$
# $Revision$ 
# $Author$ 

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

## define the directory where phase related python functions should be installed 
## this directory should be in your python search path

pythondir=$(prefix)/python
SLSDEST=gfalc.psi.ch:/work/sls/lib/X_OL_PYTH/ # gfalc accepts kerberos ssh 

## everything 2b installed in idldir
python_DATA  = phase.py \
               phase_test.py

slsinstall:
	rsync -v *.py $(SLSDEST)

docs:
	echo "in case of error:"
	echo "login to slsbl.web.psi.ch and run /home/httpd/htdocs/docs/python/myrun.sh"
	ssh slsbl.web.psi.ch /home/httpd/htdocs/docs/python/myrun.sh


# end Makefile.am