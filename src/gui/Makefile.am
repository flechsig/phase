#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <02 Feb 04 17:09:04 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

pkgdata_DATA = phase.uid
##phase_SOURCES = phase.uil 
phase_uid_DEPENDENCIES= phase.uil geometrybox.uil mirrorbox.uil \
	    grafbox.uil filemenu.uil commandsmenu.uil \
	    phasevalue.uil editmenu.uil sourcebox.uil \
	    helpmenu.uil optibox.uil blbox.uil

phase.uid: $(phase_uid_DEPENDENCIES)
	$(UIL) -o phase.uid phase.uil
