#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <04 Feb 04 09:29:44 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

pkgdata_DATA = phase.uid
##phase_uid_SOURCES = phase.uil 
##phase_uid_DEPENDENCIES = geometrybox.uil mirrorbox.uil \
##	    grafbox.uil filemenu.uil commandsmenu.uil \
##	    phasevalue.uil editmenu.uil sourcebox.uil \
##	    helpmenu.uil optibox.uil blbox.uil

mysources= phase.uil geometrybox.uil mirrorbox.uil \
	    grafbox.uil filemenu.uil commandsmenu.uil \
	    phasevalue.uil editmenu.uil sourcebox.uil \
	    helpmenu.uil optibox.uil blbox.uil


phase.uid: $(mysources)

.uil.uid:
	$(UIL) -o $@ $<	

##phase.uid: $(phase_uid_DEPENDENCIES)
##phase.uid: $(mysources)
##	$(UIL) -o phase.uid phase.uil
