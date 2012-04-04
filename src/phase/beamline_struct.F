c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/beamline_struct.F
c$$$ Date      : <04 Apr 12 11:54:45 flechsig> 
c$$$ Time-stamp: <04 Apr 12 11:57:50 flechsig> 
c$$$ Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c----------------------------------------------------------------
c                define the beamline struct in fortran
c                use same name as in phase.h with a leading "f"
c                sort substructures in alphabetic order
c----------------------------------------------------------------
        structure/fgeometrytype/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
	   integer idefl   
        end structure 
c end beamline_struct.F
