c  PHASE DRIFT_LIBRARIES
c
c Mit folgendem Befehl wird das "ATnT_port3_driftroutines.o"  Objekt erzeugt, 
c  das in Phase zur Berechnung der Drifts via FFT verwendet wird.  
c 
c   ifort -shared -fPIC -vms -o ATnT_port3_driftroutines.o -c pha_driftroutines.for
c
c     
      include 'drift_routines_02.f'
      include 'drift_routines_03.f'
      include 'drift_routines_04.f'
      include 'drift_routines_05.f'
      include 'drift_routines_06.f'
      include 'drift_routines_07.f'
      include 'drift_routines_08.f'
      include 'drift_routines_09.f'
      include 'drift_routines_10.f'
c      include 'drift_routines_11.f'
      include 'drift_routines_12.f'
      include 'drift_routines_13.f'
      include 'drift_routines_14.f'
      include 'drift_routines_15.f'
      include 'drift_routines_16.f'
      include 'drift_routines_17.f'
      include 'drift_routines_18_no_machine_consts.f'

c      include 'drift_routines_19A.f'         
c      include 'drift_routines_20.f'         


c***************************************************************************
ccccccc  Hier werden Machinen-Konstanten definiert und ueberprueft ...cccccc
c***************************************************************************

c Verbesserte neue Version
c   Test der Konstanten in s..mach.f       
      include 'port/s1mach.f'               
      include 'port/s2mach.f'
      include 'port/s3mach.f'               
c  Real Konstanten      
      include 'port/r1mach.f'               
c  Double Konstanten      
	include 'port/d1mach.f'
c  Integer Konstanten      
	include 'port/i1mach.f'
c Sonstiges abhaengiges
c      include 'port/iflr.f'
c      include 'port/iceil.f'
	include 'port/s88fmt.f'


c	Alte OriginalVersion ... 
c	(falls benutzt, '*18_no_machines_consts.f' oben auskommentieren)
c      include 'drift_routines_18A.f'


               