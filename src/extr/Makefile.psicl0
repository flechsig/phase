# File      : /home/vms/flechsig/vms/phas/extr.dir/makefile
# Date      : <29 Oct 97 16:12:39 flechsig> 
# Time-stamp: <13 Feb 98 13:32:06 flechsig> 
# Author    : Uwe Flechsig, flechsig@exp.bessy.de
#
# Datei: DISK_201_SRC0:[FLECHSIG.PHASE.OPTI]MAKEFILE.
# Datum: 19.JUL.1994
# Stand: 23-APR-1996                                           
# Autor: FLECHSIG, BESSY Berlin
#########################################################################
# Makefile for the PHASE EXTRACTION 
#
# make commands for different systems
#
# for DEC VAX   with VMS "make vax"
# for DEC Alpha with VMS "make axp"
# remove *.obj, *.exe    "make clear"
#########################################################################
.phony: all makewhat    
MAKE= make/noveri         # no debug messages
#
#######          extensions                                    ##########
#######          additional extension on AXP systems        #############
ALPH		= _axp            
#######                    basic extension               ################
EXE		= exe
OBJ             = obj
LIB		= olb      
#########################################################################
#               debugging        
DEBUG		= no           # comment out for debugging (DEBUG = # no)    
OPTIMIZE	= #no
#########        flags                                    ###############
CFLAGS	 	= /$(DEBUG)debug/$(OPTIMIZE)OPTIMIZE/OBJECT=     
FFLAGS   	= /$(DEBUG)debug/$(OPTIMIZE)OPTIMIZE/OBJECT= 
UILFLAGS	= /motif/output=
LINKFLAGS	= /$(DEBUG)debug/executable=  
###### system dependent flags                          ################## 
CFLAGSVAX 	= $(CFLAGS)
CFLAGSAXP	= /STANDARD=VAXC$(CFLAGS)  
FFLAGSVAX 	= $(FFLAGS)
FFLAGSAXP	= /ALIGNMENT=COMMONS=MULTILANGUAGE$(FFLAGS)    
#########################################################################
#               variables 
PROGRAM		= phaseextract  
DESTDIR		= extr$prg:  
# 
PROGRAM		= $(DESTDIR)$(PROGRAM).$(EXE)
#########################################################################
OLIB		= phase$prg:phaseoptilib.$(LIB)
#
PHOLIAXP	= DISK_201_SRC0:[flechsig.phas.phasec]cutils.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]activate_proc.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]phasec.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]bline.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]phasefor.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]rtrace.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]PHASEgraf.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]geometrypck.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]initdatset.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]mirrorpck.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]PHASEgraffor.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasec]pst.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasefor]phase_integration_10.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.phasefor]phase_source_10.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.opti]treiber.$(OBJ)$(ALPH),-
DISK_201_SRC0:[flechsig.phas.opti]optisubc.$(OBJ)$(ALPH),-
DISK_201_SRC0:[FLECHSIG.PHAS.PHASEC]PHASE0.$(OBJ)$(ALPH)
#########################################################################
LIBS	 	= cern_lib:MATHLIB.$(LIB)/LIB,-
PACKLIB/LIB,-
graflib/lib,-
grafx11/lib,-
PACKLIB/LIB,-
KERNLIB/LIB  
####           system dependent libs ####################################
LIBSVAX		= $(LIBS),-
DISK_201_SRC0:[flechsig.phas.phasefor.lib]phaselib.$(LIB)/lib,- 
DISK_201_SRC0:[flechsig.phas.phasec]cutils.$(OBJ),- 
batchdisk:xliblink/opt  

LIBSAXP	 	= $(OLIB)$(ALPH)/lib,-
$(LIBS),-
DISK_201_SRC0:[flechsig.phas.phasefor.lib]phaselib.$(LIB)$(ALPH)/lib,- 
batchdisk:xliblink_axp.opt/opt   
#########################################################################
OBJECTSVAX 	= phaseopti.$(OBJ),treiber.$(OBJ),-
optisubc.$(OBJ),-
cost.$(OBJ)         

OBJECTSAXP 	= phaseextract.$(OBJ)$(ALPH),cost.$(OBJ)$(ALPH)  
#########################################################################
#                            targets                                    # 
all: makewhat

makewhat: 
        arch_name = f$getsyi("ARCH_NAME")
	if arch_name .eqs. "Alpha"
	then 
		write sys$output "Making PHASEEXTRACT for AXP..." 
		$(MAKE) $(PROGRAM)$(ALPH)   
	else
		write sys$output "Making PHASEEXTRACT for VAX..."  
		$(MAKE) $(PROGRAM)   
	endif

##############################################################################
#
# Dependencies section
# for vax
$(PROGRAM): $(OBJECTSVAX)
	link$(LINKFLAGS)$(PROGRAM) $(OBJECTSVAX),$(LIBSVAX)  

# for axp
$(PROGRAM)$(ALPH): $(OBJECTSAXP) $(OLIB)$(ALPH)   
	link$(LINKFLAGS)$(PROGRAM)$(ALPH) $(OBJECTSAXP),$(LIBSAXP)  
##########################################################################
$(OLIB)$(ALPH): $(PHOLIAXP)
	lib/create $(OLIB)$(ALPH) $(PHOLIAXP)

*.$(OBJ) : *.c 
	cc $(CFLAGSVAX)$@ $< 

*.$(OBJ)$(ALPH) : *.c 
	cc $(CFLAGSAXP)$@ $< 

*.$(OBJ) : *.for 
	fortran $(FFLAGSVAX)$@ $<   

*.$(OBJ)$(ALPH): *.for 
	fortran $(FFLAGSAXP)$@ $<   
##########################################################################
clear:
	write sys$output "delete objects and exe..."  
	arch_name = f$getsyi("ARCH_NAME")
	if arch_name .eqs. "Alpha"
	then 
		del/nocon *.$(OBJ)$(ALPH);*,$(PROGRAM)$(ALPH);*
	else
		del/nocon *.$(OBJ);*,$(PROGRAM);*   
	endif
############################################################################
#
# make phase for different systems
#

# DEC VAX systems with VMS 
vax:
	write sys$output "Making PHASEEXTRACT for VAX..."  
        $(MAKE) $(PROGRAM)   

# DEC Alpha systems with VMS 
alpha: axp
axp:
	write sys$output "Making PHASEEXTRACT for AXP..." 
        $(MAKE) $(PROGRAM)$(ALPH)   
####################      end makefile       ###############################
#/home/vms/flechsig/vms/phas/extr.dir/makefile
