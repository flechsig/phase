
; file: phaSetSources.pro



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSetSrcPhaseSpace, bl, iy, ymin, ymax, iz, zmin, zmax
;+
; NAME:
;	phaSetSrcPhaseSpace
;
; PURPOSE:
;       Set phase space src internal grid for source from file
;
;
; CATEGORY:
;	pro : pha4idl - beamline/calculation options
;
; CALLING SEQUENCE:
;	phaSetSrcPhaseSpace, bl, iy, ymin, ymax, iz, zmin, zmax
;
; INPUTS:
;	bl		pha4idl beamline structure
;	iy		no. grid points in y
;	ymin		lower border in y
;	ymax		upper border in y
;	iz		no. grid points in z
;	zmin		lower border in z
;	zmax		upper border in z
;
; OUTPUTS:
;     	bl:		pha4idl beamline structure with new options
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      March 28, 2008, TL, added help
;
bl.SourceType=byte('I')
bl.PSImage.iy=iy
bl.PSImage.ymin=ymin
bl.PSImage.ymax=ymax
bl.PSImage.iz=iz
bl.PSImage.zmin=zmin
bl.PSImage.zmax=zmax
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PRO phaSetSrcUndulator, bl,
;
;END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSetPSsource, bl  ,isrctype $
,isrcy,isrcdy,sigmay,sigmayp,isrcz,isrcdz,sigmaz,sigmazp $ ; so1 stuff
,dipcy,dipcz,dipdisy,dipdisz $ ; so5 stuff
,pin_yl0,pin_yl,pin_zl0,pin_zl ; other
;+
; NAME:
;	phaSetPSsource
;
; PURPOSE:
;       Set phase space source type and parameter ????????????????????
; ?????????????????????ßß
;	not needed if source fields from file or idl are used
;
;
; CATEGORY:
;	pro : pha4idl - beamline options
;
; CALLING SEQUENCE:
;	phaSetPSsource, bl  ,isrctype $
;	,isrcy,isrcdy,sigmay,sigmayp,isrcz,isrcdz,sigmaz,sigmazp $ ; so1 stuff
;	,dipcy,dipcz,dipdisy,dipdisz $ ; so5 stuff
;	,pin_yl0,pin_yl,pin_zl0,pin_zl ; other
;
; INPUTS:
;	bl		pha4idl beamline structure
;	isrctype
;	isrcy
;	isrcdy
;	sigmay
;	sigmayp
;	isrcz
;	isrcdz
;	sigmaz
;	sigmazp
;	dipcy
;	dipcz
;	dipdisy
;	dipdisz
;	pin_yl0
;	pin_yl
;	pin_zl0
;	pin_zl
;
; OUTPUTS:
;     	bl:		pha4idl beamline structure with new options
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      March 28, 2008, TL, added help
;
; TODO:
;	setup defines for the different sourcetypes
;	 (like for the opt.elements in phainit_defines.pro, numbers to be found in phase.h)
; 	purpose unclear
;-

bl.src.isrctype=isrctype
bl.src.so1.isrcy=isrcy
bl.src.so1.isrcdy=isrcdy
bl.src.so1.sigmay=sigmay
bl.src.so1.sigmayp=sigmayp
bl.src.so1.isrcz=isrcz
bl.src.so1.isrcdz=isrcdz
bl.src.so1.sigmaz=sigmaz
bl.src.so1.sigmazp=sigmazp
;bl.src.so4.fsource4a
;bl.src.so4.fsource4b
;bl.src.so4.fsource4c
;bl.src.so4.fsource4d
bl.src.so5.dipcy=dipcy
bl.src.so5.dipcz=dipcz
bl.src.so5.dipdisy=dipdisy
bl.src.so5.dipdisz=dipdisz
;bl.src.so6.fsource6
bl.src.pin_yl0=pin_yl0
bl.src.pin_yl=pin_yl
bl.src.pin_zl0=pin_zl0
bl.src.pin_zl=pin_zl
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



