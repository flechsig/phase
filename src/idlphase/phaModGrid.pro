


pro phaModGridSizeAddZeros,bl,nz2,ny2
;+
; NAME:
;	phaModGridSizeAddZeros
;
; PURPOSE:
;       Adds a rim of zeros to the grid
;
; CATEGORY:
;	pro : pha4idl - modify grid
;
; CALLING SEQUENCE:
;	phaModGridSizeAddZeros,source4,nz2,ny2
;
; INPUTS:
;     	bl:	pha4idl beamline structure (see phainit_structures.pro)
;	nz2	new number of gridpoints in z
;	ny2	new number of gridpoints in y
;
; OUTPUTS:
;     	bl:	modified pha4idl beamline structure
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;	nz2 and ny2 have to be bigger than their original values
;
; MODIFICATION HISTORY:
;      March 28, 2008, TL, added help
;
;-

np=n_params()
IF np NE 3 THEN BEGIN 
   print,''
   print,'Wrong Number of Arguments in procedure:  phaModSizeAddZeros[,src4,nz,ny] '
   print,'ArgCount is ',np,',    but should be     3.'
   print,'Returning to IDL-Level 0 (main) ...'
   print,''
   retall
ENDIF

IF nz2 gt 256 THEN BEGIN 
   print,''
   print,'nz2 greater than 256 is not allowed in phaModSize(src4,nz,ny) '
   print,'Returning to IDL-Level 0 (main) ...'
   print,''
   retall
ENDIF
IF ny2 gt 256 THEN BEGIN 
   print,''
   print,'ny2 greater than 256 is not allowed in phaModSize(src4,nz,ny) '
   print,'Returning to IDL-Level 0 (main) ...'
   print,''
   retall
ENDIF


nz2=long(nz2)
ny2=long(ny2)

result=1
result = call_external(!phalib,'phaModSizeAddZeros',$
			bl.src.so4,  $
			nz2, ny2, $
			/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

;print, '***',result,'  ***'
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaModGridSizeCut,bl,nzmin,nzmax,nymin,nymax
;+
; NAME:
;	phaModGridSizeCut
;
; PURPOSE:
;       Cuts the grid, reducing the size and the number of points.
;
; CATEGORY:
;	pro : pha4idl - modify grid
;
; CALLING SEQUENCE:
;	phaModGridSizeCut,bl,nzmin,nzmax,nymin,nymax
;
; INPUTS:
;     	bl:	pha4idl beamline structure (see phainit_structures.pro)
;	nzmin	lower edge to be cut in z
;	nzmax	upper edge to be cut in z
;	nymin	lower edge to be cut in y
;	nymax	upper edge to be cut in y
;
; OUTPUTS:
;     	bl:	modified pha4idl beamline structure
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
;-
np=n_params()
IF np NE 5 THEN BEGIN 
   print,''
   print,'Wrong Number of Arguments in procedure:  '
   print,'          phaModSizeCut[,src4,nzmin,nzmax,nymin,nymax] '
   print,'ArgCount is ',np,',    but should be     5.'
   print,'Returning to IDL-Level 0 (main) ...'
   print,''
   retall
ENDIF

nzmax=long(nzmax)
nymax=long(nymax)
nzmin=long(nzmin)
nymin=long(nymin)

result=1
;   int phaModSizeCut(struct source4 *beam4, int *nzmax,int *nzmin,int *nymax,int *nymin)

result = call_external(!phalib,'phaModSizeCut',$
			bl.src.so4,  $
			nzmin,nzmax,nymin,nymax, $
			/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

;print, '***',result,'  ***'
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaModGridPoints,bl,nz2,ny2
;+
; NAME:
;	phaModGridPoints
;
; PURPOSE:
;       Changes the number of gridpoints used to describe the EMFields.
;		the size of the grid is conserved.
;
; CATEGORY:
;	pro : pha4idl - modify grid
;
; CALLING SEQUENCE:
;	phaModGridPoints,bl,nz2,ny2
;
; INPUTS:
;     	bl:	pha4idl beamline structure (see phainit_structures.pro)
;	nz2	new number of gridpoints in z
;	ny2	new number of gridpoints in y
;
; OUTPUTS:
;     	bl:	modified pha4idl beamline structure
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
;-

np=n_params()
IF np NE 3 THEN BEGIN 
   print,''
   print,'Wrong Number of Arguments in procedure: phaModGrid [,src4,nz2,ny2] '
   print,'ArgCount is ',np,',    but should be     3.'
   print,'Returning to IDL-Level 0 (main) ...'
   print,''
   retall
ENDIF

nz2=long(nz2)
ny2=long(ny2)

result=1
result = call_external(!phalib,'phaModGrid',$
			bl.src.so4,  $
			nz2, ny2, $
			/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

;print, '***',result,'  ***'
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
