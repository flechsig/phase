


pro phaModGridSizeAddZeros,source4,nz2,ny2

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
			source4,  $
			nz2, ny2, $
			/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

;print, '***',result,'  ***'
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaModGridSizeCut,source4,nzmin,nzmax,nymin,nymax

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
			source4,  $
			nzmin,nzmax,nymin,nymax, $
			/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

;print, '***',result,'  ***'
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaModGridPoints,source4,nz2,ny2

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
			source4,  $
			nz2, ny2, $
			/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

;print, '***',result,'  ***'
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
