;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function phasrc4add, beam1, beam2

; Calculates the difference of the EM-fields in 2 Beam-Structures
; Assuming that beam2 has the same Grid-Parameters as beam1

beamsum={source4}
beamsum=beam1
	
beamsum.zeyre=beam1.zeyre+beam2.zeyre
beamsum.zeyim=beam1.zeyim+beam2.zeyim
beamsum.zezre=beam1.zezre+beam2.zezre
beamsum.zezim=beam1.zezim+beam2.zezim

return, beamsum

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function phasrc4diff, beam1, beam2

; Calculates the difference of the EM-fields in 2 Beam-Structures
; Assuming that beam2 has the same Grid-Parameters as beam1

beamdiff={source4}
beamdiff=beam1
	
beamdiff.zeyre=beam1.zeyre-beam2.zeyre
beamdiff.zeyim=beam1.zeyim-beam2.zeyim
beamdiff.zezre=beam1.zezre-beam2.zezre
beamdiff.zezim=beam1.zezim-beam2.zezim

return, beamdiff

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro phaLabelSrc4, beam, name

; Labels the fsource4(a-d)-Tags

;label = bytearr(80)
;label(0:strlen(name)-1 = byte(name) 

beam.fsource4a=bytarr(80)
beam.fsource4b=bytarr(80)
beam.fsource4c=bytarr(80)
beam.fsource4d=bytarr(80)

beam.fsource4a(0:strlen(name+'_a')-1) = byte(name+'_a') 
beam.fsource4b(0:strlen(name+'_b')-1) = byte(name+'_b')
beam.fsource4c(0:strlen(name+'_c')-1) = byte(name+'_c')
beam.fsource4d(0:strlen(name+'_d')-1) = byte(name+'_d')

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro phaModSizeAddZeros,source4,nz2,ny2

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

pro phaModSizeCut,source4,nzmin,nzmax,nymin,nymax

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

pro phaModGrid,source4,nz2,ny2

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
