
; Name: phaSaveEMField.pro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO phaSaveEMField, bl, MainFileName
;+
; NAME:
;	phaSaveEMField
;
; PURPOSE:
;       Save phasestyle EMFields from beamline
;
; CATEGORY:
;	pro : pha4idl - beamline
;
; CALLING SEQUENCE:
;	phaSaveEMField, bl, MainFileName
;
; INPUTS:
;     	bl		beamline struct
;	MainFileName	prefix for the phase EMField files
;			phase adds the following postfixes:
;			-ezre, -ezim, -eyre, -eyim  : real and imaginary 
;				of the EM-fields with z & y polarizaiton
;
; OUTPUTS:
;     	phase style EMField files on hdd
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

if np ne 2 then begin
	print, 'Wrong Number of arguments ...'
	print, 'Usage: SavePhaseResults, beam[source4], MainFileName[string] '
	print, ''
	return
endif

phaSaveEzReal,bl.src.so4,MainFileName+'-ezrec'
phaSaveEzImag,bl.src.so4,MainFileName+'-ezimc'
phaSaveEyReal,bl.src.so4,MainFileName+'-eyrec'
phaSaveEyImag,bl.src.so4,MainFileName+'-eyimc'

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSaveEzReal, beam, fname

; init source4:  beam = {source4}



nx = beam.iezrex
ny = beam.iezrey

cols=3
rows= nx * ny

data=dblarr(cols,rows)

j=0
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		data(0,j)= beam.xezremin + ix * beam.dxezre ; x-coord
		data(1,j)= beam.yezremin + iy * beam.dyezre ; y-coord
		data(2,j)= beam.zezre(ix,iy)                ; field 
		j=j+1
	endfor    
endfor

OpenW, lun, fname, /get_lun

printf, lun, nx , ny ; field dimension

printf, lun, data    ; data

Free_Lun, lun

print,'Saved EzReal to : ',fname

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSaveEzImag, beam, fname

; init source4:  beam = {source4}



nx = beam.iezimx
ny = beam.iezimy

cols=3
rows= nx * ny

data=dblarr(cols,rows)

j=0
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		data(0,j)= beam.xezimmin + ix * beam.dxezim ; x-coord
		data(1,j)= beam.yezimmin + iy * beam.dyezim ; y-coord
		data(2,j)= beam.zezim(ix,iy)                ; field 
		j=j+1
	endfor    
endfor

OpenW, lun, fname, /get_lun

printf, lun, nx , ny ; field dimension

printf, lun, data    ; data

Free_Lun, lun

print,'Saved EzImag to : ',fname

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSaveEyReal, beam, fname

; init source4:  beam = {source4}



nx = beam.ieyrex
ny = beam.ieyrey

cols=3
rows= nx * ny

data=dblarr(cols,rows)

j=0
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		data(0,j)= beam.xeyremin + ix * beam.dxeyre ; x-coord
		data(1,j)= beam.yeyremin + iy * beam.dyeyre ; y-coord
		data(2,j)= beam.zeyre(ix,iy)                ; field 
		j=j+1
	endfor    
endfor

OpenW, lun, fname, /get_lun

printf, lun, nx , ny ; field dimension

printf, lun, data    ; data

Free_Lun, lun

print,'Saved EyReal to : ',fname

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSaveEyImag, beam, fname

; init source4:  beam = {source4}



nx = beam.ieyimx
ny = beam.ieyimy

cols=3
rows= nx * ny

data=dblarr(cols,rows)

j=0
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		data(0,j)= beam.xeyimmin + ix * beam.dxeyim ; x-coord
		data(1,j)= beam.yeyimmin + iy * beam.dyeyim ; y-coord
		data(2,j)= beam.zeyim(ix,iy)                ; field 
		j=j+1
	endfor    
endfor

OpenW, lun, fname, /get_lun

printf, lun, nx , ny ; field dimension

printf, lun, data    ; data

Free_Lun, lun

print,'Saved EyImag to : ',fname

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



