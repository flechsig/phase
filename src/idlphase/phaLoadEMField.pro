

;; filename:  phaLoadEMField.pro

function phaLoadEMField, MainFileName, lambda, _EXTRA = e
;+
; NAME:
;	phaLoadEMField
;
; PURPOSE:
;       Loads phasestyle EMFields and creates 
;         a src4 beam structure from it,
;       sets wavelength in struct,
;       optionally scales dimensions of field
;
; CATEGORY:
;	pro : pha4idl - create src4
;
; CALLING SEQUENCE: 
;	phaLoadEMField(MainFileName, lambda)
;
; INPUTS:
;	      MainFileName	prefix for the phase EMField files
;			    phase adds the following postfixes:
;			    -ezre, -ezim, -eyre, -eyim  : real and imaginary 
;				  of the EM-fields with z & y polarizaiton
;
;       lambda  wavelength in nm
;
;       [optional]: SCALE=[double] scaling factor for field dimensions
;                   default is 1.0, i.e. units in file are interpreted as [mm]
;                   (often file format is in [m], therefore SCALE=10^3 is needed)
; OUTPUTS:
;     	beam		filled source4 struct
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      Nov. 1, 2009, SG, initial version
;
;-

np=n_params()

if np lt 2 then begin
	print, 'Too few number of arguments ...'
	print, 'Usage:phaSrcLoadEMField(MainFileName[string], lambda[double], {SCALE=[double]}) '
	print, ''
	retall
endif

beam={source4}
phaLoadEzReal,beam,MainFileName+'-ezrec', _EXTRA = e
phaLoadEzImag,beam,MainFileName+'-ezimc', _EXTRA = e
phaLoadEyReal,beam,MainFileName+'-eyrec', _EXTRA = e
phaLoadEyImag,beam,MainFileName+'-eyimc', _EXTRA = e

beam.xlam = lambda;
return, beam
END


PRO phaLoadEzReal, beam, fname, scale = SC

; init source4:  beam = {source4}

if KEYWORD_SET(SC) then scale=SC $
  else scale=1.0;

nx=long(0);
ny=long(0);
rows=long(0);

OpenR, lun, fname, /get_lun
readf, lun, nx, ny
cols=3
rows= nx * ny
data=dblarr(cols,rows)
readf, lun, data
Free_Lun, lun

beam.iezrex   = long(nx)
beam.xezremin = data(0,0)*scale;
beam.xezremax = data(0,rows-1)*scale;
beam.dxezre   = (beam.xezremax-beam.xezremin)/(beam.iezrex-1)

beam.iezrey=long(ny)
beam.yezremin = data(1,0)*scale;
beam.yezremax = data(1,rows-1)*scale;
beam.dyezre   = (beam.yezremax-beam.yezremin)/(beam.iezrey-1)


;print,'nx ',nx,' ny ',ny,' cols ',cols,' rows ',rows
;print,'x/y-min/max ',beam.xezremin,beam.yezremin,beam.xezremax,beam.yezremax
print,'Read EzReal from : ',fname



j = long(0); need expliyit long declaration for cases bigger than 16Bit
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		beam.zezre(ix,iy)=data(2,j)
		j=j+1
	endfor    
endfor

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaLoadEzImag, beam, fname, scale = SC

; init source4:  beam = {source4}

if KEYWORD_SET(SC) then scale=SC $
  else scale=1.0;

nx=long(0);
ny=long(0);
rows=long(0);

OpenR, lun, fname, /get_lun
readf, lun, nx, ny
cols=3
rows= nx * ny
data=dblarr(cols,rows)
readf, lun, data
Free_Lun, lun

beam.iezimx   = long(nx);
beam.xezimmin = data(0,0)*scale;
beam.xezimmax = data(0,rows-1)*scale;
beam.dxezim   = (beam.xezimmax-beam.xezimmin)/(beam.iezimx-1)

beam.iezimy=long(ny)
beam.yezimmin = data(1,0)*scale;
beam.yezimmax = data(1,rows-1)*scale;
beam.dyezim   = (beam.yezimmax-beam.yezimmin)/(beam.iezimy-1)


;print,'nx ',nx,' ny ',ny,' cols ',cols,' rows ',rows
;print,'x/y-min/max ',beam.xezimmin,beam.yezimmin,beam.xezimmax,beam.yezimmax

print,'Read EzImag from : ',fname

j = long(0); need expliyit long declaration for cases bigger than 16Bit
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		beam.zezim(ix,iy)=data(2,j)
		j=j+1
	endfor    
endfor

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO phaLoadEyReal, beam, fname, scale = SC

; init source4:  beam = {source4}

if KEYWORD_SET(SC) then scale=SC $
  else scale=1.0;

nx=long(0);
ny=long(0);
rows=long(0);

OpenR, lun, fname, /get_lun
readf, lun, nx, ny
cols=3
rows= nx * ny
data=dblarr(cols,rows)
readf, lun, data
Free_Lun, lun

beam.ieyrex   = long(nx)
beam.xeyremin = data(0,0)*scale;
beam.xeyremax = data(0,rows-1)*scale;
beam.dxeyre   = (beam.xeyremax-beam.xeyremin)/(beam.ieyrex-1)

beam.ieyrey=long(ny)
beam.yeyremin = data(1,0)*scale;
beam.yeyremax = data(1,rows-1)*scale;
beam.dyeyre   = (beam.yeyremax-beam.yeyremin)/(beam.ieyrey-1)


;print,'nx ',nx,' ny ',ny,' cols ',cols,' rows ',rows
;print,'x/y-min/max ',beam.xeyremin,beam.yeyremin,beam.xeyremax,beam.yeyremax
print,'Read EyReal from : ',fname

j = long(0); need expliyit long declaration for cases bigger than 16Bit
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		beam.zeyre(ix,iy)=data(2,j)
		j=j+1
	endfor    
endfor


END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaLoadEyImag, beam, fname, scale = SC

; init source4:  beam = {source4}

if KEYWORD_SET(SC) then scale=SC $
  else scale=1.0;

nx=long(0);
ny=long(0);
rows=long(0);

OpenR, lun, fname, /get_lun
readf, lun, nx, ny
cols=3
rows= nx * ny
data=dblarr(cols,rows)
readf, lun, data
Free_Lun, lun

beam.ieyimx   = long(nx)
beam.xeyimmin = data(0,0)*scale;
beam.xeyimmax = data(0,rows-1)*scale;
beam.dxeyim   = (beam.xeyimmax-beam.xeyimmin)/(beam.ieyimx-1)

beam.ieyimy=long(ny)
beam.yeyimmin = data(1,0)*scale;
beam.yeyimmax = data(1,rows-1)*scale;
beam.dyeyim   = (beam.yeyimmax-beam.yeyimmin)/(beam.ieyimy-1)


;print,'nx ',nx,' ny ',ny,' cols ',cols,' rows ',rows
;print,'x/y-min/max ',beam.xeyimmin,beam.yeyimmin,beam.xeyimmax,beam.yeyimmax
print,'Read EyImag from : ',fname

j = long(0); need expliyit long declaration for cases bigger than 16Bit
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		beam.zeyim(ix,iy)=data(2,j)
		j=j+1
	endfor    
endfor


END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
