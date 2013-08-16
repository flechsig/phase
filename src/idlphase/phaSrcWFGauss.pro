;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 
;+
; NAME:
;	beispiel

function phaSrcWFGauss, ianzz, zmin, zmax, $
			ianzy, ymin, ymax, $
                        w0, deltax, xlam, $
                        ez0, ey0, dphi_zy, $
                        verbose=verbose, dataformat=dataformat
;+
; NAME:
;	phaSrcWFGauss
;
; PURPOSE:
;       Fill struct src4 with gaussian EMfield distribution
;	z and y polarized components are created
;
;	Ez = gauss(...) * ez0
;	Ey = gauss(...) * ey0 * expi(dphi_zy)
;
;	Ditribution is allways normalized to 1
;
; CATEGORY:
;	func : pha4idl - create src4
;
; CALLING SEQUENCE:
;	src4struct = phaSrcWFGauss( ianzz, zmin, zmax, $
;				    ianzy, ymin, ymax, $
;       	                    w0   , zfoc, xlam, $
;               	            ez0, ey0, dphi_zy )
;
;        e.g: IDL> beam0=phaSrcWFGauss(128,-1,1,128,-1,1,0.2,0,20,1,0,0)
;	      for a completely z polarized field, 
;
; INPUTS:
;	ianzz		no. of gridpoints in z direction
;	zmin		lower border in z [mm]
;	zmax		upper border in z [mm]
;	ianzy		no. of gridpoints in y direction
;	ymin		lower border in y [mm]
;	ymax		upper border in y [mm]
;	w0		waist of the beam in the focus / focal spotsize [mm]
;	zfoc		distance of focus in direction of propagation [mm]
;	xlam		wavelenght in [nm]
;	ez0		scaling paramter for Ez
;	ey0		scaling paramter for Ey
;	dphi_zy		phase beetwen Ez and Ey [rad]
;
; KEYWORD PARAMETERS:
;   verbose:    verbose
;   dataformat: format of output data, 'source4c' or 'source4' (default)
;
; OUTPUTS:
;     	src4struct	struct source4 
;			(eg struct beamline.src.so4)
;
; KEYWORDS:
;	verbose: verbose
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      March 28, 2008, TL, added help
;      Aug 13 UF add verbose, add dataformat
;-



;e.g.... "IDL> beam0=phaSrcWFGauss(128, -1, 1,128, -1, 1, 0.2 , 0 , 20, 1,0,0) "

if n_elements(dataformat) eq 0 then dataformat='source4' else dataformat='source4c'
if n_elements(verbose) ne 0 then print, 'phaSrcWFGauss called, dataformat= ', dataformat

np=n_params()
IF np NE 12 THEN BEGIN 
   print,''
   print,'Wrong Number of Arguments in function:  phaSrcWFGauss '
   print,'ArgCount is ',np,',    but should be    12.'
   print,'Returning to IDL-Level 0 (main) ...'
   print,''
   retall
ENDIF

  ;long(var): convert var to 32bit integer: 
  ;IDL_LONG - C int - ifort integer (*4) -- Standard

ianzy=long(ianzy) 
ianzz=long(ianzz)
ymin=double(ymin)
ymax=double(ymax)
;dy=(ymax-ymin)/double(ianzy)
zmin=double(zmin)
zmax=double(zmax)
;dz=(zmax-zmin)/double(ianzz)
xlam=double(xlam)
w0=double(w0)
deltax=double(deltax)
ez0=double(ez0)
ey0=double(ey0)
dphi_zy=double(dphi_zy)

result = 1

print, '***'
print, 'phaSrcWFGauss started...'

if dataformat eq 'source4' then ;; torstens version with source4
begin     
    source= {source4}
    result = call_external(!phalib,'phaSrcWFGauss',$
                           source,  $
                           ianzz, zmin, zmax, $
                           ianzy, ymin, ymax, $
                           w0,  deltax, xlam, $
                           ez0, ey0, dphi_zy, $
                           /I_VALUE,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

endif else ;; UF version with source 4c
begin
    source= {source4c}
    result = call_external(!phalib,'phaSrcWFGaussc',$
			 source,  $
                         ianzz, zmin, zmax, $
			 ianzy, ymin, ymax, $
		         w0,  deltax, xlam, $
                         ez0, ey0, dphi_zy, $
                         /I_VALUE,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)
endelse

print, 'phaSrcWFGauss finished...'
print, '***'

return, source
END

function phaSrcWFGauss_zpolar, ianzz, zmin, zmax, $
                               ianzy, ymin, ymax, $
                               w0  , deltax, xlam, verbose=verbose, dataformat=dataformat

return, phaSrcWFGauss(ianzz, zmin, zmax, $
                      ianzy, ymin, ymax, $
                      w0, deltax, xlam, $
                      1, 0, 0, verbose=verbose, dataformat=dataformat)
END




function phaSrcWFGauss_ypolar, ianzz, zmin, zmax, $
			ianzy, ymin, ymax, $
                        w0  , deltax, xlam, verbose=verbose, dataformat=dataformat

return, phaSrcWFGauss(ianzz, zmin, zmax, $
			ianzy, ymin, ymax, $
                        w0, deltax, xlam, $
                        0, 1, 0, verbose=verbose, dataformat=dataformat)
END

