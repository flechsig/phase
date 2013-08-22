;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phaplotting.pro
;  Date      : <20 Aug 13 08:40:08 flechsig> 
;  Time-stamp: <22 Aug 13 14:12:42 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

; Phase 3D-Surface-Plot
;
;
function get_pha_src4_axis_z, beam
 
minz=dblarr(beam.iezrex)
minz(*)=beam.xezremin

z = minz + ((beam.xezremax-beam.xezremin)/(beam.iezrex-1)) * dindgen(beam.iezrex)

return,z

END

function get_pha_src4_axis_y, beam
 
miny=dblarr(beam.iezrey)
miny(*)=beam.yezremin

y = miny + ((beam.yezremax-beam.yezremin)/(beam.iezrey-1)) * dindgen(beam.iezrey)

return,y

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaIntensitySurface,beam, name, ax = AX, az = AZ, _EXTRA = SHADE
;+
; NAME:
; phaIntensitySurface
;
; PURPOSE:
;       Draws three-dimensional surface mesh of the intensity.
;
; CATEGORY:
;       pro : pha4idl - extract grid information
;
; CALLING SEQUENCE:
;       phaIntensitySurface, beam, name, [ax = AX], [az = AZ]
;
; INPUTS:
;       beam: pha4idl beam source4 structure (see phainit_structures.pro)
;       name: title of the plot
;       [optional]: ax = AX: rotate about X-axis, relative to 30 degree
;       [optional]: az = AZ: rotate about Z-axis, relative to 30 degree
;	[optional]: /SHADE:  draw shaded surface instead of wireframe mesh
; 
; OUTPUTS:
;       None.
;
; KEYWORDS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS: 
;
; MODIFICATION HISTORY:
;       SG: 09/17/2010 -- HELP added
;
;-

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

if KEYWORD_SET(AX) then ax=30+AX else ax=30;
if KEYWORD_SET(AZ) then az=30+AZ else az=30;

if KEYWORD_SET(SHADE) then $
  shade_surf,((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)(0:beam.iezrex-1,0:beam.iezrey-1)), z, y, title=name, AX=az, AZ=ax $ 
else $
  surface,((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)(0:beam.iezrex-1,0:beam.iezrey-1)), z, y, title=name, AX=az, AZ=ax


;surface,((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name, AX=az, AZ=ax
;shade_surf,((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaIntensityShade_Surf,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

shade_surf,((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro iphaIntensitySurface,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

isurface,((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaRealSurface_Ez,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

surface,(beam.zezre(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro phaRealShade_Surf_Ez,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

shade_surf,(beam.zezre(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Phase 3D-Surface-Plot of Real Part
pro iphaRealSurface_Ez,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

isurface,(beam.zezre(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaImagSurface_Ez,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

surface,(beam.zezim(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaImagShade_Surf_Ez,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

shade_surf,(beam.zezim(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Phase 3D-Surface-Plot of Immaginary Part
pro iphaImagSurface_Ez,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

isurface,(beam.zezim(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaRealSurface_Ey,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

surface,(beam.zeyre(0:beam.ieyrex-1,0:beam.ieyrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro phaRealShade_Surf_Ey,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

shade_surf,(beam.zeyre(0:beam.ieyrex-1,0:beam.ieyrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Phase 3D-Surface-Plot of Real Part
pro iphaRealSurface_Ey,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

isurface,(beam.zeyre(0:beam.ieyrex-1,0:beam.ieyrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaImagSurface_Ey,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

surface,(beam.zeyim(0:beam.ieyrex-1,0:beam.ieyrey-1)),z,y,title=name

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaImagShade_Surf_Ey,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

shade_surf,(beam.zeyim(0:beam.ieyrex-1,0:beam.ieyrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Phase 3D-Surface-Plot of Immaginary Part
pro iphaImagSurface_Ey,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

isurface,(beam.zeyim(0:beam.ieyrex-1,0:beam.ieyrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaDrawVecField, beam, pol
;+
; NAME:
;	phaDrawVecField
;
; PURPOSE:
;       Draws two-dimensional vector array of a complex field.
;       real part       ->  abscissa
;       imaginart part  ->  ordinate
;
; CATEGORY:
;	      pro : pha4idl - 
;
; CALLING SEQUENCE:
;	      phaDrawVecField, beam
;
; INPUTS:
;     	beam:	pha4idl beamline structure (see phainit_structures.pro)
;       pol: choose y-polarized part if (pol EQ 0), z-polarized otherwise
;	
;	
; OUTPUTS:
;     	None.
;
; KEYWORDS:
;	      None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS: 
;       Imaginary and real array is assumed to be of the same size.
; 
; MODIFICATION HISTORY:
;   UF Aug 2013 make polarization optimal  
;
;-


if n_elements(pol) eq 0 then pol = 0

zerofield = 0

; choose y-or z-polarized part
IF (pol EQ 0) THEN BEGIN
  fieldRe = beam.zeyre
  fieldIm = beam.zeyim
  nx = beam.ieyrex
  ny = beam.ieyrey
  title = "Y polarized"
ENDIF ELSE BEGIN
  fieldRe = beam.zezre
  fieldIm = beam.zezim  
  nx = beam.iezrex
  ny = beam.iezrey
  title = "Z polarized"
ENDELSE

; check if there are elements which are non-zero 
IF (min(fieldRe) EQ 0) AND (max(fieldRe) EQ 0) AND (min(fieldIm) EQ 0) AND (max(fieldIm) EQ 0) $
THEN BEGIN
  (fieldRe(0) = 0.01) ;need at least one non-zero entry for normalizing
  zerofield = 1
ENDIF

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)


; TODO: y, z Labeling vertauscht durch mich, sonst Widerspruch zu Fresnel-Kirchhoff-Propagation
; TODO: Size of drawn vectors not always appropriate

step = 5

IF (zerofield EQ 0) $
THEN $; plot vector field
  velovect, ((fieldRe*step)(0:nx-1:step,0:ny-1:step)), ((fieldIm*step)(0:nx-1:step,0:ny-1:step)), y(0:nx-1:step), z(0:ny-1:step), TITLE=title $
ELSE $; plot vector field where all vectors have point-like appearence
  velovect, ((fieldRe*step)(0:nx-1:step,0:ny-1:step)), ((fieldIm*step)(0:nx-1:step,0:ny-1:step)), y(0:nx-1:step), z(0:ny-1:step), LENGTH=0.0001, TITLE=title

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro phaDrawIntensity, beam, name, colors = CO, _EXTRA = LOG
;+
; NAME:
; phaDrawIntensity
;
; PURPOSE:
;       Draws two-dimensional shaded contour plot of the intensity.
;
; CATEGORY:
;       pro : pha4idl - extract grid information
;
; CALLING SEQUENCE:
;       phaGetIntensityArray, beam, array
;
; INPUTS:
;       beam: pha4idl beam source4 structure (see phainit_structures.pro)
;       name: title of the plot
;       [optional]: COLORS=N: use N shades for plot
;       [optional]: /LOG: use logarithmic scale for values
; 
; OUTPUTS:
;       None.
;
; KEYWORDS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS: 
;
; MODIFICATION HISTORY:
;       SG: 08/24/2010 -- add LOG and COLORS option
;
;-

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

if KEYWORD_SET(CO) then colors=CO $
  else colors=10;

; TODO: y, z Labeling vertauscht durch mich, sonst Widerspruch zu Fresnel-Kirchhoff-Propagation
if KEYWORD_SET(LOG) then $
  contour, alog((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)(0:beam.iezrex-1,0:beam.iezrey-1)), z, y, title=name, /FILL $
else $
  contour, ((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)(0:beam.iezrex-1,0:beam.iezrey-1)), z, y, title=name, /FILL, NLEVELS=colors

end


;; UF routine phaDrawPhase is missing
pro phaDrawPhase, beam, name, zpol=zpol
;+
; NAME:
; phaDrawPhase
;
; PURPOSE:
;       Draws two-dimensional shaded contour plot of the field points's phases.
;       The Y-polarized part of the field is drawn by default, unless /ZPOL option is used.
;
; CATEGORY:
;       pro : pha4plotting - field visualization
;
; CALLING SEQUENCE:
;       phaDrawPhase, beam
;
; INPUTS:
;       beam: pha4idl beam source4 structure (see phainit_structures.pro)
;       name: title of the plot
;       [optional]: COLORS=N: use N shades for plot
;       [optional]: /ZPOL: use Z-polarized part instead of the Y-polarized one
;
; OUTPUTS:
;       None.
;
; KEYWORDS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS: 
;
; MODIFICATION HISTORY:
;       SG: 09/24/2012 -- initial version
;
;-

;; UF the SG version is lost?

print, 'phaplotting.pro: phaDrawPhase not yet implemented'

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

if n_elements(zpol) eq 0 then f= complex(beam.zeyre, beam.zeyim) else f= complex(beam.zezre, beam.zezim)

phase= atan(f, /phase)

mycontour, (f(0:beam.iezrex-1,0:beam.iezrey-1)), z, y, title=name

return
end
