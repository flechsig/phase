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

;Phase 3D-Surface-Plot of Intensity

pro phaIntensitySurface,beam,name

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

surface,((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)(0:beam.iezrex-1,0:beam.iezrey-1)),z,y,title=name
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

pro phaDrawVecField, bl, pol
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
;	      phaDrawVecField, bl
;
; INPUTS:
;     	bl:	pha4idl beamline structure (see phainit_structures.pro)
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
;     
;
;-

zerofield = 0

; choose y-or z-polarized part
IF (pol EQ 0) THEN BEGIN
  fieldRe = bl.zeyre
  fieldIm = bl.zeyim
  nx = bl.ieyrex
  ny = bl.ieyrey
  title = "Y polarized"
ENDIF ELSE BEGIN
  fieldRe = bl.zezre
  fieldIm = bl.zezim  
  nx = bl.iezrex
  ny = bl.iezrey
  title = "Z polarized"
ENDELSE

; check if there are elements which are non-zero 
IF (min(fieldRe) EQ 0) AND (max(fieldRe) EQ 0) AND (min(fieldIm) EQ 0) AND (max(fieldIm) EQ 0) $
THEN BEGIN
  (fieldRe(0) = 0.01) ;need at least one non-zero entry for normalizing
  zerofield = 1
ENDIF

y=get_pha_src4_axis_y(bl)
z=get_pha_src4_axis_z(bl)


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

pro phaDrawIntensity, beam, name
;+
; NAME:
;	phaDrawIntensity
;
; PURPOSE:
;       Draws two-dimensional shaded contour plot of the intensity.
;
; CATEGORY:
;	      pro : pha4idl - extract grid information
;
; CALLING SEQUENCE:
;	      phaGetIntensityArray, beam, array
;
; INPUTS:
;     	beam:	pha4idl beam source4 structure (see phainit_structures.pro)
;	      name: title of the plot
;	
; OUTPUTS:
;     	None.
;
; KEYWORDS:
;	      None.
;
; SIDE EFFECTS:
;       
;
; RESTRICTIONS: 
;
; MODIFICATION HISTORY:
;     
;
;-

y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

contour, ((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)(0:beam.iezrex-1,0:beam.iezrey-1)), z, y, title=name, /FILL

end