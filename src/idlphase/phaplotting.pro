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