
; file: phaSetSources.pro



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSetSrcPhaseSpace, bl, iy, ymin, ymax $
                           , iz, zmin, zmax
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

