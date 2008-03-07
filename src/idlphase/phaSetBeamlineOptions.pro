
; file: phaSetBeamlineOptions.pro




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSetControlFlags, bl ,$
iord, iordsc, iexpand, iplmode, ibright, ispline,$   ;
inorm, inorm1, inorm2, matrel, igrating, ipinarr;,$   ;
;ilimits, ipath       ; not in blf
; END of inparams
bl.BLOptions.ifl.iord   =iord
bl.BLOptions.ifl.iordsc =iordsc
bl.BLOptions.ifl.iexpand=iexpand
bl.BLOptions.ifl.iplmode=iplmode
bl.BLOptions.ifl.ibright=ibright
bl.BLOptions.ifl.ispline=ispline
bl.BLOptions.ifl.inorm  =inorm
bl.BLOptions.ifl.inorm1 =inorm1
bl.BLOptions.ifl.inorm2 =inorm2
bl.BLOptions.ifl.matrel =matrel
bl.BLOptions.ifl.igrating=igrating
bl.BLOptions.ifl.ipinarr=ipinarr
;bl.BLOptions.ifl.ilimits=ilimits  ; not in blf
;bl.BLOptions.ifl.ipath  =ipath    ; not in blf
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSetApertures, bl , $
	srcymin        ,srcymax        ,srczmin        ,srczmax        ,rpin        , $
	ymin_ap        ,ymax_ap        ,zmin_ap        ,zmax_ap        ,rpin_ap
;	w_min        ,w_max        ,xl_min        ,xl_max
; END inparams
bl.BLOptions.apr.srcymin=srcymin
bl.BLOptions.apr.srcymax=srcymax
bl.BLOptions.apr.srczmin=srczmin
bl.BLOptions.apr.srczmax=srczmax
bl.BLOptions.apr.rpin=rpin
bl.BLOptions.apr.ymin_ap=ymin_ap
bl.BLOptions.apr.ymax_ap=ymax_ap
bl.BLOptions.apr.zmin_ap=zmin_ap
bl.BLOptions.apr.zmax_ap=zmax_ap
bl.BLOptions.apr.rpin_ap=rpin_ap
; not in blfile
;bl.BLOptions.apr.w_min=w_min
;bl.BLOptions.apr.w_max=w_max
;bl.BLOptions.apr.xl_min=xl_min
;bl.BLOptions.apr.xl_max=xl_max
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSetIntegrationParameter, bl , $
	distfocy, distfocz, ianzy0, ymin, ymax , ianzz0  , zmin ,zmax ,$
	d12_max , iamp_smooth  ,iord_amp  ,iord_pha ,$
	ifm_amp  ,ifm_pha  ,id12  ,ianz0_cal  ,ianz0_fixed
;	phase_change_1 , phase_change_2 ,  amp_change , iphase_curv  , iphase_pi2
;END of inparams
bl.BLOptions.xi.distfocy=distfocy
bl.BLOptions.xi.distfocz=distfocz
bl.BLOptions.xi.ianzy0=ianzy0
bl.BLOptions.xi.ymin=ymin
bl.BLOptions.xi.ymax=ymax
bl.BLOptions.xi.ianzz0=ianzz0
bl.BLOptions.xi.zmin=zmin
bl.BLOptions.xi.zmax=zmax
;bl.BLOptions.xi.phase_change_1=10  ; not in BLF
;bl.BLOptions.xi.phase_change_2=11  ; not in BLF
bl.BLOptions.xi.d12_max=d12_max
;bl.BLOptions.xi.amp_change=13  ; not in BLF
bl.BLOptions.xi.iamp_smooth=iamp_smooth
bl.BLOptions.xi.iord_amp=iord_amp
bl.BLOptions.xi.iord_pha=iord_pha
;bl.BLOptions.xi.iphase_curv=17  ; not in BLF
;bl.BLOptions.xi.iphase_pi2=18  ; not in BLF
bl.BLOptions.xi.ifm_amp=ifm_amp
bl.BLOptions.xi.ifm_pha=ifm_pha
bl.BLOptions.xi.id12=id12
bl.BLOptions.xi.ianz0_cal=ianz0_cal
bl.BLOptions.xi.ianz0_fixed=ianz0_fixed
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSetPSsource, bl  ,isrctype $
,isrcy,isrcdy,sigmay,sigmayp,isrcz,isrcdz,sigmaz,sigmazp $ ; so1 stuff
,dipcy,dipcz,dipdisy,dipdisz $ ; so5 stuff
,pin_yl0,pin_yl,pin_zl0,pin_zl ; other
; src names vorerst wie beamlinename+passende endung !!!
;so4.fsource4a
;so4.fsource4b
;so4.fsource4c
;so4.fsource4d
;so6.fsource6
bl.src.isrctype=isrctype
bl.src.so1.isrcy=isrcy
bl.src.so1.isrcdy=isrcdy

bl.src.so1.sigmay=sigmay
bl.src.so1.sigmayp=sigmayp

print,bl.src.so1.sigmay
print,bl.src.so1.sigmayp

bl.src.so1.isrcz=isrcz
bl.src.so1.isrcdz=isrcdz
bl.src.so1.sigmaz=sigmaz
bl.src.so1.sigmazp=sigmazp
;bl.src.so4.fsource4a
;bl.src.so4.fsource4b
;bl.src.so4.fsource4c
;bl.src.so4.fsource4d
bl.src.so5.dipcy=dipcy
bl.src.so5.dipcz=dipcz
bl.src.so5.dipdisy=dipdisy
bl.src.so5.dipdisz=dipdisz
;bl.src.so6.fsource6
bl.src.pin_yl0=pin_yl0
bl.src.pin_yl=pin_yl
bl.src.pin_zl0=pin_zl0
bl.src.pin_zl=pin_zl
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



