
; file: phaSetBeamlineOptions.pro




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSetControlFlags, bl ,$
iord, iordsc, iexpand, iplmode, ibright, ispline,$   ;
inorm, inorm1, inorm2, matrel, igrating, ipinarr;,$   ;
;+
; NAME:
;	phaSetControlFlags
;
; PURPOSE:
;       Set various control flags for the phase run
;
; CATEGORY:
;	pro : pha4idl - beamline options
;
; CALLING SEQUENCE:
;	phaSetControlFlags, bl ,$
;	iord, iordsc, iexpand, iplmode, ibright, ispline,$
;	inorm, inorm1, inorm2, matrel, igrating, ipinarr
;
; INPUTS:
;	bl		pha4idl beamline structure
;	iord
;	iordsc
;	iexpand
;	iplmode
;	ibright
;	ispline
;	inorm
;	inorm1
;	inorm2
;	matrel
;	igrating
;	ipinarr
;
; OUTPUTS:
;     	bl:		pha4idl beamline structure with new options
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
;+
; NAME:
;	phaSetApertures
;
; PURPOSE:
;       Set apertures for the phase run
;
; CATEGORY:
;	pro : pha4idl - beamline options
;
; CALLING SEQUENCE:
;	phaSetApertures, bl , $
;	srcymin, srcymax, srczmin, srczmax, rpin, $
;	ymin_ap, ymax_ap, zmin_ap, zmax_ap, rpin_ap
;
; INPUTS:
;	bl		pha4idl beamline structure
;	srcymin
;	srcymax
;	srczmin
;	srczmax
;	rpin
;	ymin_ap
;	ymax_ap
;	zmin_ap
;	zmax_ap
;	rpin_ap
;
; OUTPUTS:
;     	bl:		pha4idl beamline structure with new options
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
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSetIntegrationParameter, bl , $
	distfocy, distfocz, ianzy0, ymin, ymax , ianzz0  , zmin ,zmax ,$
	d12_max , iamp_smooth  ,iord_amp  ,iord_pha ,$
	ifm_amp  ,ifm_pha  ,id12  ,ianz0_cal  ,ianz0_fixed
;+
; NAME:
;	phaSetIntegrationParameter
;
; PURPOSE:
;       Set integration parameter for the phase run
;
; CATEGORY:
;	pro : pha4idl - beamline options
;
; CALLING SEQUENCE:
;	phaSetIntegrationParameter, bl , $
;	distfocy, distfocz, ianzy0, ymin, ymax , ianzz0  , zmin ,zmax ,$
;	d12_max , iamp_smooth  ,iord_amp  ,iord_pha ,$
;	ifm_amp  ,ifm_pha  ,id12  ,ianz0_cal  ,ianz0_fixed
;
; INPUTS:
;	bl		pha4idl beamline structure
;	distfocy
;	distfocz
;	ianzy0
;	ymin
;	ymax
;	ianzz0
;	zmin
;	zmax
;	d12_max
;	iamp_smooth
;	iord_amp
;	iord_pha
;	ifm_amp
;	ifm_pha
;	id12
;	ianz0_cal
;	ianz0_fixed
;
; OUTPUTS:
;     	bl:		pha4idl beamline structure with new options
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

