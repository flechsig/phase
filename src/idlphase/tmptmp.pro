;;;
; tmptmp.pro   --- functions zum spaeteren sortieren ...
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSet

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewBeamline, blfname
;input        -none-
;output     bl : struct pha4idlBeamlineType

if n_params() eq 0 then blfname=string('Unnamed')

bl          = { pha4idlBeamlineType }
bl.blfname = blfname
bl.fnamesrc4ezre = blfname+'-ezrec'
bl.fnamesrc4ezim = blfname+'-ezimc'
bl.fnamesrc4eyre = blfname+'-eyrec'
bl.fnamesrc4eyim = blfname+'-eyimc'
bl.fnamesrc6 = blfname+'-so6'

; defaultvalues
bl.raynumber = 225



return,bl
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOptElement, name
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

if n_params() eq 0 then blfname=string('Unnamed')

OptElement = { pha4idlOptElementType }
OptElement.name = name

return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaDefineOpticalElement , OptElement $; returns: OptElement  pha4idlOptElement structure
 ;     ,ElementName $;
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut  $; struct GDat - geometry
      ,r1,r2,alpha,rmi,rho,iflagmi,w1,w2,l1,l2,slopew,slopel,du,dw,dl,dRu,dRw,dRl,Art ; struct MDat - mirror

; total arguments: 31

;OptElement = { pha4idlOptElementType }  ; 'zero' opt.element

np=n_params()

; Set NAME of new element
;OptElement.name = ElementName

; Set struct GDat:{gdatset}
phaOptElementDefGeo, OptELement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; set struct MDat:{mdatset}
phaOptElementDefMDat, OptELement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaOptElementDefGeo, OptELement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut
      ;all contents of struct GDat - geometry

; Set struct GDat:{gdatset}
OptElement.GDat.theta0    = theta0
OptElement.GDat.r         = r
OptElement.GDat.rp        = rp
OptElement.GDat.xdens(0)  = xdens0
OptElement.GDat.xdens(1)  = xdens1
OptElement.GDat.xdens(2)  = xdens2
OptElement.GDat.xdens(3)  = xdens3
OptElement.GDat.xdens(4)  = xdens4
OptElement.GDat.lambda    = lambda
OptElement.GDat.inout     = inout
OptElement.GDat.iflag     = iflag
OptElement.GDat.azimut    = azimut

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaOptElementDefMDat, OptELement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art
      ;all contents of struct MDat - geometry

; set struct MDat:{mdatset}
OptElement.MDat.r1        = r1
OptElement.MDat.r2        = r2
OptElement.MDat.alpha     = alpha
OptElement.MDat.rmi       = rmi
OptElement.MDat.rho       = rho
OptElement.MDat.iflagmi   = iflagmi
OptElement.MDat.w1        = w1
OptElement.MDat.w2        = w2
OptElement.MDat.l1        = l1
OptElement.MDat.l2        = l2
OptElement.MDat.slopew    = slopew
OptElement.MDat.slopel    = slopel
OptElement.MDat.du        = du
OptElement.MDat.dw        = dw
OptElement.MDat.dl        = dl
OptElement.MDat.dRu       = dRu
OptElement.MDat.dRw       = dRw
OptElement.MDat.dRl       = dRl
OptElement.MDat.Art       = Art

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION bltest;,bl_out,mirror_out

; FUNCTION phaDefineOpticalElement ; returns: OptElement  pha4idlOptElement structure
;     ,ElementName $;
;     ,r1,r2,alpha,rmi,rho,iflagmi,w1,w2,l1,l2,slopew,slopel,du,dw,dl,dRu,dRw,dRl,Art $; struct MDat - mirror
;     ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lamdba,inout,iflag,azimut  ; struct GDat - geometry
;;;;;; total arguments: 32

;phaAddOptElement, bl, OptElement 

bl          = { pha4idlBeamlineType }

mirror      = { pha4idlOptElementType }

elname='new spiegel'
;phaDefineOpticalElement , mirror    $; mirror is of pha4idlOptElementType structure
  ;   ,elname $
  ;   ,r1,r2,alpha,rmi,rho,iflagmi,w1,w2,l1,l2,slopew,slopel,du,dw,dl,dRu,dRw,dRl,Art $; struct MDat - mirror
  ;   ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lamdba,inout,iflag,azimut  ; struct GDat - geometry
;;;;;;  total arguments: 32

phaDefineOpticalElement , mirror $

       ,elname ,2,3,4,5,6,7,8 $
       ,9 $
       ,10,11,12,13,14,15,16,17,18,19,20 $
       ,21,22,23,24,25,26,27,28,29,30 ,31 ,32

phaAddOptElement, bl, mirror


mirror.name = 'new spiegel 2'
phaAddOptElement, bl, mirror


;mirror_out =mirror
;return,mirror_out
bl_out = bl
return,bl_out 
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dummy for tests
pro bl,bli

np=n_params()
;if np eq 0 then fname = 'test.bl.in'
fnamein = 'test.bl.in'
fnameout= 'test.bl.out'

if np lt 1 then bli = { pha4idlBeamlineType }


; Read BLfile
 phaReadBLFile, fnamein , bli

; Mod filenames for phase run
bli.fnamesrc4ezre =  string('so4zre.dat')
;bli.fnamesrc4ezim = 'so4zim.dat'
;bli.fnamesrc4eyre = 'so4yre.dat'
;bli.fnamesrc4eyim = 'so4yim.dat'
bli.fnamesrc6     = 'so6.dat'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Add optical element, manually     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set NAME of new element
bli.ElementList(bli.NumElements).elementname=byte('new idl mirror')
; set struct MDat:{mdatset}
bli.ElementList(bli.NumElements).MDat.r1        = 1
bli.ElementList(bli.NumElements).MDat.r2        = 2
bli.ElementList(bli.NumElements).MDat.alpha     = 3
bli.ElementList(bli.NumElements).MDat.rmi       = 4
bli.ElementList(bli.NumElements).MDat.rho       = 5
bli.ElementList(bli.NumElements).MDat.iflagmi   = 6
bli.ElementList(bli.NumElements).MDat.w1        = 7
bli.ElementList(bli.NumElements).MDat.w2        = 8
bli.ElementList(bli.NumElements).MDat.l1        = 9
bli.ElementList(bli.NumElements).MDat.l2        = 10
bli.ElementList(bli.NumElements).MDat.slopew    = 11
bli.ElementList(bli.NumElements).MDat.slopel    = 12
bli.ElementList(bli.NumElements).MDat.du        = 13
bli.ElementList(bli.NumElements).MDat.dw        = 14
bli.ElementList(bli.NumElements).MDat.dl        = 15
bli.ElementList(bli.NumElements).MDat.dRu       = 16
bli.ElementList(bli.NumElements).MDat.dRw       = 17
bli.ElementList(bli.NumElements).MDat.dRl       = 18
bli.ElementList(bli.NumElements).MDat.Art       = 19
; Set struct GDat:{gdatset}
bli.ElementList(bli.NumElements).GDat.theta0    = 1
bli.ElementList(bli.NumElements).GDat.r         = 2
bli.ElementList(bli.NumElements).GDat.rp        = 3
bli.ElementList(bli.NumElements).GDat.xdens(0)  = 110
bli.ElementList(bli.NumElements).GDat.xdens(1)  = 111
bli.ElementList(bli.NumElements).GDat.xdens(2)  = 112
bli.ElementList(bli.NumElements).GDat.xdens(3)  = 113
bli.ElementList(bli.NumElements).GDat.xdens(4)  = 114
bli.ElementList(bli.NumElements).GDat.lambda    = 4
bli.ElementList(bli.NumElements).GDat.inout     = 5
bli.ElementList(bli.NumElements).GDat.iflag     = 6
bli.ElementList(bli.NumElements).GDat.azimut    = 7
; increment num of element
bli.NumElements++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Add optical element finished      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Write 'new' BLfile
phaWriteBLFile, fnameout, bli

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;