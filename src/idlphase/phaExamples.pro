;
;
; FILE: phaExamples.pro
;
;           Various examples to phase4idl
;
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION AnEasyPhaseRun01

!P.Multi=[0,1,2]

;;;;; all mirror data:


beamlinefilename = 'AnEasyPhaseRun01'
resultname       = 'AnEasyPhaseRun01_Result'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describe beamline : general parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; struct bl = phaNewBeamline(BLFileName)
bl = phaNewBeamline(beamlinefilename)


;phaSetSrcPhaseSpace, bl, ny, ymin, ymax, nz, zmin, zmax
phaSetSrcPhaseSpace, bl, 15, -0.1 , 0.1 , 15, -0.1, 0.1


;phaSetControlFlags, bl , iord, iordsc, iexpand, iplmode, ibright, ispline, inorm, inorm1, inorm2, matrel, igrating, ipinarr
phaSetControlFlags, bl  , 4   , 4     ,  1     ,   0    ,  0     , 0      ,  0   ,  0    ,  0    ,  0    ,  0      ,  0


;phaSetApertures, bl, srcymin,srcymax,srczmin,srczmax,rpin,ymin_ap,ymax_ap,zmin_ap,zmax_ap,rpin_ap
phaSetApertures,  bl, -100   , 100   , -100  , 100   , 300, -100  , 100   , -100  , 100   , 200 


;phaSetIntegrationParameter, bl , distfocy, distfocz, ianzy0, ymin, ymax , ianzz0  , zmin ,zmax , $
phaSetIntegrationParameter, bl  ,   0     ,   0     ,  451  ,-0.015, 0.015, 451    ,-0.015,0.015, $
;	d12_max , iamp_smooth ,iord_amp ,iord_pha, ifm_amp, ifm_pha ,id12 ,ianz0_cal,ianz0_fixed
	  1     ,     0       ,  -2     ,  -1    ,  3     ,  5      , 1   ,  0      ,   301


;phaSetPSsource,bl ,isrctype,isrcy,isrcdy,sigmay,sigmayp,isrcz,isrcdz,sigmaz,sigmazp
phaSetPSsource, bl ,   4    , 4   ,  0   , 0.1  ,0.00025, 4   ,  4   ,  0.1 ,0.0025 $
;                   ,dipcy,dipcz,dipdisy,dipdisz,pin_yl0,pin_yl,pin_zl0,pin_zl
                    , 0   ,  1  ,  800  ,  800  , 0.01  , 0.005, 0.01  , 0.005







; struct OptEl = phaNewMirror(ElementName)
 mirror01  = phaNewOptElement('Spieglein an der Wand')

;grating01  = phaNewOptElement('ein kleines gitter')


; define mirror    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
phaDefineOpticalElement , mirror01 $
; struct GDat - geometry
;      ,theta0,r src    ,rp image
       , 80    , 2000    , 300 $
; ,xdens0,xdens1,xdens2,xdens3,xdens4
      ,0 , 0    , 0    , 0    , 0 $
;,lambda[mue]   ,inout(diff.order),iflag,azimut
   ,60e-6       , 0               , 0   , 0 $
; struct MDat - mirror
;   ,r1 src,r2 image,alpha,rmi (rw) , rho (rl),iflagmi
    ,10000 , 200    ,80   , 2258.34 , 68.097  , 0     $
; ,w1   ,w2 , l1    ,l2   ,slopew , slopel
,-100 , 100 , -10  ,  10  , 0.1   , 1     $
;du,dw,dl,dRu,dRw,dRl    --- misalignment
 ,0 , 0, 0,0  ,0  ,0   $
;,ElementArt $
 , 66
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; add mirror to beamline
phaAddOptElement, bl, mirror01




; create src field
bl.src.so4 = phaSrcWFGauss(128, -1, 1,128, -1, 1, 0.2 , 0 , 20)


;;; DO THE PHASE RUN --> spaeter zu einer funktion zusammenfassen !!!
PHAINTENSITYSURFACE,bl.src.so4,'pre  phase run'
phaSaveEMField, bl.src.so4, bl.blfname
phaWriteBLFile, bl

cmode = 3 ; phasespace
phaBatchMode_nosubprocess, bl.blfname, bl.blfname, cmode

phaLoadEMField, bl.src.so4, bl.blfname
PHAINTENSITYSURFACE,bl.src.so4,'post phase run'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;bl2 = phaReadBLFile(beamlinefilename)



return, bl

END ; AnEasyPhaseRun01
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO dummypro


END ; dummypro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



