;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phainit_structures.pro
;  Date      : <14 Aug 13 11:24:26 flechsig> 
;  Time-stamp: <14 Aug 13 12:27:40 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

; phase structures...

PRO phainit_structures, verbose=verbose

if n_elements(verbose) ne 0 then print, 'phainit_defines called'

MaxPathLength=long(255)
MaximumOptElements=long(64)

defsysv, '!phaMaxArraySize', 2048 
 
intzero=long(0)
dblzero=double(0)

; wie funzts mit Pointern als Unterelement ???
; nehme Pointersize 4Byte, also uint(0) an, und uebergebe nur Adresse als uint-Wert
cptr=uint(0) 
; Beachte: idlarr(x,y) = c-arr[y][x]
; in c: typedef double MAP7TYPE [5][5][5][5];   
MAP7TYPE  = dblarr(5,5,5,5)
; in c: typedef double MAP70TYPE [70][70];     
MAP70TYPE = dblarr(70,70) 

;  the structures xxx will now be initialized ...
;  " IDL> s = { xxx } " will create a zeroed structure of type xxx.


;/* --------------- sources -----------------------------------*/
dummy_source1 = { source1	,$  ;             /* Gauss + Hard edge */ 
         sigmay:dblzero,sigmayp:dblzero,sigmaz:dblzero,sigmazp:dblzero, $
	   isrcy:intzero,isrcdy:intzero,isrcz:intzero,isrcdz:intzero $
};source1  
 
dummy_source2 = { source2 , $
	  fsource2a:bytarr(80), fsource2b:bytarr(80), $
     	  ceyre:dblarr(61,121), ceyim:dblarr(61,121), $
        fscale:dblzero,small:dblzero, $
	  iordzern:intzero,iflagl:intzero $
};source2  

dummy_source3 = { source3 , $
	  fsource3a:bytarr(80),fsource3b:bytarr(80), $
	  xsrcreal:dblarr(1024),ysrcreal:dblarr(1024), $
   	  xsrcimag:dblarr(1024),ysrcimag:dblarr(1024), $
	  isrcreal:intzero,isrcimag:intzero,iactreal:intzero,iactimag:intzero $
};source3  

dummy_source4 =  { source4 ,                                                $
        fsource4a:bytarr(80),fsource4b:bytarr(80), $
        fsource4c:bytarr(80),fsource4d:bytarr(80), $
	xeyremin:dblzero , xeyremax:dblzero , dxeyre:dblzero ,$
	xeyimmin:dblzero , xeyimmax:dblzero , dxeyim:dblzero ,$
	yeyremin:dblzero , yeyremax:dblzero , dyeyre:dblzero ,$
	yeyimmin:dblzero , yeyimmax:dblzero , dyeyim:dblzero ,$
	zeyre:dblarr(!phaMaxArraySize,!phaMaxArraySize) , zeyim:dblarr(!phaMaxArraySize,!phaMaxArraySize) ,$
	xezremin:dblzero , xezremax:dblzero , dxezre:dblzero , $
	xezimmin:dblzero , xezimmax:dblzero , dxezim:dblzero , $
	yezremin:dblzero , yezremax:dblzero , dyezre:dblzero , $
	yezimmin:dblzero , yezimmax:dblzero , dyezim:dblzero , $
	zezre:dblarr(!phaMaxArraySize,!phaMaxArraySize) , zezim:dblarr(!phaMaxArraySize,!phaMaxArraySize) ,$
	gridx:dblarr(!phaMaxArraySize)     , gridy:dblarr(!phaMaxArraySize), deltatime:dblzero, $
      ampeyre:dblzero , ampeyim:dblzero , $
      ampezre:dblzero , ampezim:dblzero , $
      xlam:dblzero , $
      ieyrex:intzero , ieyimx:intzero , ieyrey:intzero , ieyimy:intzero , $
      iezrex:intzero , iezimx:intzero , iezrey:intzero , iezimy:intzero , $
      nsource:intzero, nimage:intzero, nfreqtot:intzero, $
	nfreqpos:intzero ,nfreqneg:intzero , iconj:intzero $
};source4

dummy_source4c = { source4c , $
                   xemin:dblzero, xemax:dblzero, dx:dblzero, yemin:dblzero, yemax:dblzero, dy:dblzero, $
                   zeyre:ptr_new(), zeyim:ptr_new(), $
                   zezre:ptr_new(), zezim:ptr_new(), $
                   gridx:ptr_new(), gridy:ptr_new(), deltatime:dblzero, $
                   ampeyre:dblzero, ampeyim:dblzero, ampezre:dblzero, ampezim:dblzero, $
                   xlam:dblzero, $
                   iex:intzero, iey:intzero, $
                   nsource:intzero, nimage:intzero, nfreqtot:intzero, nfreqpos:intzero, nfreqneg:intzero, iconj:intzero $
                 } ;; source4c

dummy_source5 = {  source5 ,  $;                 /* Dipol Quelle  */
           dipcy:dblzero ,   dipcz:dblzero ,   dipdisy:dblzero , dipdisz:dblzero ,$
           dipymin:dblzero , dipymax:dblzero , dipzmin:dblzero , dipzmax:dblzero  $
};source5  

dummy_source6 = { source6 , $
	  fsource6:bytarr(80), $
	  br:dblarr(16,16,16,16), $
	  brxmin:dblzero ,brxmax:dblzero ,brdx:dblzero ,$
	  brymin:dblzero ,brymax:dblzero ,brdy:dblzero ,$
	  brpxmin:dblzero ,brpxmax:dblzero ,brdpx:dblzero ,$
	  brpymin:dblzero ,brpymax:dblzero ,brdpy:dblzero ,$
	  abr:dblarr(16,16,16,16),$
	  abrxmin:dblzero ,abrxmax:dblzero ,abrdx:dblzero ,$
	  abrymin:dblzero ,abrymax:dblzero ,abrdy:dblzero ,$
	  abrpxmin:dblzero ,abrpxmax:dblzero ,abrdpx:dblzero ,$
	  abrpymin:dblzero ,abrpymax:dblzero ,abrdpy:dblzero ,$
  	  ibrpy:intzero,ibrpx:intzero,ibry:intzero,ibrx:intzero,$
        iabrpy:intzero,iabrpx:intzero,iabry:intzero,iabrx:intzero $
};source6    


dummy_sources = { sources, $ ;              /* Sammelstruktur         */
           so1:{source1},  $ ;              /* Gauss + Hard edge      */
           so2:{source2},  $ ;              /* Zernike                */
           so3:{source3},  $ ;              /* radiale Vert. vom File */
           so4:{source4},  $ ;              /* Quelle von File        */
           so5:{source5},  $ ;              /* Dipol Quelle           */
           so6:{source6},  $ ;              /* Brightness             */
	  pin_yl0:dblzero,pin_yl:dblzero,pin_zl0:dblzero,pin_zl:dblzero , $
	  isrctype:intzero   $
};sources     
  

dummy_PHASEset = { PHASEset, $   ;        /* Datensatz in MainPickName 	*/
	matrixname:bytarr(MaxPathLength), $
	mapname:bytarr(MaxPathLength), $    
	sourceraysname:bytarr(MaxPathLength), $    
	imageraysname:bytarr(MaxPathLength), $
	intersecname:bytarr(MaxPathLength), $
	geometryname:bytarr(MaxPathLength), $
	elementname:bytarr(MaxPathLength), $
	sourcepckname:bytarr(MaxPathLength), $
	geometrypckname:bytarr(MaxPathLength), $
	elementpckname:bytarr(MaxPathLength), $
	pssourcename:bytarr(MaxPathLength), $
	plotpsname:bytarr(MaxPathLength), $
	printpclname:bytarr(MaxPathLength), $
	optipckname:bytarr(MaxPathLength), $
	beamlinename:bytarr(MaxPathLength) $
};PHASEset              

dummy_RTSourceType = { RTSourceType ,$
  Quellep:ptr_new() , $
  SourceRays:ptr_new() , $ ;struct RayType *SourceRays
  QuellTyp:intzero, QuellTyp_old:intzero, modified:intzero, raynumber:intzero $
};       

;struct PSDType  {
;  double *y, *z, *psd, *stfd1phmaxc, *stinumbc, *s1c, *s2c, *s3c, 
;    *eyrec, *ezrec, *eyimc, *ezimc;
; double simpre[0x8000], simpim[0x8000], sintre[0x8000], sintim[0x8000], 
;    simpa[0x8000], simpp[0x8000], d12[24576]; 
;  int iy, iz;
;};

dummy_RESULTType = { RESULTType , $
  RESp:ptr_new() , $
  points:intzero, typ:intzero $
};       


dummy_mirrortype = { mirrortype, a:dblarr(6,6) }
;mirrortype


dummy_geometrytype = { geometrytype,$  ;/* in phase_struct.h ist die analoge structur geometryst */
	sina:dblzero, cosa:dblzero, sinb:dblzero, cosb:dblzero, $
	r:dblzero, rp:dblzero, x:dblarr(5), xlam:dblzero, $
  	idefl:intzero $
};geometrytype 

dummy_xlenmaptype = { xlenmaptype, $
   xlen1c:MAP7TYPE, xlen2c:MAP7TYPE $
};xlenmaptype

dummy_mdatset = { mdatset , $ ; aus:  mirrorpck.h
  r1:dblzero,$
  r2:dblzero,$
  alpha:dblzero,$        
  rmi:dblzero,$        
  rho:dblzero,$  
  iflagmi:intzero,$
  w1:dblzero, w2:dblzero, l1:dblzero, l2:dblzero,$
  slopew:dblzero, slopel:dblzero,$
  du:dblzero, dw:dblzero, dl:dblzero, dRu:dblzero, dRw:dblzero, dRl:dblzero,$
  Art:intzero  $   ;   /* UF 9.7.04 */
};mdatset   

dummy_gdatset = { gdatset , $   ; aus: geometrypck.h
      theta0:dblzero,$
	r:dblzero,$
	rp:dblzero,$ 
      xdens:dblarr(5),$  
      lambda:dblzero,$; 
      inout:intzero,$                                 
	iflag:intzero,$ 
      azimut:intzero $;     /* vertikal 0; nach links 1; nach unten 2 */
};gdatset   

dummy_ElementType = { ElementType , $
  ElementOK:intzero , $
  matrix:MAP70TYPE, MtoSource:MAP70TYPE, $
  ypc1:MAP7TYPE, zpc1:MAP7TYPE, dypc:MAP7TYPE, dzpc:MAP7TYPE, wc:MAP7TYPE, xlc:MAP7TYPE, $ 
  xlm:{xlenmaptype}, $ 
  mir:{mirrortype}, $ 
  MDat:{mdatset},  $                          
  geo:{geometrytype},  $ 
  GDat:{gdatset}, $ 
  elementname:bytarr(MaxPathLength) $
};ElementType


dummy_integration = { integration , $
	distfocy:dblzero,distfocz:dblzero,$
	ianzy0:intzero,$
	ianzz0:intzero,$
	ymin:dblzero,ymax:dblzero,$
	zmin:dblzero,zmax:dblzero,$
	phase_change_1:dblzero,phase_change_2:dblzero,d12_max:dblzero,$
	amp_change:dblzero,$
	iamp_smooth:intzero,iord_amp:intzero,iord_pha:intzero,iphase_curv:intzero,$
	iphase_pi2:intzero,ifm_amp:intzero,ifm_pha:intzero,id12:intzero,ianz0_cal:intzero,ianz0_fixed:intzero $
};integration  

;/* --------------- apertures ----------------------------------*/
dummy_apertures = { apertures , $
	srcymin:dblzero,srcymax:dblzero,srczmin:dblzero,srczmax:dblzero,rpin:dblzero, $
	ymin_ap:dblzero,ymax_ap:dblzero,zmin_ap:dblzero,zmax_ap:dblzero,rpin_ap:dblzero,$
	w_min:dblzero,w_max:dblzero,xl_min:dblzero,xl_max:dblzero $
};apertures  

;/* ----------------- flags ---------------------------------------*/
dummy_control_flags = { control_flags , $
   iord:intzero,iordsc:intzero,iexpand:intzero,iplmode:intzero,ibright:intzero,ispline:intzero, $
   inorm:intzero,inorm1:intzero,inorm2:intzero, $
   matrel:intzero, $
   igrating:intzero,ipinarr:intzero,ilimits:intzero, $
   ipath:intzero $ 
};control_flags



dummy_PSSourceType = { PSSourceType, $
    sigy:dblzero, sigdy:dblzero, sigz:dblzero, sigdz:dblzero,$
    yhard:intzero, dyhard:intzero, zhard:intzero, dzhard:intzero  $
};PSSourceType 

;struct RayType { double y, z, dy, dz, phi; };  


dummy_UndulatorSource0Type = { UndulatorSource0Type $
      ,length:dblzero, lambda:dblzero, sigvert:dblzero, sighor:dblzero, deltaz:dblzero $
      ,sigmaez:dblzero, sigmaey:dblzero, sigmaedz:dblzero, sigmaedy:dblzero $
};

dummy_DipolSourceType   =  { DipolSourceType $
            , sigy:dblzero, sigdy:dblzero, sigz:dblzero, dz:dblzero $
};
dummy_PointSourceType   =  { PointSourceType $
            , sigy:dblzero, sigdy:dblzero, sigz:dblzero, sigdz:dblzero $
};
dummy_RingSourceType    =  { RingSourceType  $
            , dy:dblzero, dz:dblzero $
};
dummy_SRSourceType      =  { SRSourceType    $
            , y:dblzero, z:dblzero, dy:dblzero, dz:dblzero $
};
dummy_HardEdgeSourceType=  { HardEdgeSourceType $
            , disty:dblzero, distz:dblzero, divy:dblzero, divz:dblzero $
            ,iy:intzero, iz:intzero, idy:intzero, idz:intzero $
};

dummy_PSImageType = { PSImageType, $
    ymin:dblzero, ymax:dblzero, zmin:dblzero, zmax:dblzero, $
    iy:intzero, iz:intzero  $
};PSSourceType 




dummy_PSOptionsType = { PSOptionsType , $
  intmod:intzero, ndyfix:intzero, ndzfix:intzero,$
  PSSource:{PSSourceType}, $
  dyminfix:dblzero, dymaxfix:dblzero, dzminfix:dblzero, dzmaxfix:dblzero $
};PSOptionsType     




dummy_OptionsType = { OptionsType, $
  SourcetoImage:intzero, wrMatrix:intzero, CalcMod:intzero, wrSource:intzero, WithAlign:intzero,$
  epsilon:dblzero, lambda:dblzero, xlam_save:dblzero, displength:dblzero,$
  PSO:{PSOptionsType} ,$
  ifl:{control_flags} ,$
  apr:{apertures} ,$
  xi:{integration} $
};OptionsType    

dummy_BeamlineType = { BeamlineType, $
  	ElementList:cptr ,$;Hier wird auf Struktur gezeigt...  *ElementList:{ElementType}   
;  	RTSource:{RTSourceType} ,  $ 
	map70:MAP70TYPE, lmap:MAP70TYPE, rmap:MAP70TYPE, MtoSource:MAP70TYPE, $  
	ypc1:MAP7TYPE, zpc1:MAP7TYPE, dypc:MAP7TYPE, $
	dzpc:MAP7TYPE, wc:MAP7TYPE, xlc:MAP7TYPE, $
	fdetc:MAP7TYPE, fdetphc:MAP7TYPE, fdet1phc:MAP7TYPE, $;
  	xlm:{xlenmaptype}, $ 
	raysout:cptr ,$ ;Hier wird auf Struktur gezeigt...  *raysout:{RayType}  
;	RESULT:{RESULTType} , $
  	beamlineOK:uint(0), elementzahl:uint(0), position:uint(0), hormapsloaded:uint(0), $ 
	BLOptions:{OptionsType} ,$
  	deltalambdafactor:dblzero, xlen0:dblzero, $
      src:{sources}  $
}; 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;// *************************************************************************/
dummy_pha4idlBeamline = { pha4idlBeamlineType $
;//  unsigned int beamlineOK, position, hormapsloaded :intzero $;
    ,NumElements:intzero $;
    ,raynumber:intzero $;
    ,SourceType:byte(' ') $;
    ,deltalambdafactor:dblzero $; 
;//  double xlen0 :dblzero $;
    ,ElementList:replicate({ElementType},MaximumOptElements)  $
    ,BLOptions:{OptionsType} $
    ,src:{sources}  $
    ,PSImage:{ PSImageType } $
    ,UndulatorSrc:{ UndulatorSource0Type } $; // Undu0Src enthaelt alle elemente von norm undu
    ,DipolSrc:{ DipolSourceType } $;
    ,PointSrc:{ PointSourceType } $;
    ,SRSrc:{ SRSourceType } $;
    ,HardEdgeSrc:{ HardEdgeSourceType }  $;
    ,RingSrc:{ RingSourceType } $;
    ,fnamesrc4ezre:string(''), fnamesrc4ezim:string('') $
    ,fnamesrc4eyre:string(''), fnamesrc4eyim:string('') $ ;
    ,fnamesrc6:string('') $
    ,blfname:string('') $
}; 
;// *************************************************************************/


dummy_pha4idlOptElement = { pha4idlOptElementType, $
   name:string('') $
  ,MDat:{mdatset}  $
  ,GDat:{gdatset}  $
};OptElementType    









END



