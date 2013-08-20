;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phabeamload.pro
;  Date      : <20 Aug 13 08:56:02 flechsig> 
;  Time-stamp: <20 Aug 13 08:56:06 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

; Loads an Array into mysrc4-Beam-Structure

function phasrc4load,ianzz,zmin,zmax,ianzy,ymin,ymax,xlam,ezre,ezim,eyre,eyim

np=n_params()

if np lt 7 then begin
  print,'Error in "phabeamload" --- ArgCount=',np
  print,'ArgCount should be at least 7 (all fields zero)'
  print,'Check Arguments: nz,zmin,zmax,ny,ymin,ymax,xlam,ezre,ezim,eyre,eyim'
  print,''
  retall
endif

;print,size(ezre)
;SIZE(array) returns the following Vector
;		[No.Dims , No.1.Dim , No.2.Dim ... No.n.Dim , TypeCode , Total No.of Elements]
;
; TypeCode: 0-undef., 1-byte,2-int,3-long,4-float,5-double,6-cmplxflt,7-string,8-struct,9-dblcmplx
;

eyim=dblarr(256,256)
eyre=dblarr(256,256)
ezim=dblarr(256,256)
ezre=dblarr(256,256)

ymin= double(ymin)
ymax= double(ymax)
dy=(ymax-ymin)/double(ianzy)
zmin= double(zmin)
zmax= double(zmax)
dz=(zmax-zmin)/double(ianzz)
xlam= double(xlam)

IF np le 9 THEN  begin 
   eyim=dblarr(256,256) 
   if np le 8 then  begin 
      eyre=dblarr(256,256)
      if np le 7 then  begin 
         ezim=dblarr(256,256) 
         if np le 6 then  begin 
            ezre=dblarr(256,256)
	 endif 
      endif
   endif
endif
 
;  Wegen versch.Programmierern wird hier z zu x ...
;  phase intern heisst's x, in der phase-GUI und nun auch in IDL heisst's z
newbeam={source4,$
	zeyre:eyre,zeyim:eyim,$
	zezre:ezre,zezim:ezim,$
	xeyremin:zmin,xeyremax:zmax,dxeyre:dz,$
	xeyimmin:zmin,xeyimmax:zmax,dxeyim:dz,$
	yeyremin:ymin,yeyremax:ymax,dyeyre:dy,$
	yeyimmin:ymin,yeyimmax:ymax,dyeyim:dy,$
	xezremin:zmin,xezremax:zmax,dxezre:dz,$
	xezimmin:zmin,xezimmax:zmax,dxezim:dz,$
	yezremin:ymin,yezremax:ymax,dyezre:dy,$
	yezimmin:ymin,yezimmax:ymax,dyezim:dy,$
	xlam:xlam,$
	ieyrex:ianzz,ieyimx:ianzz,ieyrey:ianzy,ieyimy:ianzy,$
        iezrex:ianzz,iezimx:ianzz,iezrey:ianzy,iezimy:ianzy}  

return, beam4

END
