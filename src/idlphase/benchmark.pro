;+
; NAME: benchmark.pro
;	
;   Benchmark for the Phase Prop Routines

pro benchmarkfk,n,jjmax


phainit

tfk=dblarr(jjmax)
tnear=dblarr(jjmax)
tfar=dblarr(jjmax)

waist=0.2
d0=0
zmin=-1
ymin=zmin
zmax=-zmin
ymax=-ymin
lambda=20
distance=3000

print,'---------------------------------------------------------------------------------------'
print,' Benchmark mit Gaussquelle '
print,'---------------------------------------------------------------------------------------'
print,'Quelle: w0 [mm] =',double(waist),' ::::: Wellenlaenge l [nm] =',double(lambda),'' 
print,'        nx = ny =',double(n)    ,' ::::: Kantenlaenge s [mm] =',double(2*zmax),''
print,'         d [mm] =',double(distance) 
print,'---------------------------------------------------------------------------------------'



;---------------------------------------------------------------------------------------------


FOR jj=0,(jjmax-1) DO BEGIN


beam=phaSrcWFGauss( n , zmin , zmax , n, ymin , ymax , waist, d0 , lambda,1,1,0)

zeit=systime(1) ; "1" entspricht "/seconds"  
phaPropWFFresnelKirchhoff,beam, distance,n, zmin, zmax, n, ymin, ymax
tfk(jj)=systime(1)-zeit


;zeit=systime(1) ; "1" entspricht "/seconds"  
;phaPropFFTnear,beam,  distance
;tnear(jj)=systime(1)-zeit


;zeit=systime(1) ; "1" entspricht "/seconds"  
;phaPropFFTfar, beam,  distance
;tfar(jj)=systime(1)-zeit


ENDFOR

mtfk = MEAN  (tfk, /DOUBLE )
dtfk = STDDEV( tfk , /DOUBLE )
;mtnear = MEAN  (tnear, /DOUBLE )
;dtnear = STDDEV( tnear , /DOUBLE )
;mtfar = MEAN  (tfar, /DOUBLE )
;dtfar = STDDEV( tfar , /DOUBLE )


print,'---------------------------------------------------------------------------------------'
print,'    N = ',n,'   Anzahl der Messungen : ',jjmax
print,'  tfk = ',mtfk,'+-',dtfk
;print,'tnear = ',mtnear,'+-',dtnear
;print,' tfar = ',mtfar,'+-',dtfar
print,'-------------------------------------------------------------------------------end-test'
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro benchmarkfft,n,jjmax


phainit

tnear=dblarr(jjmax)
tfar=dblarr(jjmax)

waist=0.2
d0=0
zmin=-1
ymin=zmin
zmax=-zmin
ymax=-ymin
lambda=20
distance=3000

print,'---------------------------------------------------------------------------------------'
print,' Benchmark mit Gaussquelle '
print,'---------------------------------------------------------------------------------------'
print,'Quelle: w0 [mm] =',double(waist),' ::::: Wellenlaenge l [nm] =',double(lambda),'' 
print,'        nx = ny =',double(n)    ,' ::::: Kantenlaenge s [mm] =',double(2*zmax),''
print,'         d [mm] =',double(distance) 
print,'---------------------------------------------------------------------------------------'



;---------------------------------------------------------------------------------------------


FOR jj=0,(jjmax-1) DO BEGIN


beam=phaSrcWFGauss( n , zmin , zmax , n, ymin , ymax , waist, d0 , lambda,1,1,0)



zeit=systime(1) ; "1" entspricht "/seconds"  
phaPropFFTnear,beam,  distance
tnear(jj)=systime(1)-zeit


zeit=systime(1) ; "1" entspricht "/seconds"  
phaPropFFTfar, beam,  distance
tfar(jj)=systime(1)-zeit


ENDFOR

mtnear = MEAN  (tnear, /DOUBLE )
dtnear = STDDEV( tnear , /DOUBLE )
mtfar = MEAN  (tfar, /DOUBLE )
dtfar = STDDEV( tfar , /DOUBLE )


print,'---------------------------------------------------------------------------------------'
print,'    N = ',n,'   Anzahl der Messungen : ',jjmax
print,'tnear = ',mtnear,'+-',dtnear
print,' tfar = ',mtfar,'+-',dtfar
print,'-------------------------------------------------------------------------------end-test'
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

