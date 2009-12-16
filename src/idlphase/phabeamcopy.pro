; Copies Beam-Structure

function phacopybeam, beam

ianzy= beam.ieyrey
ianzz= beam.ieyrex
ymin= beam.yeyremin
ymax= beam.yeyremax
dy=(ymax-ymin)/double(ianzy)
zmin= beam.xeyremin
zmax= beam.xeyremax
dz=(zmax-zmin)/double(ianzz)
xlam= beam.lambda
;zeyre:dblarr(256,256),zeyim:dblarr(256,256)
;	zezre:dblarr(256,256),zezim:dblarr(256,256),$

;  Wegen versch.Programmierern wird hier z zu x ...
;  phase intern heisst's x, in der phase-GUI und nun auch in IDL heisst's z
newbeam={mysource4,$
	zeyre:beam.zeyre,zeyim:beam.zeyim,$
	zezre:beam.zezre,zezim:beam.zezim,$
	xeyremin:zmin,xeyremax:zmax,dxeyre:dz,$
	xeyimmin:zmin,xeyimmax:zmax,dxeyim:dz,$
	yeyremin:ymin,yeyremax:ymax,dyeyre:dy,$
	yeyimmin:ymin,yeyimmax:ymax,dyeyim:dy,$
	xezremin:zmin,xezremax:zmax,dxezre:dz,$
	xezimmin:zmin,xezimmax:zmax,dxezim:dz,$
	yezremin:ymin,yezremax:ymax,dyezre:dy,$
	yezimmin:ymin,yezimmax:ymax,dyezim:dy,$
	lambda:xlam,$
	ieyrex:ianzz,ieyimx:ianzz,ieyrey:ianzy,ieyimy:ianzy,$
        iezrex:ianzz,iezimx:ianzz,iezrey:ianzy,iezimy:ianzy}  

return, newbeam

END
