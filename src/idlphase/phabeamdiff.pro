; Calculates the difference of the EM-fields in 2 Beam-Structures

function phabeamdiff, beam1, beam2

ianzy= beam1.ieyrey
ianzz= beam1.ieyrex
ymin= beam1.yeyremin
ymax= beam1.yeyremax
dy=(ymax-ymin)/double(ianzy)
zmin= beam1.xeyremin
zmax= beam1.xeyremax
dz=(zmax-zmin)/double(ianzz)
xlam= beam1.lambda

;  Wegen versch.Programmierern wird hier z zu x ...
;  phase intern heisst's x, in der phase-GUI und nun auch in IDL heisst's z
beamdiff={mysource4,$
	zeyre:beam1.zeyre-beam2.zeyre,zeyim:beam1.zeyim-beam2.zeyim,$
	zezre:beam1.zezre-beam2.zezre,zezim:beam1.zezim-beam2.zezim,$
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

return, beamdiff

END
