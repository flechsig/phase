

;; filename:  ReadPhaseResults.pro

pro LoadEzReal, beam, fname

; init source4:  beam = {source4}


OpenR, lun, fname, /get_lun
readf, lun, nx, ny
cols=3
rows= nx * ny
data=dblarr(cols,rows)
readf, lun, data
Free_Lun, lun

beam.iezrex   = long(nx)
beam.xezremin = data(0,0)
beam.xezremax = data(0,rows-1)
beam.dxezre   = (beam.xezremax-beam.xezremin)/(beam.iezrex-1)

beam.iezrey=long(ny)
beam.yezremin = data(1,0)
beam.yezremax = data(1,rows-1)
beam.dyezre   = (beam.yezremax-beam.yezremin)/(beam.iezrey-1)


print,'nx ',nx,' ny ',ny,' cols ',cols,' rows ',rows
print,'x/y-min/max ',beam.xezremin,beam.yezremin,beam.xezremax,beam.yezremax


j=0
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		beam.zezre(ix,iy)=data(2,j)
		j=j+1
	endfor    
endfor

;	xeyremin:dblzero , xeyremax:dblzero , dxeyre:dblzero ,$
;	xeyimmin:dblzero , xeyimmax:dblzero , dxeyim:dblzero ,$
;	yeyremin:dblzero , yeyremax:dblzero , dyeyre:dblzero ,$
;	yeyimmin:dblzero , yeyimmax:dblzero , dyeyim:dblzero ,$
;	zeyre:dblarr(256,256) , zeyim:dblarr(256,256) ,$
;	xezremin:dblzero , xezremax:dblzero , dxezre:dblzero , $
;	xezimmin:dblzero , xezimmax:dblzero , dxezim:dblzero , $
;	yezremin:dblzero , yezremax:dblzero , dyezre:dblzero , $
;	yezimmin:dblzero , yezimmax:dblzero , dyezim:dblzero , $
;	zezre:dblarr(256,256) , zezim:dblarr(256,256) ,$
;	ieyrex:intzero , ieyimx:intzero , ieyrey:intzero , ieyimy:intzero , $
;     iezrex:intzero , iezimx:intzero , iezrey:intzero , iezimy:intzero   $


;return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro LoadEzImag, beam, fname

; init source4:  beam = {source4}


OpenR, lun, fname, /get_lun
readf, lun, nx, ny
cols=3
rows= nx * ny
data=dblarr(cols,rows)
readf, lun, data
Free_Lun, lun

beam.iezimx   = long(nx)
beam.xezimmin = data(0,0)
beam.xezimmax = data(0,rows-1)
beam.dxezim   = (beam.xezimmax-beam.xezimmin)/(beam.iezimx-1)

beam.iezimy=long(ny)
beam.yezimmin = data(1,0)
beam.yezimmax = data(1,rows-1)
beam.dyezim   = (beam.yezimmax-beam.yezimmin)/(beam.iezimy-1)


print,'nx ',nx,' ny ',ny,' cols ',cols,' rows ',rows
print,'x/y-min/max ',beam.xezimmin,beam.yezimmin,beam.xezimmax,beam.yezimmax


j=0
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		beam.zezim(ix,iy)=data(2,j)
		j=j+1
	endfor    
endfor

;	xeyremin:dblzero , xeyremax:dblzero , dxeyre:dblzero ,$
;	xeyimmin:dblzero , xeyimmax:dblzero , dxeyim:dblzero ,$
;	yeyremin:dblzero , yeyremax:dblzero , dyeyre:dblzero ,$
;	yeyimmin:dblzero , yeyimmax:dblzero , dyeyim:dblzero ,$
;	zeyre:dblarr(256,256) , zeyim:dblarr(256,256) ,$
;	xezremin:dblzero , xezremax:dblzero , dxezre:dblzero , $
;	xezimmin:dblzero , xezimmax:dblzero , dxezim:dblzero , $
;	yezremin:dblzero , yezremax:dblzero , dyezre:dblzero , $
;	yezimmin:dblzero , yezimmax:dblzero , dyezim:dblzero , $
;	zezre:dblarr(256,256) , zezim:dblarr(256,256) ,$
;	ieyrex:intzero , ieyimx:intzero , ieyrey:intzero , ieyimy:intzero , $
;     iezrex:intzero , iezimx:intzero , iezrey:intzero , iezimy:intzero   $


;return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro LoadeyReal, beam, fname

; init source4:  beam = {source4}


OpenR, lun, fname, /get_lun
readf, lun, nx, ny
cols=3
rows= nx * ny
data=dblarr(cols,rows)
readf, lun, data
Free_Lun, lun

beam.ieyrex   = long(nx)
beam.xeyremin = data(0,0)
beam.xeyremax = data(0,rows-1)
beam.dxeyre   = (beam.xeyremax-beam.xeyremin)/(beam.ieyrex-1)

beam.ieyrey=long(ny)
beam.yeyremin = data(1,0)
beam.yeyremax = data(1,rows-1)
beam.dyeyre   = (beam.yeyremax-beam.yeyremin)/(beam.ieyrey-1)


print,'nx ',nx,' ny ',ny,' cols ',cols,' rows ',rows
print,'x/y-min/max ',beam.xeyremin,beam.yeyremin,beam.xeyremax,beam.yeyremax


j=0
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		beam.zeyre(ix,iy)=data(2,j)
		j=j+1
	endfor    
endfor

;	xeyremin:dblzero , xeyremax:dblzero , dxeyre:dblzero ,$
;	xeyimmin:dblzero , xeyimmax:dblzero , dxeyim:dblzero ,$
;	yeyremin:dblzero , yeyremax:dblzero , dyeyre:dblzero ,$
;	yeyimmin:dblzero , yeyimmax:dblzero , dyeyim:dblzero ,$
;	zeyre:dblarr(256,256) , zeyim:dblarr(256,256) ,$
;	xezremin:dblzero , xezremax:dblzero , dxezre:dblzero , $
;	xezimmin:dblzero , xezimmax:dblzero , dxezim:dblzero , $
;	yezremin:dblzero , yezremax:dblzero , dyezre:dblzero , $
;	yezimmin:dblzero , yezimmax:dblzero , dyezim:dblzero , $
;	zezre:dblarr(256,256) , zezim:dblarr(256,256) ,$
;	ieyrex:intzero , ieyimx:intzero , ieyrey:intzero , ieyimy:intzero , $
;     iezrex:intzero , iezimx:intzero , iezrey:intzero , iezimy:intzero   $


;return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro LoadeyImag, beam, fname

; init source4:  beam = {source4}


OpenR, lun, fname, /get_lun
readf, lun, nx, ny
cols=3
rows= nx * ny
data=dblarr(cols,rows)
readf, lun, data
Free_Lun, lun

beam.ieyimx   = long(nx)
beam.xeyimmin = data(0,0)
beam.xeyimmax = data(0,rows-1)
beam.dxeyim   = (beam.xeyimmax-beam.xeyimmin)/(beam.ieyimx-1)

beam.ieyimy=long(ny)
beam.yeyimmin = data(1,0)
beam.yeyimmax = data(1,rows-1)
beam.dyeyim   = (beam.yeyimmax-beam.yeyimmin)/(beam.ieyimy-1)


print,'nx ',nx,' ny ',ny,' cols ',cols,' rows ',rows
print,'x/y-min/max ',beam.xeyimmin,beam.yeyimmin,beam.xeyimmax,beam.yeyimmax


j=0
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		beam.zeyim(ix,iy)=data(2,j)
		j=j+1
	endfor    
endfor

;	xeyremin:dblzero , xeyremax:dblzero , dxeyre:dblzero ,$
;	xeyimmin:dblzero , xeyimmax:dblzero , dxeyim:dblzero ,$
;	yeyremin:dblzero , yeyremax:dblzero , dyeyre:dblzero ,$
;	yeyimmin:dblzero , yeyimmax:dblzero , dyeyim:dblzero ,$
;	zeyre:dblarr(256,256) , zeyim:dblarr(256,256) ,$
;	xezremin:dblzero , xezremax:dblzero , dxezre:dblzero , $
;	xezimmin:dblzero , xezimmax:dblzero , dxezim:dblzero , $
;	yezremin:dblzero , yezremax:dblzero , dyezre:dblzero , $
;	yezimmin:dblzero , yezimmax:dblzero , dyezim:dblzero , $
;	zezre:dblarr(256,256) , zezim:dblarr(256,256) ,$
;	ieyrex:intzero , ieyimx:intzero , ieyrey:intzero , ieyimy:intzero , $
;     iezrex:intzero , iezimx:intzero , iezrey:intzero , iezimy:intzero   $


;return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
