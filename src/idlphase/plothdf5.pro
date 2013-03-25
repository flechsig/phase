;;-*-idlwave-*-
fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.h5'
file_id     = H5F_OPEN(fname)
dataset_id1 = H5D_OPEN(file_id, 'slice000001/field')
dataset_id2 = H5D_OPEN(file_id, 'gridsize')
field0      = H5D_READ(dataset_id1)
gridsize    = H5D_READ(dataset_id2)

h5d_close, dataset_id1
h5d_close, dataset_id2
h5f_close, file_id

len   = n_elements(field0)/2
size  = fix(sqrt(len))
field2= reform(field0, 2, size, size)

print, 'size= ', size, ' gridsize= ', gridsize

real= reform(field2[0,*,*], size, size)
imag= reform(field2[1,*,*], size, size)

amp  = sqrt(real^2+imag^2)
phase= atan(imag,real)

print, 'real(0,0)= ',  real[0,0]  
print, 'imag(0,0)= ',  imag[0,0] 
print, 'real(10,0)= ', real[10,0] 
;;real[30,10]= 1e15

x0= dindgen(size)- size/2
x = x0* gridsize[0]* 1e3
y = x*1.0

;zone,2,2
;window,0
mycontour,real, x, y, title='real', xtitle='z (mm)', ytitle='y (mm)'
spng,'genesis-real.png'

;window,1
mycontour,imag,x,y,title='imag', xtitle='z (mm)', ytitle='y (mm)'
spng,'genesis-imag.png'

;window,2
mycontour,amp, x, y, title='amplitude', xtitle='z (mm)', ytitle='y (mm)'
spng,'genesis-ampl.png'

;window,3
mycontour,phase, x, y, title='phase', xtitle='z (mm)', ytitle='y (mm)'
spng,'genesis-phas.png'
