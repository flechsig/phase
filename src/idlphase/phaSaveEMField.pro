;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phaSaveEMField.pro
;  Date      : <20 Aug 13 09:05:44 flechsig> 
;  Time-stamp: <20 Aug 13 09:11:22 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

; Name: phaSaveEMField.pro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO phaSaveEMField, beam, MainFileName
;+
; NAME:
;	phaSaveEMField
;
; PURPOSE:
;       Save phasestyle EMFields from beamline
;
; CATEGORY:
;	pro : pha4idl - beamline
;
; CALLING SEQUENCE:
;	phaSaveEMField, bl, MainFileName
;
; INPUTS:
;     	bl		beamline struct
;	MainFileName	prefix for the phase EMField files
;			phase adds the following postfixes:
;			-ezre, -ezim, -eyre, -eyim  : real and imaginary 
;				of the EM-fields with z & y polarizaiton
;
; OUTPUTS:
;     	phase style EMField files on hdd
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
;      August 24, 2010, SG, modified to accept src4 instead of bl's
;-
np=n_params()

if np ne 2 then begin
	print, 'Wrong Number of arguments ...'
	print, 'Usage: SavePhaseResults, beam[source4], MainFileName[string] '
	print, ''
	return
endif

phaSaveEzReal,beam,MainFileName+'-ezrec'
phaSaveEzImag,beam,MainFileName+'-ezimc'
phaSaveEyReal,beam,MainFileName+'-eyrec'
phaSaveEyImag,beam,MainFileName+'-eyimc'

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSaveEzReal, beam, fname

; init source4:  beam = {source4}



nx = beam.iezrex
ny = beam.iezrey

cols=3
rows= nx * ny

data=dblarr(cols,rows)

j = 0L
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		data(0,j)= beam.xezremin + ix * beam.dxezre ; x-coord
		data(1,j)= beam.yezremin + iy * beam.dyezre ; y-coord
		data(2,j)= beam.zezre(ix,iy)                ; field 
		j=j+1
	endfor    
endfor

OpenW, lun, fname, /get_lun

printf, lun, nx , ny ; field dimension

printf, lun, data    ; data

Free_Lun, lun

print,'Saved EzReal to : ',fname

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSaveEzImag, beam, fname

; init source4:  beam = {source4}



nx = beam.iezimx
ny = beam.iezimy

cols=3
rows= nx * ny

data=dblarr(cols,rows)

j=0L
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		data(0,j)= beam.xezimmin + ix * beam.dxezim ; x-coord
		data(1,j)= beam.yezimmin + iy * beam.dyezim ; y-coord
		data(2,j)= beam.zezim(ix,iy)                ; field 
		j=j+1
	endfor    
endfor

OpenW, lun, fname, /get_lun

printf, lun, nx , ny ; field dimension

printf, lun, data    ; data

Free_Lun, lun

print,'Saved EzImag to : ',fname

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSaveEyReal, beam, fname

; init source4:  beam = {source4}



nx = beam.ieyrex
ny = beam.ieyrey

cols=3
rows= nx * ny

data=dblarr(cols,rows)

j=0L
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		data(0,j)= beam.xeyremin + ix * beam.dxeyre ; x-coord
		data(1,j)= beam.yeyremin + iy * beam.dyeyre ; y-coord
		data(2,j)= beam.zeyre(ix,iy)                ; field 
		j=j+1
	endfor    
endfor

OpenW, lun, fname, /get_lun

printf, lun, nx , ny ; field dimension

printf, lun, data    ; data

Free_Lun, lun

print,'Saved EyReal to : ',fname

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaSaveEyImag, beam, fname

; init source4:  beam = {source4}



nx = beam.ieyimx
ny = beam.ieyimy

cols=3
rows= nx * ny

data=dblarr(cols,rows)

j=0L
for iy=0, ny-1 do begin
	for ix=0, nx-1 do begin
		data(0,j)= beam.xeyimmin + ix * beam.dxeyim ; x-coord
		data(1,j)= beam.yeyimmin + iy * beam.dyeyim ; y-coord
		data(2,j)= beam.zeyim(ix,iy)                ; field 
		j=j+1
	endfor    
endfor

OpenW, lun, fname, /get_lun

printf, lun, nx , ny ; field dimension

printf, lun, data    ; data

Free_Lun, lun

print,'Saved EyImag to : ',fname

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO phaSaveEMFieldHDF5, beam, FileName, ziplevel = ZIP
;+
; NAME:
;	phaSaveEMFieldHDF5
;
; PURPOSE:
;       Save phasestyle EMFields from beamline in HDF5 file format
;
; CATEGORY:
;	pro : pha4idl - beamline
;
; CALLING SEQUENCE:
;	phaSaveEMFieldHDF5, beam, FileName, [ziplevel]
;
; INPUTS:
;     	beam		beam src4 struct
;	FileName	the name of the HDF5 file to be created
;	ziplevel	optional level of data compression, 
;                       0 if none is passed
;                       9 is highest (takes most processing time)
;
; OUTPUTS:
;     	EMField files on hdd in HDF5
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      March 23, 2011, SG, initial version
;-

if KEYWORD_SET(ZIP) then ziplevel=ZIP $
  else ziplevel=0;

print, 'Saving fields to file ', FileName, ' with compression=', ziplevel;

fid = h5F_create(FileName);

nz = beam.iezrex; TODO: wrong convention
ny = beam.iezrey;

lambda = DOUBLE(beam.xlam);

origin = DBLARR(2);
delta = DBLARR(2);

origin(0) = beam.xezremin;
origin(1) = beam.yezremin;

delta(0) = beam.dxezre;
delta(1) = beam.dyezre;

datatype_double_id = H5T_IDL_CREATE(lambda);  
dataspace_id = H5S_CREATE_SIMPLE(1);

; store the wavelength
dataset_id = H5D_CREATE(fid, 'lambda', datatype_double_id, dataspace_id);     
H5D_WRITE, dataset_id, lambda;
H5D_CLOSE,dataset_id    
H5S_CLOSE,dataspace_id  

; store the origin vector (y0, z0)
dataspace_id = H5S_CREATE_SIMPLE(2);

dataset_id = H5D_CREATE(fid, 'origin', datatype_double_id, dataspace_id);     
H5D_WRITE, dataset_id, origin;
H5D_CLOSE,dataset_id    

; store the delta vector (dy, dz)
dataset_id = H5D_CREATE(fid, 'delta', datatype_double_id, dataspace_id);     
H5D_WRITE, dataset_id, delta;
H5D_CLOSE,dataset_id;    

H5S_CLOSE,dataspace_id;  


; store the fields in extra group
group_id = H5G_CREATE(fid, 'data');
dataspace_id = H5S_CREATE_SIMPLE([ny, nz], max_dimensions=[2048, 2048]);

dataset_id = H5D_CREATE(group_id,'eyre', datatype_double_id, dataspace_id, CHUNK_DIMENSIONS=[256, 256], GZIP=ziplevel)  
H5D_WRITE, dataset_id, beam.zeyre(0:ny-1, 0:nz-1); save portion of grid which is actually used
print,'Saved field eyre to ', FileName

dataset_id = H5D_CREATE(group_id,'eyim', datatype_double_id, dataspace_id, CHUNK_DIMENSIONS=[256, 256], GZIP=ziplevel)  
H5D_WRITE, dataset_id, beam.zeyim(0:ny-1, 0:nz-1); 
print,'Saved field eyim to ', FileName

dataset_id = H5D_CREATE(group_id,'ezre', datatype_double_id, dataspace_id, CHUNK_DIMENSIONS=[256, 256], GZIP=ziplevel)  
H5D_WRITE, dataset_id, beam.zezre(0:ny-1, 0:nz-1);
print,'Saved field ezre to ', FileName

dataset_id = H5D_CREATE(group_id,'ezim', datatype_double_id, dataspace_id, CHUNK_DIMENSIONS=[256, 256], GZIP=ziplevel)  
H5D_WRITE, dataset_id, beam.zezim(0:ny-1, 0:nz-1);
print,'Saved field ezim to ', FileName

   
H5G_CLOSE, group_id;
H5S_CLOSE,dataspace_id;

H5T_CLOSE,datatype_double_id;
H5F_CLOSE, fid;
END
