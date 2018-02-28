import h5py
import numpy as np

def h5_write_surf_error(fname, ename, u, w, l, verbose=True) :
   """h5_write_surf_error - write surface error into hdf5

   Args:
      verbose=True (bool): verbosity

      Example:
          >>> h5_write_surf_error() 
              

   """

   usage= "usage: h5_write_surf_error('surf_err.h5', 'ename', u, w, l)"
   print("Usage: ", usage) 

# make double
   u*= 1.0
   w*= 1.0
   l*= 1.0

   nw= w.size
   nl= l.size
   nu= u.size

   nwnl= nw* nl

   if nu != nwnl :
       print("dimension error")
       return

   f= h5py.File(fname, 'w')
   f.create_dataset('/height2D', data=u)
   f.create_dataset('/height_vec', data=u.reshape((nwnl)))
   f.create_dataset('/wvec', data=w)
   f.create_dataset('/lvec', data=l)

   f.close()
   print('wrote surface error file: ', fname)
