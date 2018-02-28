import h5py
import numpy as np

def h5_read_surf_error(fname, verbose=True) :
   """h5_write_surf_error - write surface error into hdf5

   Args:
      verbose=True (bool): verbosity

   Returns:
      dictionary

   Example:
          >>> dict= h5_read_surf_error('surf_err.h5') 
              

   """

   usage= "usage: h5_read_surf_error('surf_err.h5', 'ename', u, w, l)"
   print("Usage: ", usage) 



   f= h5py.File(fname)
   u2= f['/height2D'][:]
   u1= f['/height_vec'][:]
   w= f['/wvec'][:]
   l= f['/lvec'][:]
   f.close()
   print('read surface error file: ', fname)

   dict= {'u2': u2, 'u1': u1, 'wvec': w, 'lvec': l}

   return dict
