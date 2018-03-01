import h5py
import numpy as np
import PyQt4.QtGui

def print_attrs(name, obj):
   print("name= ", name)

def h5_read_surf_error(fname=None, ename=None, verbose=True) :
   """h5_write_surf_error - write surface error into hdf5

   Args:
      fname (string): filename
      ename (string): elementname
      verbose=True (bool): verbosity

   Returns:
      dictionary

   Example:
          >>> dict= h5_read_surf_error('surf_err.h5', 'M11') 
              

   """

#   usage= "usage: h5_read_surf_error('surf_err.h5', 'ename')"
#   print("Usage: ", usage) 

   if (fname == None):
      fname= PyQt4.QtGui.QFileDialog.getOpenFileName(None, 'Open file', '.', 
                                                      "hdf5 files (*.h5);;All files (*.*)")

   f= h5py.File(fname)

   if (ename == None):
# List all groups
      print("no ename provided - list available keys")
#      print("Keys: %s" % f.keys()
      #a_group_key = list(f.keys())[0]
      f.visititems(print_attrs)
      return

   u2= f[ename+'/height2D'][:]
   u1= f[ename+'/height_vec'][:]
   w= f[ename+'/wvec'][:]
   l= f[ename+'/lvec'][:]
   f.close()
   print('read surface error file: ', fname)

   dict= {'u2': u2, 'u1': u1, 'wvec': w, 'lvec': l}

   return dict
