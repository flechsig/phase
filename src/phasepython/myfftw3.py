 # File      : /afs/psi.ch/project/phase/GIT/phase/src/phasepython/myfftw3.py
 # Date      : <07 Mar 18 10:07:16 flechsig> 
 # Time-stamp: <07 Mar 18 12:18:04 flechsig> 
 # Author    : Flechsig Uwe, uwe.flechsig&#64;psi.&#99;&#104;

 # $Source$ 
 # $Date$
 # $Revision$ 
 # $Author$ 

import numpy as np

def myfftw3(field, direction) :
    """myfftw3 - generic wrapper function for 2d fft using fftw3 immplemnted in c

    Args:
        field (complex array): real part of the field
        direction (int): -1 means forward fft, 1 means backward

    Returns:
        field (complex array): the fft output
        

    Example:
          >>> b= myfftw3(a, -1)

    """

    re= field.real.copy()
    im= field.imag.copy()
    (rows, cols)= field.shape
    libpath = '/afs/psi.ch/project/phase/lib/' 
    libname = 'myfftw3'                   # without extension
    print("my fftw3 call c code from {}.so".format(libpath+libname))
    myfftw3_lib = np.ctypeslib.load_library(libname, libpath)
    fft = myfftw3_lib.myfftw3
    arg1 = np.ctypeslib.as_ctypes(re)
    arg2 = np.ctypeslib.as_ctypes(im)
    
    #print("call", arg1)
    
    fft(arg1, arg2, rows, cols, direction)

    #print("returned")

    ore= np.ctypeslib.as_array(arg1)
    oim= np.ctypeslib.as_array(arg2)
    out= ore + oim *1j
    return out

# end myfftw3
