 # File      : /afs/psi.ch/project/phase/GIT/phase/src/phasepython/myfftw3.py
 # Date      : <07 Mar 18 10:07:16 flechsig> 
 # Time-stamp: <21 Mar 18 15:18:34 flechsig> 
 # Author    : Flechsig Uwe, uwe.flechsig&#64;psi.&#99;&#104;

 # $Source$ 
 # $Date$
 # $Revision$ 
 # $Author$ 

import numpy as np

def myfftw3(field, direction, shift=1, verbose=1, 
            libpath='/afs/psi.ch/project/phase/lib/') :
    """myfftw3 - generic wrapper function for 2d fft using fftw3 

    the function is implemented in c, it allows forward and backward 
    transforms, as option it calls fftshift to center the output, the correct amplitude 
    scaling is debugged- the fft is normalized (energy is kept)

    Args:
        field (complex array): the complex field
        direction (int): -1 means forward fft, 1 means backward
        shift=1 (int): apply fftshift
        verbose=1 (int): verbosity
        libpath='/afs/psi.ch/project/phase/lib/' (string): the path to the shared library myfftw3.so

    Returns:
        field (complex array): the fft output
    
    Example:
          >>> b= myfftw3(a, -1)

    """

    re= field.real.copy()
    im= field.imag.copy()
    (rows, cols)= field.shape
#    libpath = '/afs/psi.ch/project/phase/lib/' 
    libname = 'myfftw3'                   # without extension
    if (verbose > 0) :
        print("myfftw3: call c code from {}.so".format(libpath+libname))
    myfftw3_lib = np.ctypeslib.load_library(libname, libpath)
    myfftw3 = myfftw3_lib.myfftw3
    arg1 = np.ctypeslib.as_ctypes(re)
    arg2 = np.ctypeslib.as_ctypes(im)
    
    #print("call", arg1)
    
    myfftw3(arg1, arg2, rows, cols, direction, shift, verbose)

    #print("returned")

    ore= np.ctypeslib.as_array(arg1)
    oim= np.ctypeslib.as_array(arg2)
    out= ore + oim *1j
    return out

# end myfftw3
