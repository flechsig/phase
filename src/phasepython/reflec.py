# File      : /afs/psi.ch/project/phase/GIT/phase/src/phasepython/reflec.py
# Date      : <23 Aug 17 16:01:05 flechsig> 
# Time-stamp: <2021-09-13 14:27:58 flechsig> 
# Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

# $Source$ 
# $Date$
# $Revision$ 
# $Author$  

import re
import os.path
import numpy as np
import matplotlib.pyplot as plt
import atomparser

COMMON_MATERIALS = {'Diamond': ['C', 3.51],
                    'Quartz':  ['SiO2', 2.65],
                    'Silica':  ['SiO2', 2.2],
                    'ULE':     ['Si0.925Ti0.075O2', 2.205],
                    'Zerodur': ['Si0.56Al0.5P0.16Li0.04Ti0.02Zr0.02Zn0.03O2.46', 2.53],
                    'polyimide': ['C22H10N2O5', 1.43],
                    'boron nitride': ['BN', 2.25],
                    'silicon nitride': ['Si3N4', 3.44],
                    'polypropylene': ['C3H6', 0.9],
                    'PMMA': ['C5H8O2', 1.19],
                    'polycarbonate': ['C16H14O3', 1.2],
                    'mylar': ['C10H8O4',1.4],
                    'Teflon': ['C2F4', 2.2],
                    'Parylene-C': ['C8H7Cl', 1.29],
                    'Parylene-N': ['C8H8', 1.11],
                    'Air': ['N1.562O.42C.0003Ar.0094', -1],
                    'P-10': ['Ar.9C.1H.4', -1],
                    'Methane': ['C1H4', -1],
                    'Propane': ['C3H8', -1],
                    }  # dict with formula and density

def reflec(element, en, theta, d=1.0, log='', p=-1, plot='reflectivity', punit='pascal', rho=-1, T=-1, verbose=True) :

   """calculate reflectivity of a thick mirror as function of photon energy 
      and the transmittance of a foil with thickness d 

      ! for very thin layers we have to combine transmission tp, ts and trans !

   Args:
      element (str): element as chemical formula (String, case sensitive)
      energy (double): energy vector
      theta (double): Grazing incidence angle of mirror (rad) 
      d=1.0 (double): thickness of the foil or gas in m 
      log='' (str): logscal for axis ['', 'x', 'y', 'xy']   
      p=-1 (float): gas pressure in Pa (pressure in mbar x 100)
      plot='reflectivity' (str): plot ['', 'reflectivity', 'transmittance'], other strings: no plot
      punit='pascal' (str): pressure unit for gas ['pascal', 'mbar', 'torr'...]
      rho=-1 (float): density in g/cm^3 - overwrites value from table(s)
      T=-1 (float): gas temperature in K
      verbose=True (bool): verbosity

   Returns:
      result (dictionary):
         ac:      critical grazing angle in rad (output)

         crp:     complex reflectivity rp (output

         crs:     complex reflectivity rs (output

         beta:    Im(n)= beta, imaginary part of refractive index (output)

         delta:   Re(n)= 1-delta, real part of refractive index (output)

         mu:      linear absorption coefficient (output)

         n:       complex refractive index n= 1-delta + j*beta (output)

         rp:      reflectivity rp

         rs:      reflectivity rs

         tp:      transmission tp

         ts:      transmission ts

         trans:   transmission of a foil

   Example:
          >>> en=np.arange(101)/100*970+30 
              dict=reflec('Au', en, 4e-3)
              print('Rp= ', dict['rp'])
   """

   usage= "usage example: dict=reflec('Au', en, 4e-3, d=1e-4)"

   if verbose:
      print("Usage: ", usage) 

   elements, rho0 = _check_common_materials(element, verbose=verbose)  # check for common materials first
   
   if rho0 < -5 :                       # element not found in common_materials hand it over to atomparser
      elements = element
   
   dict = atomparser.parse_formula(elements)
   
   (f1, f2, ni) = _readhenkes(en, dict, verbose=verbose)
   (Z, A, rho1) = _readmaterials(dict, verbose=verbose)
   if verbose:
      print("readmaterials returns Z= {}, A= {}, rho= {} ".format(Z, A, rho1))
      print("ni = {} mol".format(ni))

   R0 = 8.3144598                                              # Gas constant Nm/(K mol) or m^3 Pa/(K mol)
   
   if rho > 0 :         # command line input has highest priority                    
      if verbose:
         print("take rho from input ({})".format(rho))
   else :
      if rho0 > 0 :   
         rho = rho0
         if verbose:
            print("take rho from common materials ({})".format(rho))
      else :
         if T > 0 and p > 0 :                 # gas input
            # we can calculate rho= P * A /( R0 * T) but how to take into account O2 ???
            # there is still something wrong with the molar mass
            p = p2pa(p, punit, verbose=verbose)
            rho= 1e-6 * p * A /( R0 * T)
            # Nt = p / (T * KB)   
            if verbose :
               print("gas transmittance mode (rho= {} g/cm^3,  p= {:.3e} Pa, A={} g/mol)".format(rho, p, A))
               print("!!!results for compound gases may be not correct - 2b debugged!!!")
         else :
            rho = rho1
            if verbose:
               print("take averaged rho from table(s) ({})".format(rho))

   
    
   KB = 1.38066e-23                                            # Boltzmann constant Nm/K
   NA = 6.0221e23                                              # Avogadronumber
   re = 2.81794e-15                                            # Classical electron radius (m)
   Nt = 1e6* ni * rho * NA / A                                 # Teilchendichte  (1/m^3), rho is in (g/cm^3)
   wavelength = 1239.842e-9/ en                                # Wavelength      (m)

   if verbose :
      print("debug Nt= {:.3e} m^-3, p= {:.3e} Pa, A={} g/mol)".format(Nt,p, A))
   
   delta = re * wavelength**2 * Nt * f1 / (2.0 * np.pi)
   beta  = re * wavelength**2 * Nt * f2 / (2.0 * np.pi)
   
   #n     = np.complex(1.0 - delta, beta)                      # complex index of refraction
   n = np.zeros(delta.size, dtype=complex)
   for i in np.arange(delta.size) :
      n[i]= complex(1.0 - delta[i], beta[i])

   arg = 1.0 - delta
   if np.any(np.abs(arg) > 1) :
       #verbose = True
       if verbose :
          idxv = np.where(np.abs(arg) > 1)
          print("warning: critical grazing angles not defined for the following energies: {}".format(en[idxv]))
       ac = np.arccos(np.clip(arg, -1, 1))                     # critical (grazing) angle in rad
   else :
       ac = np.arccos(arg)                                     # critical (grazing) angle in rad

   wu  = np.sqrt( n**2 - (np.cos(theta))**2 )                  # Fresnel - formulas
   crs = (      np.sin(theta) - wu ) / ( np.sin(theta) + wu)   # reflection coeff. s-pol
   cts = (  2 * np.sin(theta)      ) / ( np.sin(theta) + wu)   # transmiss. coeff. s-pol
   
   crp = (n**2 * np.sin(theta) - wu ) / ( n**2 *np.sin(theta) + wu)  # reflection coeff. p-pol
   ctp = (2* n * np.sin(theta)      ) / ( n**2 *np.sin(theta) + wu)  # transmiss. coeff. s-pol
   
   Rs  =  np.abs(crs)**2                                          # reflectance s-pol   
   Rp  =  np.abs(crp)**2                                          # reflectance p-pol.  
   Ts  =  np.abs( 2*  wu / (       np.sin(theta) + wu))**2        # transmitance s-pol        
   Tp  =  np.abs( 2*n*wu / (n**2 * np.sin(theta) + wu))**2        # transmitance p-pol  

   mu  = 2 * re * wavelength * Nt * f2                            # mu linear absorption coefficient 
   trans = np.exp(-d * mu)                                        # transmission through a filter
   
   if plot == 'reflectivity' :
      thetag= theta*180./np.pi 
      title = element + ',   grazing angle = {:.3f} mrad = {:.3f} deg.'.format(theta*1e3,thetag)
      plt.plot(en, Rs, label='{}: Rs'.format(element))
      plt.plot(en, Rp, label='{}: Rp'.format(element))
      plt.legend()
      plt.ylabel('reflectivity')
      plt.xlabel('photon energy (eV)')
      if log == 'x' :
         plt.semilogx()
      if log == 'y' :
          plt.semilogx()
      if log == 'xy' :
         plt.loglog()
      plt.title(title)
      plt.show()
   # end plot 

   if plot == 'transmittance' :
      thetag= theta*180./np.pi 
      title = element + ',   thickness = {:.2e} m'.format(d)
      plt.plot(en, trans, label=element)
      plt.legend()
      plt.ylabel('transmittance')
      plt.xlabel('photon energy (eV)')
      if log == 'x' :
         plt.semilogx()
      if log == 'y' :
          plt.semilogx()
      if log == 'xy' :
         plt.loglog()
      plt.title(title)
      plt.show()
   # end plot 
   
   # return data, access example: a= reflec(...)  print(a['rs'])
   dict= {'ac': ac, 'crp': crp, 'crs': crs, 'beta': beta, 'delta': delta, 'n': n, 
          'rp': Rp, 'rs': Rs, 'tp': Tp, 'ts': Ts, 'mu': mu, 'trans': trans}
 
   return dict 
   # end reflec

def p2pa(p, punit='pascal', verbose=True) :
   """calculates pressure in pascal

   Args:
      p (flt): pressure
      punit='pascal' (str): ['pascal', 'mbar', 'torr'...]

   Returns: (flt) pressure in Pa
      
   Example:
          >>> ppa= p2pa(p, punit='torr')

   """

   factors= {'pascal': 1.0, 'bar': 1e5, 'mbar': 1e2, 'torr': 133.322, 'mm Hg':133.322, 
             'at' : 0.981e5,  # techn. atm kp/cm^2 
             'atm': 1.013e5,  # phys atm
             'psi': 6895.,    # lb/in^2
            }

   if punit in factors :
      factor = factors[punit]
   else : 
      raise ValueError("error: unknown pressure unit: {}".format(punit))
   if verbose :
      print("{} {} = {} Pa".format(p, punit, p * factor))

   return p * factor

#end p2pa

def _readhenkes(en, tuples, verbose=True) :
   """read a material dictionary returns averages plus interpolation"""
   w1 = 0.0
   f1 = 0.0
   f2 = 0.0
   
   for atom in tuples:
      w = tuples[atom]
      (en0, f10, f20) = readhenke(atom, verbose=verbose)
      f11 = np.interp(en, en0, f10)
      f22 = np.interp(en, en0, f20)
      f1 += w * f11
      f2 += w * f22
      w1 += w
      
   if w1 > 0.0 :
      return f1/w1, f2/w1, w1   # return averages and sum
   else :
      raise ValueError("_readhenkes error -> not tablulated material")
#end _readhenkes

def readhenke(element, verbose=True) :
   """readhenke read henke tables (path hardcoded)

   Args: 
      element (str): element
      verbose=True (bool): verbosity
   """

   path ='/afs/psi.ch/project/soft_x/OpticsTools/ray_reflec/henke/'
   ext = '.f12'
   fname = path + element + ext
   
   if os.path.isfile(fname) == False :
      errst= "readhenke: did not found table: {}".format(fname)
      raise ValueError(errst)
     
   field1 = np.loadtxt(fname, skiprows=1)
   en= field1[:, 0]
   f1= field1[:, 1]
   f2= field1[:, 2]
   if verbose :
      print("read Henke table from: ", fname)

   return en, f1, f2
#end readhenke

def _readmaterials(tuples, verbose=True) :
   """read a material dictionary returns averages and molar mass respectively """

   w1 = 0.0
   z1 = 0.0
   a1 = 0.0
   r1 = 0.0
   for atom in tuples:
      w = tuples[atom]    
      (z, a, r) = readmaterial(atom, verbose=verbose)
      z1 += w * z
      a1 += w * a 
      r1 += w * r
      w1 += w
      
   if w1 > 0.0 :
      return z1/w1, a1, r1/w1   # return averages for z snd rho and molar mass

   raise ValueError("sum of weights == 0")
# end _readmaterials

def readmaterial(element, verbose=True) :
   """read material table (path hardcoded)

   Args: 
      element (str): element
      verbose=True (bool): verbosity
   """

   fname = '/afs/psi.ch/project/soft_x/OpticsTools/ray_reflec/material/rhoatom_idl.dat'
   if verbose :
      print("read mat table from: ", fname)

   f = open(fname, 'r')
   for line in f:
      el, z, r, t = line.strip().split()
      if element == el :
         if verbose :
            print("readmaterial -> found: ", el, z, r, t)
         return int(z), float(r), float(t)

   raise ValueError("did not found element: ", el)
#end readmaterial

def _check_common_materials(element, verbose=True) :
   """check for common materials"""

   formula = ""
   density = -9
   if element in COMMON_MATERIALS :
      formula= COMMON_MATERIALS[element][0]
      density= COMMON_MATERIALS[element][1]
      if verbose:
         print("found >>{}<< in table of common materials: formula= {}, density= {} g/cm^3".format(element, formula, density))
   
   return formula, density   
#end _check_common_materials()
