# File      : /afs/psi.ch/project/phase/GIT/phase/src/phasepython/reflec.py
# Date      : <23 Aug 17 16:01:05 flechsig> 
# Time-stamp: <28 Aug 18 17:07:40 flechsig> 
# Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

# $Source$ 
# $Date$
# $Revision$ 
# $Author$  

import os.path
import numpy as np
import matplotlib.pyplot as plt
import atomparser

def reflec(element, en, theta, d=1.0, log='', plot='reflectivity', verbose=True) :

   """calculate reflectivity of a thick mirror as function of photon energy 
      and the transmittance of a foil with thickness d 

      ! for very thin layers we have to combine transmission tp, ts and trans !

   Args:
      element (str): element as chemical formula (String, case sensitive)
      energy (double): energy vector
      theta (double): Grazing incidence angle (rad) 
      d=1.0 (double): thickness of the foil or gas in m 
      log='' (str): logscal for axis ['', 'x', 'y', 'xy']   
      plot='reflectivity' (str): plot ['', 'reflectivity', 'transmittance']
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

   usage= "usage: dict=reflec('Au', en, 4e-3, d=1e-4)"

   print("Usage: ", usage) 

   (en0, f10, f20) = readhenke(element)
   (Z  , A  , rho) = readmaterial(element)

   if (en == -1) or (Z == -1) :
      dict = atomparser.parse_formula(element)
      print("compounds not yet implemented", dict)
      return

#   if isinstance( en, int ):                                  # UF does not work 
#      en = en0

   f1 = np.interp(en, en0, f10)
   f2 = np.interp(en, en0, f20)

   NA = 6.0221e23                                              # Avogadronumber
   re = 2.81794e-15                                            # Classical electron radius (m)
   Nt = 1e6* rho * NA / A                                      # Teilchendichte  (1/m^3), rho is in (g/cm^3)
   wavelength = 1239.842e-9/ en                                # Wavelength      (m)

   delta = re * wavelength**2 * Nt * f1 / (2.0 * np.pi)
   beta  = re * wavelength**2 * Nt * f2 / (2.0 * np.pi)
   
   #n     = np.complex(1.0 - delta, beta)                      # complex index of refraction
   n = np.zeros(len(delta), dtype=complex)
   for i in np.arange(len(delta)) :
      n[i]= complex(1.0 - delta[i], beta[i])

   ac  = np.arccos(1.0 - delta)                                # critical (grazing) angle in rad
   wu  = np.sqrt( n**2 - (np.cos(theta))**2 )                  # Fresnel - formulas
   crs = (      np.sin(theta) - wu ) / ( np.sin(theta) + wu)   # reflection coeff. s-pol
   cts = (  2 * np.sin(theta)      ) / ( np.sin(theta) + wu)   # transmiss. coeff. s-pol
   
   crp = (n**2 * np.sin(theta) - wu ) / ( n**2 *np.sin(theta) + wu)  # reflection coeff. p-pol
   ctp = (2*n * np.sin(theta)      ) / ( n**2 *np.sin(theta) + wu)   # transmiss. coeff. s-pol
   
   Rs  =  np.abs(crs)**2                                          # reflectance s-pol   
   Rp  =  np.abs(crp)**2                                          # reflectance p-pol.  
   Ts  =  np.abs( 2*  wu / (       np.sin(theta) + wu))**2        # transmitance s-pol        
   Tp  =  np.abs( 2*n*wu / (n**2 * np.sin(theta) + wu))**2        # transmitance p-pol  

   mu  = 2 * re * wavelength * Nt * f2                            # mu linear absorption coefficient 
   trans = np.exp(-d * mu)                                        # transmission through a filter
   
   if plot == 'reflectivity' :
      thetag= theta*180./np.pi 
      title = element + ',   grazing angle = {:.3f} mrad = {:.3f} deg.'.format(theta*1e3,thetag)
      plt.plot(en, Rs, label='Rs')
      plt.plot(en, Rp, label='Rp')
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
      print("readhenke: did not found table: ", fname)
      return -1, -1, -1

   field1 = np.loadtxt(fname, skiprows=1)
   en= field1[:, 0]
   f1= field1[:, 1]
   f2= field1[:, 2]
   if verbose :
      print("read Henke table from: ", fname)
   return en, f1, f2
#end readhenke

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
         print("found: ", el, z, r, t)
         return int(z), float(r), float(t)

   return -1, -1, -1
   print("did not found element: ", el)
#end readmaterial

