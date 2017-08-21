# File      : /afs/psi.ch/project/phase/GIT/phase/src/phasepython/phase.py
# Date      : <15 Aug 17 16:25:49 flechsig> 
# Time-stamp: <18 Aug 17 17:01:50 flechsig> 
# Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

# $Source$ 
# $Date$
# $Revision$ 
# $Author$ 

# ******************************************************************************
#
#   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
#                      Paul Scherrer Institut Villigen, Switzerland
#   
#   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
#          Uwe Flechsig,    uwe.flechsig@psi.ch
#
# ------------------------------------------------------------------------------
#
#   This file is part of PHASE.
#
#   PHASE is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, version 3 of the License, or
#   (at your option) any later version.
#
#   PHASE is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY# without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with PHASE (src/LICENSE).  If not, see <http:#www.gnu.org/licenses/>. 
#
# ******************************************************************************

import numpy as np
import matplotlib.pyplot as plt

class emf(object):
   """main object of an electromagnetic field """
   def __init__( self ):
      """ constructor """
      self.name = ""
      self.wavelength = 1e-10
      self.y_vec = np.array([0.0])
      self.z_vec = np.array([0.0])
      self.field = np.array([[0.0+0.0j],[0.0+0.0j]])

   def __str__(self):
        if self.name != '':
            s = "dataset name: \"%s\",  wavelength: %e m" % (
                self.name, self.wavelength)
        else:
            s = "no data (nothing read in)"

        return s   

   def gaussbeam(self, dist=None, drift=None, w0=1e-5, Nz=243, Ny=None, sizez=1e-3, sizey=None, \
               wavelength=1e-10, plot=False, example=False, \
               z_off=0.0, y_off=0.0):
       """generate the electromagnetic field of a gaussian beam and fill the emf

       Args:
          dist=None (double): the drift distance in m
          drift=None (double): the drift distance in m (overwrites dist)
          w0=1e-5 (double): w0
          Nz=243 (int): number of points in z (horizontal)
          Ny=None (int): number of points in y (vertical), default is Nz
          sizez=1e-3 (double): size z (horizontal) in m 
          sizey=None (double): size y (vertical) in m, default is sizez
          wavelength=1e-10 (double): wavelength
          plot=False (bool): plot the intensity
          example=False (bool): make an example (includes the plot)
          z_off=0.0 (double): offset z in m
          y_off=0.0 (double): offset y in m

       Example:
          >>> emf = phase.initphase()
              emf.gaussbeam(example=True)   
       """
       self.name = 'Gaussbeam'

       if example is True :
           print('**********************************************************')
           print('example: HeNe Laser ')
           print('wavelength=633e-9, w0= 1e-3, dist= 10., sizez=1e-2')
           print('**********************************************************')
           #self.gaussbeam(dist=10., wavelength=633e-9, w0=1e-3, sizez=1e-2, plot=True)
           self.gaussbeam(dist=10., wavelength=633e-9, w0=1e-3, sizez=1e-2, plot=True)
           print('**********************************************************')
           print('end example')
           print('**********************************************************')
           return
   
       if drift is not None:
           dist= drift
       if Ny is None:
           Ny= Nz
       if sizey is None:
           sizey= sizez

       field  = np.zeros((Nz, Ny), dtype=complex) 
       z_vec  = np.linspace(-0.5*sizez, 0.5*sizez, Nz)  
       y_vec  = np.linspace(-0.5*sizey, 0.5*sizey, Ny)
       dz     = z_vec[1]- z_vec[0]
       dy     = y_vec[1]- y_vec[0]

       print( 'wavelength (m) = ', wavelength)
       print( 'Nz     = ', Nz      , ', Ny     = ', Ny)
       print( 'sizez (m) = ', sizez   , ', sizey (m) = ', sizey)
       print( 'dz (m)    = ', dz      , ', dy (m)    = ', dy)
       print( 'z_off (m) = ', z_off   , ', y_off (m) = ', y_off)
       print( 'w0    (m) = ', w0      , ', dist  (m) = ', dist)

       k   = np.pi * 2    / wavelength        # wave number
       z0  = np.pi * w0**2 / wavelength       # Rayleigh Range
       w   = w0 * np.sqrt(1.0+ (dist/z0)**2)  # w(dist)
       w2  = w**2
       eta = np.arctan(dist/z0)
       Ri  = dist / (dist**2 + z0**2)         # curvature Ri  = 1/R;

       print( 'z0    (m) = ', z0, ' (Rayleigh Range= +/- z0)')
       print( 'w     (m) = ', w   ,', w2 (m^2) = ', w2)
       print( 'eta (rad) = ', eta ,', Ri (1/m) = ', Ri)

       truncation= 0 
       for i in range(0, Nz-1):
          for j in range(0, Ny-1):
             rho2  =  (z_vec[i]-z_off)**2 + (y_vec[j]-y_off)**2 
             arg1  = -1 *  rho2 / w2               # the intensity factor as function of aperture
             if arg1 <= -40: 
                 arg1 = -40                        #  -40, but -80 is still ok
                 truncation= 1
       
             arg2  = 0.5 * k * rho2 * Ri + k*dist - eta                    # For notation of Siegman multiply by -1                    
             phas2 = complex(np.cos(arg2), np.sin(arg2))     
             field[i,j]= phas2 * np.exp(arg1) * w0 / w
       
       
       # norm to 0.5 W  !! we assume only one polarization 
       intensity = np.absolute(field)**2/377.0
       binsize= (z_vec[1]-z_vec[0])*(y_vec[1]-y_vec[0])
       itot= np.sum(intensity)*binsize*2.0
       scale= 1.0/np.sqrt(itot)
       field*= scale

       if truncation > 0:
           print ('!! warning -- some outside points are truncated !!')

# plot using mycontour
#       plot= False
       if plot is True:
           print('plot')
           bamp = np.absolute(field)
           #window, 20
           #stat = dblarr(7)
           #fit   = gauss2dfit(bamp,    stat, z_vec, y_vec) 
           #fit2  = gauss2dfit(bamp^2, stat2, z_vec, y_vec) 
           #print 'gaussfit amplitude: rms_z, rms_y (m)= ', stat(2),  stat(3)
           #print 'gaussfit intensity: rms_z, rms_y (m)= ', stat2(2), stat2(3)
           #title= 'gaussbeam intensity '+  'size='+  string(stat2(2)*1e6,FORMAT="(f6.1)")+ ' x ' + string(stat2(3)*1e6, FORMAT="(f6.1)") + textoidl(' \mum^2 rms')
           self.mycontour(bamp, z_vec*1e3, y_vec*1e3, xlabel='z (mm)', ylabel='y (mm)', zlabel='intensity $(W/m^2)$', title=self.name)

           pha = np.angle(field)
           if np.amax(pha)- np.amin(pha) > 1e-10: 
               print( "no yet ")
               #window,21
               #mycontour, pha, z_vec*1e3, y_vec*1e3, xtitle='z (mm)', ytitle='y (mm)', title='gaussbeam phase'
           else:
              print( 'phase(z,y) is zero- no phase plot')
              #device,window_state=window_list
              #if window_list[21] gt 0 then wdelete, 21
              #endelse
           #endif  plot

       self.wavelength= wavelength
       self.field= field
       self.z_vec= z_vec
       self.y_vec= y_vec

   def mycontour(self, z, x, y, xlabel='z (mm)', ylabel='y (mm)', zlabel='intensity (W)', title='title'): 
       """make a contour plot (helper function)

       Args:
       
       Example:
          >>> emf = phase.initphase()
              emf.gaussbeam(example=True, plot=False)
              ph= emf.getphase()
              emf.mycontour(ph)

       """
       cp= plt.contour(x, y, z, 15, linewidths = 0.5, colors = 'k')  # contour linien
       plt.pcolormesh(x, y, z, cmap = plt.get_cmap('rainbow'))       # farbe
       cbar= plt.colorbar() 
       cbar.set_label(zlabel)#, rotation=270)
       plt.xlabel(xlabel)
       plt.ylabel(ylabel)
       plt.title(title)
       plt.clabel(cp, inline=True, 
                  fontsize=10)
       plt.show()
       # mycontour
   
   def propfraunhofer(self, drift=None):
       '''propfraunhofer'''
       field = self.field
       y_vec = self.y_vec
       z_vec = self.z_vec
       wavelength = self.wavelength

       u1    = 'usage: propfraunhofer, y_vec=y_vec, z_vec=z_vec, field=field,drift=drift'
       u2    = '[, plot=plot][, wavelength=wavelength]'    
       usage = u1+u2
       if drift is None:
          print("drift not given - set default drift to 10 m")
          print(usage)
          drift = 10.0

       print('------------------ propfraunhofer called ----------------------------')

       k  = 2* np.pi/wavelength
       nz = len(z_vec)
       ny = len(y_vec)
       zz = (z_vec[nz-1]- z_vec[0] ) * nz / (nz-1)                      # total width, 12.9.2013: corrected for nz/(nz-1)
       yy = (y_vec[ny-1]- y_vec[0] ) * ny / (ny-1)                      # total height,     -"-
  
       print('width (input) = ', zz*1e3, ' x ', yy*1e3, ' mm^2 ')
       print('drift         = ', drift)

       newfield0 = np.fft.fft2(modfield)                               # forward 2d fft, centered output           
       newfield  = np.fft.fftshift(newfield0)
       u0        = np.arange(nz)/(nz-1) - 0.5                           # define the vectors in the image plane     
       v0        = np.arange(ny)/(ny-1) - 0.5   
       uscale   = (drift*wavelength)/zz * nz                            # why is uscale,vscale of type array[1] ?   
       vscale   = (drift*wavelength)/yy * ny                            #-> wavelength comes as array[1], solved    
       u        = u0*uscale
       v        = v0*vscale
       z0       = z_vec[0]
       y0       = y_vec[0]
       print( ' z0 = ',z0, ' y0 = ',y0)
       print( ' nz = ',nz, ' ny = ',ny)

       scale   = np.zeros((nz, ny), dtype=complex)                                     # make a complex array
       for i in range(0, nz-1):
           for j in range(0, ny-1):
               phase = (u[i]**2 + v[j]**2) * k/(2.0*drift)             
               phase = phase + (u[i]*z0 + v[j] * y0) * k / drift        # set origin  12.9.2013: changed sign from - to +
               scale[i,j]= complex(np.cos(phase), np.sin(phase)) 

       scale1 = complex(np.cos(k*drift), np.sin(k*drift))      # why is this of type array[1] ?
       scale2 = complex(0.0         , (wavelength*drift))      # -> wavelength comes as array[1] -> k is array[1]
       self.field  = zz * yy * newfield * scale  * scale1/ scale2
       self.z_vec  = u
       self.y_vec  = v
       print( '------------------ propfraunhofer end ----------------------------')
   # end propfraunhofer

   def propfresnel(self, drift=None):
       '''propfresnel'''
       field = self.field
       y_vec = self.y_vec
       z_vec = self.z_vec
       wavelength = self.wavelength

       print('------------------ propfresnel called ----------------------------')

       

       u1    = 'usage: propfresnel, y_vec=y_vec, z_vec=z_vec, field=field,drift=drift'
       u2    = '[, plot=plot][, wavelength=wavelength]'    
       usage = u1+u2   
       if drift is None:
          print("drift not given - set default drift to 10 m")
          print(usage)
          drift = 10.0

       k  = 2* np.pi/wavelength
       nz = len(z_vec)
       ny = len(y_vec)
       zz = (z_vec[nz-1]- z_vec[0] ) * nz / (nz-1)                      # total width, 12.9.2013: corrected for nz/(nz-1)
       yy = (y_vec[ny-1]- y_vec[0] ) * ny / (ny-1)                      # total height,     -"-

       print('width (input) = ', zz*1e3, ' x ', yy*1e3, ' mm^2 ')
       print('drift         = ', drift)

#------------------------ Multipy input field with phase factor -------)
       driftarr= np.zeros((nz, ny), dtype=complex)                      # make a complex array
       for i in range(0, nz-1):
           for j in range(0, ny-1):
               phase= k*(z_vec[i]**2 + y_vec[j]**2)/(2.0*drift)         
               driftarr[i,j]= complex(np.cos(phase), np.sin(phase))
       
       modfield = field * driftarr
       #; print, '--------------- FT of Source field ------------------ exp(-i ...)'
       
       newfield0 = np.fft.fft2(modfield)                               # forward 2d fft, centered output           
       newfield  = np.fft.fftshift(newfield0)
       u0       = np.arange(nz)/(nz-1) - 0.5                           # define the vectors in the image plane     
       v0       = np.arange(ny)/(ny-1) - 0.5   
       uscale   = (drift*wavelength)/zz * nz                           # why is uscale,vscale of type array[1] ?   
       vscale   = (drift*wavelength)/yy * ny                           #-> wavelength comes as array[1], solved    
       u        = u0*uscale
       v        = v0*vscale
       z0       = z_vec[0]
       y0       = y_vec[0]

       print( ' z0 = ',z0, ' y0 = ',y0)
       print( ' nz = ',nz, ' ny = ',ny)
#-------------------- Multiply new field with phase factor ----------------

       scale   = np.zeros((nz, ny), dtype=complex)                                     # make a complex array
       for i in range(0, nz-1):
           for j in range(0, ny-1):
               phase = (u[i]**2 + v[j]**2) * k/(2.0*drift)             
               phase = phase + (u[i]*z0 + v[j] * y0) * k / drift        # set origin  12.9.2013: changed sign from - to +
               scale[i,j]= complex(np.cos(phase), np.sin(phase))   
       

       scale1 = complex(np.cos(k*drift), np.sin(k*drift))      # why is this of type array[1] ?
       scale2 = complex(0.0         , (wavelength*drift))      # -> wavelength comes as array[1] -> k is array[1]
       
       self.field  = zz * yy * newfield * scale  * scale1/ scale2
       self.z_vec  = u
       self.y_vec  = v

       print('------------------ propfresnel end ----------------------------')

       #propfresnel

   def getfield( self ):
       "returns the field"
       return self.field

   def getimag( self ):
       "returns the imaginary part of the field"
       return imag(self.field)

   def getintensity( self ):
       "returns the intensity"
       return np.absolute(self.field)**2/377.0

   def getname( self ):
       "returns the nameof the field"
       return self.name

   def getphase(self, param='raw'):
       "returns the phase of the field"
       phi= 0.0
       if param == "raw":
          print( "get raw phase") 
          phi= np.angle(self.field)
       elif param == "herra":
          print( "herra not yet implemented")
       elif param == "numpy":
          print("numpy unwrap")
          phi0= np.angle(self.field)
          phi= np.unwrap(phi0)
       return phi
  # getphase

   def getphotons(self):
      "returns the photons"
      return np.absolute(self.field)**2/377.0/1.6e-19

   def getreal(self):
      "returns the real part of the field"
      return self.field.real

   def getWavelength( self ):  
      "returns the wavelength in m"
      return self.wavelength

   def gety_vec( self ):
       "returns the y vector"
       return self.y_vec

   def getz_vec( self ):
       "returns the z vector"
       return self.z_vec

   def setField( self, field ):  
      "set a new field"
      self.field= field

   def setName( self, name ):  
      "set a new name"
      self.name= name

   def setWavelength( self, wavelength ):  
      "set a new wavelength in m"
      self.wavelength= wavelength

   def sety_vec( self, vec ):  
      "set a new y_vec"
      self.y_vec= vec

   def setz_vec( self, vec ):  
      "set a new z_vec"
      self.z_vec= vec


#    def instance_method(self,arg):
#        statements
#    @classmethod
#    deg class_method(cls, arg):
#        statements 
#    @staticmethod
#    def static_method(arg):
#       statements
#a= Myobj()  # create an instance of the object

class initphase(emf):
    def __init__(self):
        super().__init__()
        print("initphase done")

# end
