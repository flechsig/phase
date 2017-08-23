# File      : /afs/psi.ch/project/phase/GIT/phase/src/phasepython/phase.py
# Date      : <15 Aug 17 16:25:49 flechsig> 
# Time-stamp: <22 Aug 17 16:02:11 flechsig> 
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

# the class methods are in alphabetical order

import numpy as np
import matplotlib.pyplot as plt
import gausfitter2

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

   def aperture(self, example=False, shape=1, p1=None, p2=None, p3=None, p4=None, 
                plot=False, Ny=None, sizey=None, Nz=None, sizez=None, verbose=True):
      """acts as an aperture, or generates wavefield with amplitude according to 'shape'

      Args:
         example=False (bool):
         field
         example
         shape=1 (int):      shape of aperture
            shape 1  : rectangular              P1 = hsize, P2 = vsize
            shape 2  : vertical slit            P1 = hsize, P2 = hpos (default= 0)
            shape 3  : horizontal slit          P1 = vsize, P2 = vpos (default= 0)
            shape 12 : double slit vertical,    P1 = hsize, P2= hsep
            shape 13 : double slit horizontal,  P1 = vsize, P2= vsep
            shape 20 : circular                 P1 = Radius
                      Radius > 0: central circular part is transparent
                      Radius < 0: central circular part is oblique
            shape 21 : annular                  P1 = Outer radius, P2 = inner radius
            shape 32 : vertical mirror          P1 = length, P2 = grazing angle (rad)
            shape 33 : horizontalal mirror      P1 = length, P2 = grazing angle (rad)

      Returns:
         mask (float): the mask array as np array

      Example:
          >>> emf = phase.initphase()
              mask= emf.aperture()
      """
   
      usage= 'usage: aperture, [emf,][field=field, y_vec=y_vec, z_vec=z_vec,] shape=shape, [P1=P1,] [P2=P2,] '
      print('aperture called')

      self.name = 'Aperture'

      if example :
         print('**********************************************************')
         print('example: double slit ')
         print('**********************************************************')
         self.aperture(shape=12, p1=2e-3, p2=5e-3, Ny=51, sizey=2e-2, plot=True)
         print('**********************************************************')
         print('end example')
         print('**********************************************************')
         return
     
      if (Ny and sizey) or (Nz and sizez) :  # generate a field
         create= True
         print("generate a new field")
         if (Ny and sizey) and not (Nz and sizez) :
            Nz= Ny
            sizez= sizey
         if not (Ny and sizey) and (Nz and sizez) :
            Ny= Nz
            sizey= sizez
         print("Ny= {:d}, Nz= {:d}, sizey= {:.3g} m, sizez= {:.3g} m".format(Ny, Nz, sizey, sizez))
         self.z_vec = np.linspace(-0.5 * sizez, 0.5 * sizez, Nz)  
         self.y_vec = np.linspace(-0.5 * sizey, 0.5 * sizey, Ny)
         self.field = np.zeros((Ny, Nz), dtype=complex) + 1.0 + 0j   # complex array with 1.0 = 0j  
      # end generate field 1 

#;; call case twice for speed     
      shapefound=False
      if shape == 1 :         # rectangular
         shapefound=True
         if not p2 : 
            p2 = p1
         if not p3 :
            h0= 0.0 
         else : 
            h0= p3
         if not p4 :
            v0= 0.0 
         else : 
            v0= p4
         p1half= 0.5 * p1
         p2half= 0.5 * p2
         if verbose : 
            print('rectangular aperture (h x v): ', p1, p2, ' offsets: ', h0, v0)
      # end 1

      if shape == 2 :         # vertical slit
         shapefound=True
         if not p2 : 
            p2 = 0.0
         p1half= 0.5 * p1
         if verbose : 
            print('vertical slit (hwidth, hpos): ', p1, p2)
      # end 2

      if shape == 12 :         # vertical double slit
         shapefound=True
         if not p2 : 
            p2 = p1
         p1half= 0.5 * p1
         p2half= 0.5 * p2
         if verbose : 
            print('vertical double slit (hwidth, hpos): ', p1, p2)
      # end 2

      if not shapefound :
         print("error: unknown shape, shape= ", shape, " => return")
         return
      #end shape definitions

# fill
      Ny= len(self.y_vec)
      Nz= len(self.z_vec)
      T= np.zeros((Ny, Nz), dtype=float)
      for row in np.arange(Ny) :
         for col in np.arange(Nz) :
            if shape == 1 :
               if ((np.abs(self.z_vec[col]- h0) <= p1half) and (np.abs(self.y_vec[row]- v0) <= p2half)) :
                  T[row, col]= 1.0
            if shape == 2 :
               if np.abs(self.z_vec[col]- p2) <= p1half :
                  T[row, col]= 1.0
            if shape == 12 :
               if (np.abs(self.z_vec[col]) <= (p2half + p1half)) and (np.abs(self.z_vec[col]) >= (p2half - p1half)) :
                  T[row, col]= 1.0
      # 2d for loop
      
      #print("T: ", T)
      #self.mycontour(T, self.z_vec, self.y_vec)
      self.field*= T      
      return T                                                    
   """   self.wavelength= wavelength
      self.field = field
      self.z_vec = z_vec
      self.y_vec = y_vec"""
   # end aperture

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
              emf.gaussbeam(example=True, plot=True)   
       """
       self.name = 'Gaussbeam'

       if example is True :
           print('**********************************************************')
           print('example: HeNe Laser ')
           print('wavelength=633e-9, w0= 1e-3, dist= 10., sizez=1e-2')
           print('**********************************************************')
           self.gaussbeam(dist=10., wavelength=633e-9, w0=1e-3, sizez=1e-2)
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

       field = np.zeros((Ny, Nz), dtype=complex) 
       z_vec = np.linspace(-0.5*sizez, 0.5*sizez, Nz)  
       y_vec = np.linspace(-0.5*sizey, 0.5*sizey, Ny)
       dz    = z_vec[1]- z_vec[0]
       dy    = y_vec[1]- y_vec[0]

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
       itot= np.sum(intensity) * binsize * 2.0
       scale= 1.0/np.sqrt(itot)
       field*= scale

       if truncation > 0:
           print ('!! warning -- some outside points are truncated !!')

# plot using mycontour
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
       self.field = field
       self.z_vec = z_vec
       self.y_vec = y_vec
   # end gaussbeam

   def mycontour(self, z, x, y, xlabel='z (mm)', ylabel='y (mm)', zlabel='intensity (W)', title='title', figure=True): 
       """make a contour plot (helper function)

       Args:
          z (double): a 2d array
          x (double): vector in horizontal direction
          y (double): vector in vertical direction
          figure=True (bool): plot a new figure
          title='title' (string): title
          xlabel='z (mm)' (string): label
          ylabel='y (mm)' (string): label
          zlabel='intensity (W)' (string): label
       
       Example:
          >>> emf = phase.initphase()
              emf.gaussbeam(example=True, plot=False)
              ph= emf.getphase()
              emf.mycontour(ph)

       """
       if figure is True:
          plt.figure()
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

   def plotamplitude( self ):
      """plot the amplitude
      
      Example:
          >>> emf = phase.initphase()
              emf.gaussbeam(example=True)
              emf.plotamplitude()
      """
      
      title= self.getname()+ " amplitude"
      self.mycontour(self.getamplitude(), self.getz_vec() * 1e3, self.gety_vec() * 1e3, zlabel= 'amplitude ($V/m^3$)', title=title)
   # end plotamplitude 
 
   def plotimag( self ):
      """plot the imaginary part of the field

      Example:
          >>> emf = phase.initphase()
              emf.gaussbeam(example=True)
              emf.plotimag()
      """
      
      title= self.getname()+ " imaginary part"
      self.mycontour(self.getimag(), self.getz_vec() * 1e3, self.gety_vec() * 1e3, zlabel= 'imaginary part (a. u.)', title=title)
   # end plotimag

   def plotphotons( self ):
      """plot the field intensity as photons

      Example:
          >>> emf = phase.initphase()
              emf.gaussbeam(example=True)
              emf.plotphotons()
      """
      
      title= self.getname()+ " photons"
      self.mycontour(self.getphotons(), self.getz_vec() * 1e3, self.gety_vec() * 1e3, zlabel= 'photon density ($1photons/s m^2$)', title=title)
   # end plotphotons

   def plotreal( self ):
      """plot the real part of the field

      Example:
          >>> emf = phase.initphase()
              emf.gaussbeam(example=True)
              emf.plotreal()
      """
      
      title= self.getname()+ " real part"
      self.mycontour(self.getreal(), self.getz_vec() * 1e3, self.gety_vec() * 1e3, zlabel= 'real part (a. u.)', title=title)
   # end plotreal  
   
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
       """propfresnel

       Args:
       """
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
               phase = (u[i]**2 + v[j]**2) * k/(2.0 * drift)             
               phase = phase + (u[i]*z0 + v[j] * y0) * k / drift        # set origin  12.9.2013: changed sign from - to +
               scale[i,j]= complex(np.cos(phase), np.sin(phase))   
       

       scale1 = complex(np.cos(k*drift), np.sin(k*drift))      # why is this of type array[1] ?
       scale2 = complex(0.0         , (wavelength*drift))      # -> wavelength comes as array[1] -> k is array[1]
       
       self.field  = zz * yy * newfield * scale  * scale1/ scale2
       self.z_vec  = u
       self.y_vec  = v

       print('------------------ propfresnel end ----------------------------')

       #propfresnel

   def getamplitude( self ):
       """returns the amplitude of the field

       Returns:
          field (real): amplitude as numpy array
       """
       return np.abs(self.field)
   # getamplitude


   def getfield( self ):
       """returns the field

       Returns:
          field (complex): field as numpy array
       """
       return self.field
   # getfield

   def getimag( self ):
       """returns the imaginary part of the field

       Returns:
          field (double): imaginary part as numpy array
       """
       return np.imag(self.field)
   # getimag

   def getintensity( self ):
       """returns the intensity

       Returns:
          field (double): intensity in W as numpy array
       """
       return np.absolute(self.field)**2/377.0
   # getintensity

   def getname( self ):
       """returns the nameof the field

       Returns:
          name (string): the name
       """
       return self.name
   # getname

   def getphase(self, param='raw'):
       """returns the phase of the field

       Args:
          param='raw' (string): the phase style ("raw", "herra", "numpy")
             "raw": no unwrapping
             "herra": unwrapping with Herra algorithm
             "numpy": unwrapping with numpy algorithm

       Returns:
          phase (double): the phase as np array
       """
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
       else :   
          print("error: unknown algorithm: ", param)

       return phi
  # getphase

   def getphotons(self):
      """returns the photons

      Returns:
         photons (double): the number of photons
      """
      return np.absolute(self.field)**2/377.0/1.6e-19
   # getphotons

   def getreal(self):
      """returns the real part of the field

      Returns:
         field (double): the real part of the field as np array
      """
      return self.field.real
   # getreal

   def getWavelength( self ):  
      """returns the wavelength in m

      Returns:
         wavelength (double): wavelength in m
      """
      return self.wavelength
   # getWavelength

   def gety_vec( self ):
       """returns the y vector

       Returns:
          y (double): vector
       """
       return self.y_vec
   # gety_vec

   def getz_vec( self ):
       """returns the z vector

       Returns:
          z (double): vector
       """
       return self.z_vec
   # getz_vec   

   def setField( self, field ):  
      """set a new field

      Args:
         field (complex): np complex array
      """
      self.field= field
   # setField

   def setName( self, name ):  
      """set a new name

      Args:
         name (string): name
      """
      self.name= name
   # setName

   def setWavelength( self, wavelength ):  
      """set a new wavelength in m

     Arg:
        wavelength (double): wavelength in m

        Example:
          >>> emf = phase.initphase()
              emf.setWavelength(1e-10)
              emf.getWavelength()
      """
      self.wavelength= wavelength
   # end setWavelength   

   def sety_vec( self, vec ):  
      """set a new y_vec

      Arg:
         vec (double): vector as np array

      Example:
          >>> emf = phase.initphase()
              emf.sety_vec(np.arange(11) - 5.0)
              emf.gety_vec()
      """

      self.y_vec= vec
   # end sety_vec

   def setz_vec( self, vec ):  
      """set a new z_vec

      Args:
         vec (double): vector as np array

      Example:
          >>> emf = phase.initphase()
              emf.setz_vec(np.arange(11) - 5.0)
              emf.getz_vec()
      """

      self.z_vec= vec
      # end setz_vec

   def statistics(self, comment=None, amplitude=False, nofit=False):
      """shows statistics 

      Args:
         comment=None (string): comment string (optional)
         amplitude=False (bool): amplitude statistics - default: intensity
         nofit=False (bool): skip 2d fit
      
     Returns:
          stat (dictionary)

      Example:
          >>> emf = phase.initphase()
              emf.gaussbeam(example=True)
              emf.statistics()
      """

      if amplitude :
         title= "amplitude statistics"
         myfield= self.getamplitude()
         mymaxstr= 'max field       (V/m) = ' 
         mytotstr= 'total field     (V m) = '
      else :
         title= "intensity statistics"
         myfield= self.getintensity()
         mymaxstr= 'max intensity ($W/m^2$) = ' 
         mytotstr= 'total intensity (W)   = '

      if comment :
         title+= ' => ' + comment

      z_vec= self.z_vec
      y_vec= self.y_vec
      wavelength= self.wavelength
      zmin= np.amin(z_vec)
      zmax= np.amax(z_vec)
      ymin= np.amin(y_vec)
      ymax= np.amax(y_vec)
      binsize= (z_vec[1]- z_vec[0]) * (y_vec[1]- y_vec[0])

      mymax= np.amax(myfield)                        # photons/m^2
      mymaxidx= np.unravel_index(myfield.argmax(), myfield.shape)
      mysum= np.sum(myfield)
      mytot= np.sum(myfield) * binsize   # sum of all bins*binsize

      if mymax > 0.0 :
         field_n= myfield/mymax
      else :
         field_n= myfield     # normalized field for fit

 ##    stat   = dblarr(7)

      if not nofit :
         ##fit= ##gauss2dfit(field_n, stat, z_vec, y_vec) 
         stat= gausfitter2.gaussfit(field_n)
      else :
         print('we do not fit- we search fwhm')
         z0i= mymaxidx[1]
         y0i= mymaxidx[0]
         mymax05= 0.5 * mymax
#;    print('z0i,y0i,mymax05',z0i,y0i,mymax05
         zcut= myfield[:,y0i]
         ycut= myfield[z0i,:]
#;    plot, ycut
#;    print( 'zcut::::', zcut
         zidx= (zcut > mymax05).nonzero()
#;    print( 'zidx::::', zidx
# ;   help, zidx
         yidx= (ycut > mymax05).nonzero()
    
         if len(zidx) > 1 :
            zfwhm= z_vec[zidx[len(zidx)-1]]- z_vec[zidx[0]]
         else :
            zfwhm= 0
         if len(yidx) > 1 :
            yfwhm= y_vec[yidx[len(yidx)-1]]- y_vec[yidx[0]]
         else :
            yfwhm= 0


      print('==============================================================================')
      print( title )
      print('==============================================================================')
      if not nofit :
         print('z fwhm = {:7.5g} m, rms = {:.5g} m'.format(stat[2] * 2.35, stat[2]))
         print('y fwhm = {:7.5g} m, rms = {:.5g} m'.format(stat[3] * 2.35, stat[3]))
         print('z0     = {:7.5g} m'.format(stat[4]))
         print('y0     = {:7.5g} m'.format(stat[5]))
      else :
         print('z fwhm=', zfwhm, ' m')
         print( 'y fwhm=', yfwhm, ' m')
         print( 'z0    =', z_vec[z0i], ' m')
         print( 'y0    =', y_vec[y0i], ' m')

      print( 'zmin, zmax (m) =', zmin, zmax, ', nz=', len(z_vec))
      print( 'ymin, ymax (m) =', ymin, ymax, ', ny=', len(y_vec))
      print('wavelength (nm)= {:.3f}'.format(wavelength * 1e9))
      print( mymaxstr, mymax)
      print( mytotstr, mytot)
      if not amplitude:
         print('max intensity (photons/m^2) = ', mymax/(1.6e-19*1240e-9/self.wavelength))
         print('total intensity (photons)   = ', mytot/(1.6e-19*1240e-9/self.wavelength))

      print('debug: mysum, binsize=', mysum, binsize)
      print( '==============================================================================')
      if not nofit :
         print( 'result of gauss2dfit in (m):', stat)
         print( '==============================================================================')


      total=mytot
      max= mymax


   # end statistics

   def torus(self, degree=False, s1=10.0, s2=5.0, thetan=0.0, thetag=None, verbose=True):
      """calculate radii of a toroidal mirror- angle in rad or degree, grazing or normal
      
      Args:
         degree=False (bool): theta input in degree, default is rad
         s1=10.0 (double): source distance in m
         s2=5.0 (double): image distance in m 
         thetag=None (double): grazing angle
         thetan=0.0 (double): angle to normal
         verbose=True (bool): verbosity
      
      Returns:
         rw, rl
            rw (double): radius in w in m  
            rl (double: radius in l in m

      Example:
          >>> emf = phase.initphase()
              ans = emf.torus(s1=10, s2=5, thetan= 30, degree=True)
              print(ans)
      """
      if thetan and degree :
         thetan = thetan * np.pi/180. 
      if thetag and degree :
         thetan = np.pi/2.0 - thetag * np.pi/ 180.
      if thetag and not degree :
         thetan = np.pi/2.0 - thetag
      if (np.abs(thetan) >= np.pi/2.0) or (np.abs(s1) < 1e-10) or (np.abs(s2) < 1e-10) :
         print("error: unphysical input- return")
         print("usage example: ans = emf.torus(s1=10, s2=5, thetag=1e-3)")
         return
      
      rw= 1.0/((1.0/s1+ 1.0/s2) * np.cos(thetan)/2.0)
      rl= 1.0/((1.0/s1+ 1.0/s2) / (2.0 * np.cos(thetan)))
      thetag= np.pi/2.0- thetan

      if verbose :
         print('=========== torus ==============')
         print('s1    = {:10.4g} m'.format(s1))
         print('s2    = {:10.4g} m'.format(s2))
         print('thetan= {:7.4f} = {:7.4f} deg.'.format(thetan, thetan* 180/ np.pi))
         print('thetag= {:7.4f} = {:7.4f} deg.'.format(thetag, thetag* 180/ np.pi))
         print('rw    = {:10.4g} m'.format(rw))
         print('rl    = {:10.4g} m'.format(rl))
         print('M     = {:10.4g} : 1'.format(s1/s2))
         print('========== torus end ===========')

      return rw, rl   
   # end torus    

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
