# File      : /afs/psi.ch/project/phase/GIT/phase/src/phasepython/phase.py
# Date      : <15 Aug 17 16:25:49 flechsig>
# Time-stamp: <21 Dec 17 12:24:33 flechsig>
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

import subprocess
import h5py
import numpy as np
import matplotlib.pyplot as plt
import gausfitter2
import tkinter.filedialog
import scipy.interpolate

__version__='0.1'

class emf(object):
    """main object of an electromagnetic field """

    def __init__(self):
        """ constructor """
        self.name = ""
        self.wavelength = 1e-10
        self.y_vec = np.array([0.0])
        self.z_vec = np.array([0.0])
        self.field = np.array([[0.0 + 0.0j], [0.0 + 0.0j]])

    def __str__(self):
        s= "emf version: %s\n" % __version__
        if self.name != '':
            s = s+ "dataset name: \"%s\",  wavelength: %e m" % (
                self.name, self.wavelength)
        else:
            s = s+ "no data (nothing read in)"

        return s

    def aperture(self, p1=None, p2=None, p3=None, p4=None,
                 Ny=None, sizey=None, Nz=None, sizez=None,
                 plot=False, norm=True, example=False, shape=1, verbose=True):
        """acts as an aperture, or generates wavefield with amplitude 
           according to 'shape', field generation needs Ny, Nz, sizey, 
           sizez otherwise the field is kept and multiplied by the mask

        Args:
            p[1..4]=None (double): generic parameters depending on shape 
            Ny=None (int): number of points vertical for field generation
            Nz=None (int): number of points horizontal for field generation
            sizey=None (float): size vertical for for field generation
            sizez=None (float): size horizontal for for field generation
            example=False (bool): make an example
            norm=True (bool): field generation: norm output to 0.5 W
            plot=False (bool): plot the mask

            shape=1 (int):      shape of aperture
            shape 1  : rectangular              p1 = hsize, p2 = vsize
            shape 2  : vertical slit            p1 = hsize, p2 = hpos (default=0)
            shape 3  : horizontal slit          p1 = vsize, p2 = vpos (default=0)
            shape 4  : vertical slit with LCLS like cylinders   p1 = hsize, 
                       p2 = limits,  p3=Radius of cylinders.  RF 18.12.2013
            shape 12 : double slit vertical,    p1 = hsize, p2= hsep
            shape 13 : double slit horizontal,  p1 = vsize, p2= vsep
            shape 20 : circular                 p1 = Radius
                       Radius > 0: central circular part is transparent
                       Radius < 0: central circular part is oblique
            shape 21 : annular                  p1 = Outer radius, p2 = inner radius
            shape 32 : vertical mirror          p1 = length, p2 = grazing angle (rad)
            shape 33 : horizontalal mirror      p1 = length, p2 = grazing angle (rad)
            shape 40 : diamond                  P1= width, P2= hpos, P3= vpos
            shape 50 : triangle                 P1= width, P2= hpos, P3= vpos
            shape 62 : transmission grating ruled vertical,   P1= pitch, 
                       P2= duty cycle (opening/pitch), center transparent
            shape 63 : transmission grating ruled horizontal, P1= pitch, 
                       P2= duty cycle (opening/pitch), center transparent
            shape 72 : phase grating ruled vertical,   P1= pitch, 
                       P2= duty cycle (opening/pitch), P3= phase_shift (rad), center transparent
            shape 73 : phase grating ruled horizontal, P1= pitch, 
                       P2= duty cycle (opening/pitch), P3= phase_shift (rad), center transparent
            shape 82 : vertical   slit with round blades, P1 = vsize, 
                       P2 = hpos (default= 0), P3= edge radius
            shape 83 : horizontal slit with round blades, P1 = vsize, 
                       P2 = hpos (default= 0), P3= edge radius

        Returns:
            mask (float): the mask as np array

        Example:
            >>> emf = phase.initphase()
                mask= emf.aperture(example=True)
        """

        print('aperture called')
        self.name = 'Aperture'

        if example:
            print('**********************************************************')
            print('example: double slit ')
            print('T= emf.aperture(shape=12, p1=2e-3, p2=5e-3, Ny=101, sizey=2e-2, plot=True)')
            print('**********************************************************')
            T = self.aperture(shape=12, p1=2e-3, p2=5e-3, Ny=101, sizey=2e-2, plot=True)
            print('**********************************************************')
            print('end example')
            print('**********************************************************')
            return T

        createfield = False
        if (Ny and sizey) or (Nz and sizez):  # generate a field
            createfield = True
            print("generate a new field")
            if (Ny and sizey) and not (Nz and sizez) :
                Nz = Ny
                sizez = sizey
            if not (Ny and sizey) and (Nz and sizez) :
                Ny = Nz
                sizey = sizez
            print("Ny= {:d}, Nz= {:d}, sizey= {:.3g} m, sizez= {:.3g} m".format(Ny, Nz, sizey, sizez))
            self.z_vec = np.linspace(-0.5 * sizez, 0.5 * sizez, Nz)  
            self.y_vec = np.linspace(-0.5 * sizey, 0.5 * sizey, Ny)
            self.field = np.zeros((Ny, Nz), dtype=complex) + 1.0 + 0j   # complex array with 1.0 = 0j  
    # end generate field 1 

    # switch 
        if shape == 1 :
            T= self.aperture1(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose) 
        elif shape == 2 :
            T= self.aperture2(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 3 :
            T= self.aperture3(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 4 :
            T= self.aperture4(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 12 :
            T= self.aperture12(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 13 :
            T= self.aperture13(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 20 :
            T= self.aperture20(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 21 :
            T= self.aperture21(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 32 :
            T= self.aperture32(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 33 :
            T= self.aperture33(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 40 :
            T= self.aperture40(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 50 :
            T= self.aperture50(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 62 :
            T= self.aperture62(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 63 :
            T= self.aperture63(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 72 :
            T= self.aperture72(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 73 :
            T= self.aperture73(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 82 :
            T= self.aperture82(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        elif shape == 83 :
            T= self.aperture83(p1=p1, p2=p2, p3=p3, p4=p4, verbose=verbose)
        else :
            print("error- unknown shape, shape = ", shape)
            return

        # print("xxxxxxxxxxx", type(T), type(self.field))    
        self.field*= T      # apply the mask

        if createfield and norm :                   # norm to 0.5 W  !! we assume only one polarization
            print("normalize intensity to 0.5 W")
            intensity = self.getintensity()
            binsize = (self.z_vec[1] - self.z_vec[0]) * (self.y_vec[1] - self.y_vec[0])
            itot = np.sum(intensity) * binsize * 2.0
            scale = 1.0 / np.sqrt(itot)
            self.field *= scale

        if plot :
            if np.iscomplexobj(T) :
                print("complex mask: print just real part")
                self.mycontour(np.real(T), self.z_vec * 1e3, self.y_vec * 1e3, title='aperture (complex field mask)', 
                               zlabel='factor (real part)')
            else :
                self.mycontour(T, self.z_vec * 1e3, self.y_vec * 1e3, title='aperture (field mask)', zlabel='factor')

        return T                                                    
    # end aperture

    def aperture1(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture1"""

        if not p2 : 
            p2 = p1
        if not p3 :
            h0= 0.0 
        else : 
            h0= p3
        if not p4 :
            v0 = 0.0 
        else : 
            v0 = p4
        p1half = 0.5 * p1
        p2half = 0.5 * p2
        if verbose : 
            print('rectangular aperture (h x v): ', p1, p2, ' offsets: ', h0, v0)

        Ny = self.y_vec.size
        Nz = self.z_vec.size
        T = np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if ((np.abs(self.z_vec[col] - h0) <= p1half) and (np.abs(self.y_vec[row] - v0) <= p2half)) :
                    T[row, col] = 1.0
        return T
    # aperture 1

    def aperture2(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture2"""

        if not p2 : 
            p2 = 0.0
        p1half = 0.5 * p1
        if verbose : 
            print('vertical slit (hwidth, hpos): ', p1, p2)

        Ny= len(self.y_vec)
        Nz= len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if np.abs(self.z_vec[col] - p2) <= p1half :
                    T[row, col] = 1.0

        return T
    # aperture 2

    def aperture3(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture3"""

        if not p2 : 
            p2 = 0.0
        p1half= 0.5 * p1
        if verbose : 
            print('horizontal slit (vwidth, vpos): ', p1, p2)

        Ny= len(self.y_vec)
        Nz= len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if np.abs(self.y_vec[row]- p2) <= p1half :
                    T[row, col]= 1.0
        return T
    # aperture3

    def aperture4(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture4"""

        rene      = 1.39e15   # 1/m^2  Be
        mu3kev    = 39.1e2    # 1/m    Be       optical constants of Be - can be extended to other materials
        mu12p4kev = 0.4e2     # 1/m    Be

        rene      = 1.8e15    # 1/m^2  B4C above  2 keV
        mu12p4kev = 1.33e2    # 1/m    B4C 1.33@12.4 keV
        mu        = mu12p4kev         # hard for 1 A       
        p1half    = 0.5 * p1

        if not p3 :
            print('usage error- aperture  ... r3=Radius')
            return
        if not p2 :
            p2half = p1half + p3
        else :
            p2half= 0.5 * p2

        Ny = len(self.y_vec)
        Nz = len(self.z_vec)
        T = np.zeros((Ny, Nz), dtype=complex)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                pos = np.abs(self.z_vec[col])
                if pos <= p1half :
                    T[row, col] = complex(1.0, 0.0)   # inside  P1: T=1
                elif pos < p2half :
                    d = 2* np.sqrt(p3**2 - (p3- (pos - p1half))**2)                  
                    f0 = np.exp(-mu*d/2.0)                                  # absorption 
                    f2 = (-1.0) * rene * self.wavelength * d                # phase shift
                    T[row, col] = complex(f0*np.cos(f2), f0*np.sin(f2))
        return T
    # aperture 4

    def aperture12(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture12"""

        if not p2 : 
            p2 = p1
        p1half = 0.5 * p1
        p2half = 0.5 * p2
        if verbose : 
            print('vertical double slit (hwidth, hsep): ', p1, p2)

        Ny= len(self.y_vec)
        Nz= len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if (np.abs(self.z_vec[col]) <= (p2half + p1half)) and (np.abs(self.z_vec[col]) >= (p2half - p1half)) :
                    T[row, col]= 1.0
        return T
    # aperture 12

    def aperture13(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture13"""

        if not p2 : 
            p2 = p1
        p1half= 0.5 * p1
        p2half= 0.5 * p2
        if verbose : 
            print('horizontal double slit (vwidth, vsep): ', p1, p2)

        Ny= len(self.y_vec)
        Nz= len(self.z_vec)
        T = np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if (np.abs(self.y_vec[row]) <= (p2half + p1half)) and (np.abs(self.y_vec[row]) >= (p2half - p1half)) :
                    T[row, col]= 1.0
        return T
    # aperture 13

    def aperture20(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture20"""

        if verbose : 
            print('circular aperture: R= ', p1)
            if p1 > 0.0 :
                print('Radius > 0: central circular part is transparent') 
            else :
                print('Radius < 0: central circular part is oblique') 

        Ny= len(self.y_vec)
        Nz= len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                rr= self.z_vec[col]**2 + self.y_vec[row]**2 
                if (p1 > 0.0) and (rr <= p1**2) :
                    T[row, col]= 1.0
                if (p1 < 0.0) and (rr >= p1**2) :
                    T[row, col] = 1.0      
        return T
    # aperture 20

    def aperture21(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture21"""

        if verbose : 
            print('annular aperture: outer radius= ', p1, ', inner radius= ', p2)

        Ny= len(self.y_vec)
        Nz= len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                rr= self.z_vec[col]**2 + self.y_vec[row]**2 
                if (rr <= p1**2) and (rr >= p2**2) :
                    T[row, col]= 1.0
        return T
    # aperture 21

    def aperture32(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture32"""

        ap    = p1  * np.sin(p2)
        aphalf= 0.5 * ap
        if verbose : 
            print('mirror vertical (w, theta_g): ', p1, p2, ' rad, ap= ', ap)
        # vertical mirror (assuming l= infinite)   
        Ny= len(self.y_vec)
        Nz= len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if np.abs(self.y_vec[row]) <= aphalf :
                    T[row, col]= 1.0
        return T
    # aperture 32

    def aperture33(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture33"""

        ap    = p1  * np.sin(p2)
        aphalf= 0.5 * ap
        if verbose : 
            print('mirror horizontal (w, theta_g): ', p1, p2, ' rad, ap= ', ap)
        # vertical mirror (assuming l= infinite)   
        Ny= len(self.y_vec)
        Nz= len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if np.abs(self.z_vec[col]) <= aphalf :
                    T[row, col]= 1.0
        return T
    # aperture 33

    def aperture40(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture33"""

        const = p1/np.sqrt(2.0)
        if p2 is None :
            h0 = 0.0 
        else :
            h0 = p2
        if p3 is None :
            v0 = 0.0 
        else :
            v0 = p3

        print('diamond width= ', p1) 

        Ny = len(self.y_vec)
        Nz = len(self.z_vec)
        T = np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if ((self.y_vec[row] - v0 <= self.z_vec[col] - h0 + const) and 
                   (self.y_vec[row] - v0 <= (-1.0) * (self.z_vec[col] - h0) + const) and 
                   (self.y_vec[row] - v0 >= self.z_vec[col] - h0 - const) and 
                   (self.y_vec[row] - v0 >= (-1.0) * (self.z_vec[col] - h0) - const)) :
                    T[row, col] = 1.0
                    return T
    # aperture 40 

    def aperture50(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture50"""

        m = np.sqrt(3.0)
        const = p1 / 4.0 * np.sqrt(3.0)
        if p2 is None:
            h0 = 0.0 
        else:
            h0= p2
        if p3 is None:
            v0 = 0.0 
        else :
            v0 = p3

        print('triangle width= ', p1) 

        Ny = len(self.y_vec)
        Nz = len(self.z_vec)
        T = np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if (self.y_vec[row] - v0 <= m * (self.z_vec[col] - h0) + const) and \
                        (self.y_vec[row] - v0 <= (-1.0) * m * (self.z_vec[col] - h0) + const) and \
                        (self.y_vec[row] - v0 >= ((-1.0) * const)) : 
                    T[row, col] = 1.0

        return T
    # aperture 50 

    def aperture62(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture62"""

        if p2 is None :
            p2 = 0.5
        shift = p2  *p1 / 2.      # half size of the opening 
        print('vertcally ruled grating, pitch= ', p1, ' duty cycle= ', p2, ' shift = ', shift)

        Ny = len(self.y_vec)
        Nz = len(self.z_vec)
        T = np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if ((np.abs(self.z_vec[col]) + 0.5 * p2) / p1 - np.floor((np.abs(self.z_vec[col]) + 0.5 * p2) / p1)) <= p2 / 2.0 : 
                    T[row, col] = 1.0

        return T
    # aperture 62

    def aperture63(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture63"""

        if p2 is None :
            p2 = 0.5
        shift = p2 * p1 / 2.      # half size of the opening 
        print('horizontally ruled grating, pitch= ', p1, ' duty cycle= ', p2, ' shift = ', shift)

        Ny = len(self.y_vec)
        Nz = len(self.z_vec)
        T = np.zeros((Ny, Nz), dtype=float)
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                # if ((np.abs(self.z_vec[col])+ 0.5* p2)/p1- np.floor((np.abs(self.z_vec[i])+ 0.5* p2)/p1)) <= p2/2.0 : 
                if ((((self.y_vec[row]+ shift) % p1) + p1) % p1) <= p2 * p1 :
                    T[row, col] = 1.0
        return T
    # aperture 63

    def aperture72(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture72"""

        if p2 is None:
            p2 = 0.5
        if p3 is None:
            p3 = np.pi
        print('vertcally ruled phase grating, pitch= ', p1, ' duty cycle= ', p2, ' phase_shift= ', p3)

        Ny= len(self.y_vec)
        Nz= len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=complex)+ 1+ 0j
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if ((np.abs(self.z_vec[col])+ 0.5* p2)/p1- np.floor((np.abs(self.z_vec[col])+ 0.5* p2)/p1)) <= p2/2.0 :
                    T[row, col]= complex(np.cos(p3), np.sin(p3))
        return T
    # aperture 72

    def aperture73(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture73"""

        if p2 is None:
            p2 = 0.5
        if p3 is None:
            p3 = np.pi
        print('horizontally ruled phase grating, pitch= ', p1, ' duty cycle= ', p2, ' phase_shift= ', p3)

        Ny = len(self.y_vec)
        Nz = len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=complex)+ 1+ 0j
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                if ((np.abs(self.y_vec[row])+ 0.5 * p2)/p1 - np.floor((np.abs(self.y_vec[row]) + 0.5 * p2)/p1)) <= p2/2.0 :
                    T[row, col] = complex(np.cos(p3), np.sin(p3))
        return T
    # aperture 73

    def aperture82(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture82"""

        if p2 is None: 
            p2= 0.0 
        if p3 is None:
            p3= 1e-2 
        p1half= 0.5 * p1
        k= 2.0 * np.pi/self.wavelength
        print('vertical slit with round edges (vwidth, vpos, edge_radius): ', p1, p2, p3)
        print('routine not debugged- probably not correct')

        Ny= len(self.y_vec)
        Nz= len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=complex)+ 1+ 0j
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                T[row, col]= complex(np.cos(p3), np.sin(p3))

        return T
    # aperture 82

    def aperture83(self, p1=None, p2=None, p3=None, p4=None, verbose=False) :
        """helper function aperture83"""

        if p2 is None: 
            p2= 0.0 
        if p3 is None:
            p3= 1e-2 
        p1half= 0.5 * p1
        k= 2.0* np.pi/self.wavelength
        print('horizontal slit with round edges (vwidth, vpos, edge_radius): ', p1, p2, p3)
        print('routine not debugged- probably not correct')

        Ny = len(self.y_vec)
        Nz = len(self.z_vec)
        T= np.zeros((Ny, Nz), dtype=complex)+ 1+ 0j
        for row in np.arange(Ny) :
            for col in np.arange(Nz) :
                T[row, col]= complex(np.cos(p3), np.sin(p3))
        return T
    # aperture 83

    def check_sampling(self, drift, verbose=True) :
        """ check critical sampling

        Args:
           drift (double): drift distance in m

        Returns:
           ratio (double): sampling ratio

        Example:
            >>> emf.check_sampling(22.3) 
        """
        cols= self.z_vec.size
        rows= self.y_vec.size
        zwidth = self.z_vec[-1] - self.z_vec[0] 
        ywidth = self.y_vec[-1] - self.y_vec[0]
        lam_x_x = self.wavelength * float(drift)
        yratio = 1.0 / (lam_x_x * rows/ywidth**2)
        zratio = 1.0 / (lam_x_x * cols/zwidth**2) 
        ratio = 0.5 * (yratio + zratio)
        myydrift = ywidth**2 / rows / self.wavelength
        myzdrift = zwidth**2 / cols / self.wavelength
        mydrift = 0.5* (myydrift + myzdrift)
        #print( type(ratio))
        
        if verbose :
            print('check_sampling, ratio = {:.2f}'.format(ratio))
            print('critical_sampling     = {:.3g} m^2'.format(lam_x_x))
            print('act. hor  sampling    = {:.3g} m^2'.format(zwidth**2/ cols))
            print('act. vert sampling    = {:.3g} m^2'.format(ywidth**2/ rows))
            if (ratio > 1.0) :
                print('drift = {} m yields to oversampling'.format(float(drift)))
                print('we recommend a transfer function (TR) based propagator (fourier)')
            else :
                print('drift = {} m yields to undersampling '.format(float(drift)))
                print('we recommend an impulse response (IR) based propagator (fresnel, fraunhofer)')
            print('critical drift= {:.3g} m'.format(mydrift))
        return ratio
    # end check_sampling

    def crl(self, radius=5e-4, size=None, thickness=2e-5) :
        """calculate the electric field after a parabolic compound refractive
           (Be) lens, (thin lens approximation), units (m) and (rad)

        Args:
           radius=5e-4 (double): radius
           size (double): aperture
           thickness=2e-5 (double): thickness

        Returns:
            crlfactor (complex): np.array factor which has been applied to the field

        Example:
            >>> emf = phase.initphase()
              mask= emf.aperture(example=True)
              crl= emf.crl()
        """
        if not size :
            size = 2 * radius  # aperture 
        maxr = 0.5 * size                  # define a maximum radius

        print('crl start calculation')

# optical constants of Be - can be extended to other materials
        rene        = 1.39e15   # 1/m^2
        mu3kev      = 39.1e2    # 1/m
        mu12p4kev   = 0.4e2     # 1/m
        delta3kev   = 3.8e-5    
        delta12p4kev= 2.21e-6   

# interpolate mu and delta - should be improved
        kev   = 1e-3* 1240e-9/self.wavelength     # photon energy in keV
        mu    = mu3kev+ ((mu12p4kev- mu3kev)/(12.4- 3.0))       * (kev- 3.0)
        delta = delta3kev+ ((delta12p4kev- delta3kev)/(12.4- 3.0)) * (kev- 3.0)

        mu = mu12p4kev         # hard for 1 A
        if (mu < 0.0): 
            mu = 0.0     # avoid overflow
        if (delta < 0.0): 
            delta = 0.0     # avoid overflow

        print('photon energy=', kev, ', mu=', mu, ', delta=', delta, ' radius = ', radius, ' aperture=', 2.0 * maxr) 

        nz= len(self.z_vec)
        ny= len(self.y_vec)

# determine lens-propagator for the crl
        crlcomp = np.zeros((ny, nz), dtype=complex)  # make a complex array
        for col in np.arange(nz) :
            for row in np.arange(ny) :
                rr = np.sqrt(self.z_vec[col]**2 + self.y_vec[row]**2)           # the radial distance 
                if rr < maxr :                               # inside the aperture
                    f0 = np.exp(-mu*thickness/2.0)            # absorption  in the central part of the lens 
                    f1 = np.exp(-0.5*mu*rr**2/radius)         # absorption  of the curved  part of the lens, 
                    f2 = (-1.0) * rene * self.wavelength * rr**2 / radius  # phase shift in the curved  part of the lens
                else :
                    f0 = 0.0   
                    f1 = 0.0
                    f2 = 0.0

        # print,'f0=',f0,' f1=', f1, ' f2', f2
                crlcomp[row, col] = complex(np.cos(f2), np.sin(f2))
                crlcomp[row, col] *= f0 * f1      

        self.field *= crlcomp
        print('crl end')
        return crlcomp
    # end crl

    def fermidiracbeam(self, nz=243, ny=None, sizez=1e-3, sizey=None, z_off=0.0, y_off=0.0, fwhm=5e-4, slope=None, example=False):
        """generate the electromagnetic field of a fermidirac like beam

      Args:
         fwhm=5e-4 (double): fwhm
         ny=None (int): number of points in y (vertical), default is nz
         nz=243 (int): number of points in z (horizontal)
         sizey=None (double): size y (vertical) in m, default is sizez
         sizez=1e-3 (double): size z (horizontal) in m 
         slope=None (double): slope
         y_off=0.0
         z_off=0.0

      Example:
          >>> emf = phase.initphase()
              emf.fermidiracbeam(example=True)

      """
        self.name = 'fermidiracbeam'

        if example :
            print('**********************************************************')
            print('**********************************************************')
            self.fermidiracbeam(fwhm=1e-3, sizez=1e-2)
            print('**********************************************************')
            print('end example')
            print('**********************************************************')
            return

        if ny is None:
            ny= nz
        if sizey is None:
            sizey= sizez
        if slope is None:
            slope = 0.05* fwhm

        field = np.zeros((ny, nz), dtype=complex)
        z_vec = np.linspace(-0.5*sizez, 0.5*sizez, nz)  
        y_vec = np.linspace(-0.5*sizey, 0.5*sizey, ny)
        dz    = z_vec[1]- z_vec[0]
        dy    = y_vec[1]- y_vec[0]

        print('wavelength (m) = ', self.wavelength)
        print('Nz     = ', nz      , ', Ny     = ', ny)
        print('sizez (m) = ', sizez   , ', sizey (m) = ', sizey)
        print('z_off (m) = ', z_off   , ', y_off (m) = ', y_off)
        print('fwhm    (m) = ', fwhm      , ', slope  (m) = ', slope)

        for col in np.arange(nz) :
            for row in np.arange(ny) :
                r = np.sqrt((z_vec[col]-z_off)**2 + (y_vec[row]-y_off)**2)
                arg0= (r- 0.5 * fwhm)/ slope 
                arg1= 1.0/(1.0 + np.exp(arg0))
                field[row, col]= complex(arg1, 0.0)

        self.field= field
        self.z_vec= z_vec
        self.y_vec= y_vec
    # end fermidiracbeam

    def fzp(self, f=1.0, d=1e-3, y_off=0.0, z_off=0.0) :
        """calculate the electric field after a Fresnel zone plate, includes a zero order stop, 
         no material properties so far, the current version has just an amplitude map

        Args:
            d=1e-3 (double): diameter in m
            f=1.0 (double): first order focal length in m
            y_off=0.0 (double):
            z_off=0.0 (double):

        Returns:
             fzpfactor (complex): np.array factor which has been applied to the field

        Example:
            >>> emf = phase.initphase()
                emf.fzp()
        """
        drn    = f* self.wavelength/d          # outermost zone width
        n      = d/(4* drn)
        res    = 1.22* drn
        na     = 0.610* self.wavelength/ res   # spatial resolution
        dof    = self.wavelength/(2.0*na*na)   # depth of field +/-
        dlambda= self.wavelength/n             # otherwise chromatic blu
        r1     = np.sqrt(self.wavelength*(f+ self.wavelength/4.0))  # first edge

        print('** Fresnel zone plate **')
        print('========================')
        print('focal length     f (m) =', f)
        print('diameter         D (m) =', d)
        print('wavelength         (m) =', self.wavelength)
        print('outerm. zone width (m) =', drn)
        print('inner zone rad. r1 (m) =', r1)
        print('number of zones  N     =', n)
        print('spatial resolution (m) =', res)
        print('numerical aperture     =', na)
        print('DOF +/-            (m) =', dof)
        print('dlambda must be    (m) <', dlambda)
        print('our grid dz        (m) =', self.z_vec[1]- self.z_vec[0])
        print('our grid dy        (m) =', self.y_vec[1]- self.y_vec[0])
        print('========================')

        nz= len(self.z_vec)
        ny= len(self.y_vec)
        comp = np.zeros((ny, nz), dtype=complex)
        maxr= 0.5 * d
        for col in np.arange(nz) :
            for row in np.arange(ny) :
                rr= np.sqrt(self.z_vec[col]**2 + self.y_vec[row]**2)           # the radial distance
                if (rr > r1) and (rr < maxr) :
                    nr= int(2.0/self.wavelength*(np.sqrt(f*f+ rr*rr)- f))  # calc n(r) (zone edge number)
                if (nr % 2) != 0 :
                    comp[row, col]= complex(1.0, 0.0)  # amplitude
        self.field*= comp

        return comp
    # end fzp

    def gaussbeam(self, dist=None, drift=None, w0=1e-5, Nz=243, Ny=None, sizez=1e-3, sizey=None, 
                  wavelength=1e-10, plot=False, example=False, 
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

        print('wavelength (m) = ', wavelength)
        print('Nz     = ', Nz      , ', Ny     = ', Ny)
        print('sizez (m) = ', sizez   , ', sizey (m) = ', sizey)
        print('dz (m)    = ', dz      , ', dy (m)    = ', dy)
        print('z_off (m) = ', z_off   , ', y_off (m) = ', y_off)
        print('w0    (m) = ', w0      , ', dist  (m) = ', dist)

        k   = np.pi * 2    / wavelength        # wave number
        z0  = np.pi * w0**2 / wavelength       # Rayleigh Range
        w   = w0 * np.sqrt(1.0 + (dist/z0)**2)  # w(dist)
        w2  = w**2
        eta = np.arctan(dist / z0)
        Ri  = dist / (dist**2 + z0**2)         # curvature Ri  = 1/R;

        print('z0    (m) = ', z0, ' (Rayleigh Range= +/- z0)')
        print('w     (m) = ', w   , ', w2 (m^2) = ', w2)
        print('eta (rad) = ', eta , ', Ri (1/m) = ', Ri)

        truncation= 0 
        for col in np.arange(Nz):
            for row in np.arange(Ny):
                rho2 = (z_vec[col] - z_off)**2 + (y_vec[row] - y_off)**2 
                arg1 = -1 * rho2 / w2               # the intensity factor as function of aperture
                if arg1 <= -40: 
                    arg1 = -40                        # -40, but -80 is still ok
                    truncation= 1
                arg2  = 0.5 * k * rho2 * Ri + k * dist - eta                    # For notation of Siegman multiply by -1                    
                phas2 = complex(np.cos(arg2), np.sin(arg2))     
                field[row, col] = phas2 * np.exp(arg1) * w0 / w

        # norm to 0.5 W  !! we assume only one polarization 
        intensity = np.absolute(field)**2/377.0
        binsize= (z_vec[1]-z_vec[0])*(y_vec[1]-y_vec[0])
        itot= np.sum(intensity) * binsize * 2.0
        scale= 1.0/np.sqrt(itot)
        field*= scale

        if truncation > 0:
            print('!! warning -- some outside points are truncated !!')

# plot using mycontour
        if plot is True:
            print('plot')
            bamp = np.absolute(field)
            # window, 20
            # stat = dblarr(7)
            # fit   = gauss2dfit(bamp,    stat, z_vec, y_vec) 
            # fit2  = gauss2dfit(bamp^2, stat2, z_vec, y_vec) 
            # print 'gaussfit amplitude: rms_z, rms_y (m)= ', stat(2),  stat(3)
            # print 'gaussfit intensity: rms_z, rms_y (m)= ', stat2(2), stat2(3)
            # title= 'gaussbeam intensity '+  'size='+  string(stat2(2)*1e6,FORMAT="(f6.1)")+ ' x ' 
            # + string(stat2(3)*1e6, FORMAT="(f6.1)") + textoidl(' \mum^2 rms')
            self.mycontour(bamp, z_vec*1e3, y_vec*1e3, xlabel='z (mm)', ylabel='y (mm)', zlabel='intensity $(W/m^2)$', title=self.name)

            pha = np.angle(field)
            if np.amax(pha)- np.amin(pha) > 1e-10: 
                print("no yet ")
                # window,21
                # mycontour, pha, z_vec*1e3, y_vec*1e3, xtitle='z (mm)', ytitle='y (mm)', title='gaussbeam phase'
            else:
                print('phase(z,y) is zero- no phase plot')
                # device,window_state=window_list
                # if window_list[21] gt 0 then wdelete, 21
                # endelse
                # endif  plot

        self.wavelength= wavelength
        self.field = field
        self.z_vec = z_vec
        self.y_vec = y_vec
    # end gaussbeam

    def getamplitude(self):
        """returns the amplitude of the field

        Returns:
           field (real): amplitude as numpy array
        """
        return np.absolute(self.field)
    # getamplitude

    def getfield(self):
        """returns the field

        Returns:
          field (complex): field as numpy array
        """
        return self.field
    # getfield

    def getimag(self):
        """returns the imaginary part of the field

        Returns:
           field (double): imaginary part as numpy array
        """
        return np.imag(self.field)
    # getimag

    def getintensity(self):
        """returns the intensity

        Returns:
           field (double): intensity in W as numpy array
        """
        return np.absolute(self.field)**2 / 377.0
    # getintensity

    def getname(self):
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
        phi = np.angle(self.field)
        
        if param == "raw":
            print("get raw phase") 
        elif param == "herra":
            libpath = '/afs/psi.ch/project/phase/lib/'  
            libname = 'unwrap_herra'                   # without extension
            print("herra unwrapping call c code from {}.so".format(libpath+libname))
            unwrap_herra_lib = np.ctypeslib.load_library(libname, libpath)
            uwh = unwrap_herra_lib.unwrap_phase
            arg1 = np.ctypeslib.as_ctypes(phi)
            uwh(arg1, self.z_vec.size, self.y_vec.size)
            
        elif param == "numpy":
            print("numpy unwrap")
            phi = np.unwrap(phi)
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

    def getprofile(self, amplitude=False, min=False, phase=False, y0=None, z=False, z0=None):
        """get the profile (cut) at the maximum or minimum for intensity, amplitude or phase

        Args:
            amplitude=False (bool): amplitude profile (default is intensity)      
            min=False (bool):   profile at minimum (default is maximum)
            phase=False (bool): phase profile (default is intensity)
            y0=None (int): horizontal profile at y0 (so far as index)
            z=False (bool): horizontal profile  (default is vertical)
            z0=None (int): vertical profile at z0 (so far as index)

        Returns:
            np.array with profile
        """

        if amplitude:
            field = self.getamplitude()
        elif phase: 
            field = self.getphase('numpy')
        else :
            field = self.getintensity() 

        if min:
            mymin= np.amin(field)
            myminidx= np.unravel_index(field.argmin(), field.shape)
            print("search minimum: ", mymin, myminidx)
        else :
            mymax= np.amax(field)
            mymaxidx= np.unravel_index(field.argmax(), field.shape)
            print("search maximum: ", mymax, mymaxidx)

        if z: 
            prof= field[mymaxidx[0], :]
        else :
            prof= field[:, mymaxidx[1]]

        if z0:
            prof= field[:, z0]

        if y0:
            prof= field[y0, :]

        return prof
    # getprofile

    def getreal(self):
        """returns the real part of the field

        Returns:
           field (double): the real part of the field as np array
        """
        return self.field.real
    # getreal

    def getwavelength(self):  
        """returns the wavelength in m

        Returns:
            wavelength (double): wavelength in m
        """ 
        return self.wavelength
    # getwavelength

    def gety_vec(self):
        """returns the y vector

        Returns:
            y (double): vector
        """
        return self.y_vec
    # gety_vec

    def getz_vec(self):
        """returns the z vector

        Returns:
            z (double): vector
        """
        return self.z_vec
    # getz_vec  
# end g 

    def h5_check_type(self, fname, verbose=False):
        """check hdf5 structure for genesis-, phase- and pha4idl (helper function)

        Args: 
           fname (str): filename
           verbose=False (bool): verbosity

        Returns: 
           file type (int)
        """
        f = h5py.File(fname)
        myreturn = 0
        #  test phase type
        e= 'e_field' in f
        e= e and 'y_vec' in f
        e= e and 'z_vec' in f
        e= e and 't_vec' in f
        if e:
            if verbose:
                print('h5_check_type: file ', fname, ' => hdf5 file from phase (source7)') 
            myreturn = 7

        # genesis type
        e= 'slice000001' in f       
        # myreturn*= tag_exist(fstructure, 'slice000001/field') 
        e = e and 'wavelength' in f        
        e = e and 'gridsize' in f          
        e = e and 'slicecount'in f
        if e :
            if verbose:
                print('h5_check_type: file ', fname, ' => hdf5 file from GENESIS (source7)') 
            myreturn = 8

        # test phase4idl type
        e = 'data' in f      
        e = e and 'delta' in f         
        e = e and 'lambda' in f         
        e = e and 'origin' in f
        if e :
            if verbose:
                print('h5_check_type: file ', fname, ' => hdf5 file from phase4idl (source7)') 
            myreturn = 9

        if myreturn == 0 and verbose:
            print('h5_check_type: unknown type: ')

        f.close()
        return myreturn
    # end h5_check_type 

    def h5_read(self, fname=None, vertical=False, verbose=True) :
        """read genesis-, phase- and pha4idl- hdf5 files with automatic detection of the type
           opens file selection dialog if called without filename

        Args:
           fname=None (string): filename, if None open selection dialog
           verbose=False (bool): verbosity
           vertical=False (bool): read in vertical polarization
       
        Example:
            >>> emf = phase.initphase()
                emf.h5_read("myh5.h5")
        """
        if not fname :
            # fname = tkinter.filedialog.Open().show(filetypes=['*.h5'])
            # fname = tkinter.filedialog.Open().show()
            fname = tkinter.filedialog.askopenfilename(filetypes=(("hdf5 files", "*.h5"),
                                                                  ("All files", "*.*")))
            print('got fname= ', fname)
      
        if not self.h5_test(fname) :
            print('file >>', fname, '<< is not a hdf5- return')
            return
        print('file >>', fname, '<< is hdf5- return')
      
        h5type= self.h5_check_type(fname, verbose=verbose)
        if verbose :
            print('h5_read: h5type=', h5type)
        if h5type == 7 :
            self.h5_read_phase(fname, verbose=verbose)
        elif h5type == 8 :
            self.h5_read_genesis(fname, verbose=verbose)
        elif h5type == 9 :
            self.h5_read_pha4idl(fname, verbose=verbose)
        else :
            print('not a recognized hdf5 file type')
        # end h5_read

    def h5_read_genesis(self, fname, verbose=False):
        """read genesis helper function
           read genesis source, Genesis calculates an EM field on a centered,
           equidistant, quadratic grid, the output is one field - no polarization

        Args:
           fname (str): filename
           verbose=False (bool): verbosity

        example:
             >>> emf.h5_read_genesis('/afs/psi.ch/project/phase/data/SwissFEL_3keV.out.dfl.h5')
        """
        f = h5py.File(fname)
        field0 = f['slice000001/field'][:]   # 1d array
        gridsize = f['gridsize'][:]
        self.wavelength= f['wavelength'][0]
        f.close()

        size2 = field0.size / 2       # number of complex vals
        size  = int(np.sqrt(size2))
      
        if verbose:
            print('read GENESIS file -- units: (m)')
            print('size       = ', size, ' gridsize= ', gridsize)
            print('size^2     = ', size2)
            print('wavelength = {}'.format(self.wavelength))

        # normalization
        eev = 511000   # electronen ruhemasse in eV
        k = 2.0 * np.pi / self.wavelength
        field1 = field0.view(complex) 
        field2 = np.resize(field1, (size, size))
        self.field = field2 * eev / k  # scaled field
        x0 = np.arange(size) - size/2
        self.z_vec = x0 * gridsize[0]
        self.y_vec = self.z_vec * 1.0
    # end h5_read_genesis

    def h5_read_phase(self, fname, vertical=False, verbose=False):
        """read phase helper function

        Args:
           fname (str): filename
           vertical=False (bool): vertical polarization
           verbose=False (bool): verbosity

        example:
           >>> emf.h5_read_phase('/afs/psi.ch/project/phase/data/EZRE_GB_5000.h5')
        """

        f = h5py.File(fname)
        self.z_vec = f['/z_vec'][:]
        self.y_vec = f['/y_vec'][:]
        self.t_vec = f['/t_vec'][:]
        e_field = f['/e_field'][:] 
        self.wavelength = f['/wavelength'][:]
        f.close()
      
        cols = self.z_vec.size
        rows = self.y_vec.size
        no_time_slices = self.t_vec.size
        # print("untested")
        if verbose:
            print('read Phase file -- units: (m)')
            print('cols       = ', cols)
            print('rows       = ', rows)
            print('time slices= ', no_time_slices)
            print('wavelength = ', self.wavelength)
            print('field_size = ', e_field.size)

        ycomp = np.zeros((rows, cols), dtype=complex)         
        zcomp = np.zeros((rows, cols), dtype=complex)
    # print("field shape: ", e_field.shape)

    # i+ j* cols + k * (rows * cols) + it * (rows * cols * 4)
        for it in np.arange(no_time_slices):
            for row in np.arange(rows):
                for col in np.arange(cols):
                    ycomp[row, col] = complex(e_field[it, 0, row, col], e_field[it, 1, row, col])
                    zcomp[row, col] = complex(e_field[it, 2, row, col], e_field[it, 3, row, col])

        if vertical:
            print("field represents vertical polarization")
            self.field = ycomp
        else: 
            print("field represents horizontal polarization")
            self.field = zcomp
    # end h5_read_phase

    def h5_read_pha4idl(self, fname, vertical=False, verbose=False):
        """read pha4idl helper function

        Args:
            fname (str): filename
            verbose=False (bool): verbosity
            vertical=False (bool): vertical polarization

        example:
            >>> emf.h5_read_pha4idl('/afs/psi.ch/project/phase/data/uf-gauss.h5')
        """
        f = h5py.File(fname)
        self.wavelength= f['lambda'][:]
        origin = f['origin'][:]  # read origin vector (y0, z0)
        delta  = f['delta'][:]   # read the delta vector (dy, dz)
        ezre   = f['data/ezre'][:]  # field
        eyre   = f['data/eyre'][:]  # field
        ezim   = f['data/ezim'][:]  # field
        eyim   = f['data/eyim'][:]  # field
        ny, nz= ezre.shape
        print("ny= {}, nz= {}".format(ny, nz))

        self.z_vec = origin[0]+ np.arange(nz) * delta[0]
        self.z_vec = origin[1]+ np.arange(ny) * delta[1]
        if vertical:
            print("field represents vertical polarization")
            self.field = ycomp
        else: 
            print("field represents horizontal polarization")
            self.field = zcomp
        f.close()
    # end h5_read_pha4idl

    def h5_test(self, fname=None, verbose=False) :
        """test if hdf5 file

        Args:
            fname=None (string): filename, if None take a default
            verbose=False (bool): verbosity

        Returns:
            result (bool): True if hdf5

        """
        if not fname:
            fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.phase_hdf5.h5'

        sp = subprocess.run(['file', '-bL', fname], stdout=subprocess.PIPE)
        h5 = False
        if sp.stdout == b'Hierarchical Data Format (version 5) data\n' :
            h5 = True
        return h5
    # end h5_test

    def h5_write(self, fname, delta=np.pi/6, genesis=False, phase=False, pha4idl=False, verbose=False, yscale=0.9):
        """write hdf5 output - default is GENESIS format with multiple format switches

        Args:
            fname (str): filename
            delta=np.pi/6 (flt): phase shift between Ez and Ey (phase format only), default= pi/6
            genesis=False (bool): Genesis format (default if not type is given)
            phase=False (bool):  Phase format
            pha4idl=False (bool): pha4idl format
            verbose=True (bool): verbosity
            yscale=0.9 (flt): scale of y amplitude (default= 0.9)

        Example:
            >>> h5_write("myoutput.h5")   
        """
        genesis= not phase and not pha4idl
        if genesis:
            print('save genesis h5')
            if self.y_vec.size != self.z_vec.size :
                print("GENESIS hdf5 files need a quadratic grid- return")
                return
            eev = 511000
            kk = 2.0 * np.pi / self.wavelength
            field = self.field * kk / eev
            field1 = np.resize(field, (field.size, 1))
            field2 = field1.view(float)
            gridsize = self.y_vec[1] - self.y_vec[0]
            slicecount =1
            f = h5py.File(fname, 'w')
            f.create_dataset('wavelength', data=self.wavelength)
            f.create_dataset('gridsize',   data=np.array([gridsize]))
            f.create_dataset('slicecount', data=np.array([slicecount]))
            f.create_dataset('slice000001/field', data=field2)
            f.close()
        if phase:
            print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
            print('!! we save the field to zcomp AND ycomp !!')
            print('!! use parameters delta and yscale      !!')
            print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
            cols = self.z_vec.size
            rows = self.y_vec.size
            no_time_slices = 1
            field = np.zeros((no_time_slices, 4, rows, cols), dtype=float)
            for it in np.arange(no_time_slices):
                for row in np.arange(rows):
                    for col in np.arange(cols):
                        eabs = np.absolute(self.field[row, col]) * yscale
                        angl = np.angle(self.field[row, col]) + delta
                        field[it, 0, row, col] = eabs * np.cos(angl)
                        field[it, 1, row, col] = eabs * np.sin(angl)
                        field[it, 2, row, col] = np.real(self.field[row, col])
                        field[it, 3, row, col] = np.imag(self.field[row, col])

            f = h5py.File(fname, 'w')
            f.create_dataset('/wavelength', data=self.wavelength)
            f.create_dataset('/z_vec',      data=self.z_vec)
            f.create_dataset('/y_vec',      data=self.y_vec)
            f.create_dataset('/t_vec',      data=np.array([1]))
            f.create_dataset('/e_field',    data=field)
            f.close()
        if pha4idl:
            print('save pha4idl h5 - not available so far')
    # end h5_write

    def lens(self, fy=1e200, fz=1e200): 
        """field after a thin lens

        Args:
            fy=1e200 (double): vertical focal length
            fz=1e200 (double): horizontal focal length

        Returns:
            complex array with lens factor
        """
        print("thin lens with focal length fy= {:.3g} m and fz= {:.3g} m".format(fy, fz))
        lcomp = self.field * (0 + 0j)
        for row in np.arange(self.y_vec.size):
            for col in np.arange(self.z_vec.size):
                f1 = self.z_vec[col]**2 / (2 * fz) + self.y_vec[row]**2 / (2 * fy)
                f1 *= (-2* np.pi)/ self.wavelength
                lcomp[row, col]= complex(np.cos(f1), np.sin(f1))

        self.field*= lcomp
        return lcomp
    # end lens
 
    def mirror(self, u=None, rl=0.0, rw=0.0, thetag=np.pi/2.0, azimut=0, w=None, l=None):  
        """calculate the electric field after a "thin" toroidal mirror with/without 1d or 2d height errors

        Args:
          azimut=0 (int): azimut angle or Rx in rad, math. positive, 0 means vertical deflecting
          l=None (flt):     : the mirror coordinate
          rl=0 (flt):    : short radius
          rw=0 (flt):    : long radius
          thetag=pi/2.0 (flt): grazing angle in rad  
          u=None (flt array): the height error of the mirror as a vector of the mirror coordinate w or matrix u(w,l) 
          w=None (flt):     : the mirror coordinate

        Example:
          >>> emf.mirror(thetag=thetag, rw=rw, rl=rl, u=u2, w=w, l=l)    
        """
        if (np.abs(rw)- 1e-200) < 0 :
            rw= 1e200
        if (np.abs(rl)- 1e-200) < 0 :
            rl= 1e200
        if (np.abs(thetag)- 1e-9) < 0 :
            thetag= 1e-9  

        fl = rl / (2.0 * np.sin(thetag))        # brennweiten
        fw = rw * np.sin(thetag) / 2.0

        print('mirror with radius rw = {} m, rl = {} m'.format(rw, rl))
        print('mirror with focus  fw = {} m, fl = {} m'.format(fw, fl))

        myz_vec = self.z_vec
        myy_vec = self.y_vec
        nz = myz_vec.size
        ny= myy_vec.size

        lcomp = np.zeros((ny, nz), dtype=complex)   # make a complex array

        if azimut is 0 :
            print('vertical up deflecting mirror')
        elif azimut is 90 : 
            print('horizontal left deflecting mirror')
            ff = fw
            fw = fl
            fl = ff
        elif azimut is 180 :
            print('vertical down deflecting mirror')
        elif azimut is 270 :
            print('horzontal right deflecting mirror')
            ff = fw
            fw = fl
            fl = ff
        else :
            print('no valid azimut= {}, allowed are: 0 90 180 270'.format(azimut))
            return

        for col in np.arange(nz):
            for row in np.arange(ny):
                f1 = myz_vec[col]**2 / (2.0 * fl) + myy_vec[row]**2 / (2.0 * fw)  # phase   
                f1*= (-2) * np.pi/ self.wavelength                        # k
                lcomp[row, col]= complex(np.cos(f1), np.sin(f1))

        self.field *= lcomp 

        # deal with error
        if u :
            print('mirror with height error')
            if w is None:
                print('we need w as the mirror coordinate - return')
                return
            if (u.ndim == 2) and (l is None):
                print('u.ndim == 2: we need l as the mirror coordinate - return')
                return
            if (u.ndim == 1) :
                print('deal with 1d height error')
                hw1= u * np.sin(thetag)  # the phase shift relevant height as function of w
                w1 = w * np.sin(thetag)  # the projection of the mirror to normal incidence
                f= scipy.interpolate.interp1d(w1, hw1)
                hw2= f(myy_vec)  # hw2 as function of myy_vec
                for col in np.arange(nz):
                    for row in np.arange(ny):
                        f1 = hw2[row]    
                        f1 *= (-4) * np.pi / self.wavelength
                        lcomp[row, col] = complex(np.cos(f1), np.sin(f1))
            else :
                print('deal with 2d height error - not yet debugged')
                hw1= u* np.sin(thetag)  # the phase shift relevant height as function of w
                w1 = w* np.sin(thetag)  # the projection of the mirror to normal incidence
                f= scipy.interpolate.interp2d(l, w1, hw1, kind='cubic')
                hw2= f(myy_vec, myz_vec)
                for col in np.arange(nz):
                    for row in np.arange(ny):
                        f1= hw2[row, col]    
                        f1*= (-4)* np.pi/ self.wavelength
                        lcomp[row, col] = complex(np.cos(f1), np.sin(f1))
            self.field *= lcomp
    # end mirror

    def mirrorg(self, u=None, thetag=np.pi/2.0, azimut=0, w=None, l=None):  
        """calculate the electric field after a generic mirror defined by a height profile u(w,l)

        Args:
          azimut=0 (int): azimut angle or Rx in rad, math. positive, 0 means vertical deflecting
          l=None (flt):     : the mirror coordinate
          thetag=pi/2.0 (flt): grazing angle in rad  
          u=None (flt array): the height error of the mirror as a vector of the mirror coordinate w or matrix u(w,l) 
          w=None (flt):     : the mirror coordinate

        Example:
            >>> emf.mirrorg(thetag=thetag, u=u2, w=w, l=l)    
        """
      
        if (np.abs(thetag)- 1e-9) < 0 :
            thetag = 1e-9  

        myz_vec = self.z_vec
        myy_vec = self.y_vec
        nz = myz_vec.size
        ny = myy_vec.size

        lcomp = np.zeros((ny, nz), dtype=complex)   # make a complex array

        if azimut is 0 :
            print('vertical up deflecting mirror')
            wm = myy_vec/ np.sin(thetag)
            lm = myz_vec
        elif azimut is 90 : 
            print('horizontal left deflecting mirror')
            wm = (-1.0) * myz_vec /np.sin(thetag)
            lm = myy_vec
        elif azimut is 180 :
            print('vertical down deflecting mirror')
            wm= (-1.0) * myy_vec/ np.sin(thetag)
            lm= (-1.0) * myz_vec
        elif azimut is 270 :
            print('horzontal right deflecting mirror')
            wm= myz_vec /np.sin(thetag)
            lm= (-1.0) * myy_vec
        else :
            print('no valid azimut= {}, allowed are: 0 90 180 270'.format(azimut))
            return

# we calculated the coordinate wm, lm on the mirror as function of y_vec, z_vec, azimut and thetag 
# now we calculate the 2d height matrix um
        f= scipy.interpolate.interp2d(l, w, u, kind='cubic')
        um = f(lm, wm)
        for col in np.arange(nz):
            for row in np.arange(ny):
                f1 = um[row, col] * np.sin(thetag)  # phase   
                f1*= (-2)* np.pi/ self.wavelength                        # k
                lcomp[row, col]= complex(np.cos(f1), np.sin(f1))

        self.field*= lcomp 
    # end mirrorg

    def mirrorp(self,  thetag=np.pi/2.0, azimut=0):  
        """calculate the electric field after a "thin" flat mirror defined as phase shifter, 
        this simple approach does only work in limitd cases where the phase shift is only 
        a few k (long wavelength and/or small angle), it must fail when the relative phase
        shift between points approaches 2 pi

        Args:
            azimut=0 (int): azimut angle or Rx in rad, math. positive, 0 means vertical deflecting
            thetag=pi/2.0 (flt): grazing angle in rad  
          
        Example:
            >>> emf.mirrorp(thetag=thetag)    
        """
      
        if (np.abs(thetag)- 1e-9) < 0 :
            thetag= 1e-9  

        myz_vec = self.z_vec
        myy_vec = self.y_vec
        nz= myz_vec.size
        ny = myy_vec.size

        lcomp = np.zeros((ny, nz), dtype=complex)   # make a complex array

        if azimut:
            print('azimut not yet implemented - return')
            return

        k = 2.0* np.pi/ self.wavelength
        for col in np.arange(nz):
            for row in np.arange(ny):
                f1 = -2.0* k * thetag * myy_vec[row]
                lcomp[row, col]= complex(np.cos(f1), np.sin(f1))

        self.field*= lcomp 
    # end mirrorp

    def mirrort(self,  thetag=np.pi/2.0, azimut=0):  
        """calculate the electric field after a "thin" flat mirror defined as phase shifter, 
        the method uses the transfer function approach first fft to angular spectrum, 
        second apply angular shift, third fft back to spatial distribution. The granularity 
        of the shift are frequency steps

        Args:
            azimut=0 (int): azimut angle or Rx in rad, math. positive, 0 means vertical deflecting
            thetag=pi/2.0 (flt): grazing angle in rad  
          
        Example:
            >>> emf.mirrort(thetag=thetag)    
        """
        if (np.abs(thetag)- 1e-9) < 0 :
            thetag= 1e-9  

        myz_vec = self.z_vec
        myy_vec = self.y_vec
        nz = myz_vec.size
        ny = myy_vec.size
        lcomp = np.zeros((ny, nz), dtype=complex)   # make a complex array

        if azimut:
            print('azimut not yet implemented - return')
            return

        k = 2.0* np.pi/ self.wavelength
        twopi= 2.0 * np.pi
        zz  = myz_vec[-1]- myz_vec[0]      # total width
        yy  = myy_vec[-1]- myy_vec[0]      # total width
        u = (np.arange(nz)/(nz-1) - 0.5)   # runs from -0.5..0.. 0.5 
        v = (np.arange(ny)/(ny-1) - 0.5)   # for even and odd values of Ny, Nz 
        u = u * (nz-1)/zz                  # ok with odd number of elements
        v = v * (ny-1)/yy

        fieldfft = np.fft.fftshift(np.fft.fft2(self.field))  # remember: the frequencies are the 
        # direction cosines divided by lambda
        fy= np.sin(2.*thetag)/ self.wavelength
        szi= 0
        # idx= max(where(u lt fy))
        center_idx = Ny/2
# syi has to be determined from thetag- how???
        syi= idx-center_idx  #
        # sarr= shift(e0ft,szi,syi)

        self.field= np.fft.ifft2(fieldfft)

    # end mirrort

    def mycontour(self, z, x=None, y=None, xlabel='z (mm)', ylabel='y (mm)', zlabel='intensity (W)', 
                  title='title', figure=True, cmap='rainbow', plot_contours=False): 
        """mycontour(z, x, y) - make a 2d contour plot z(x, y) (helper function)

        Args:
            z (double): a 2d array
            x=None (double): vector in horizontal direction
            y=None (double): vector in vertical direction
            cmap='rainbow' (str): colormap {'rainbow', 'gray'}
            figure=True (bool): plot a new figure
            plot_contours=False (bool): plot contour lines with labels
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
        if figure :
            plt.figure()

        if (x is None) or (y is None): 
            x = np.arange(np.shape(z)[1])
            y = np.arange(np.shape(z)[0])
        if plot_contours :
            cp= plt.contour(x, y, z, 15, linewidths=0.5, colors='k')  # contour linien
            plt.clabel(cp, inline=True, fontsize=10)
        else :
            plt.axis([np.min(x), np.max(x), np.min(y), np.max(y)])

        plt.pcolormesh(x, y, z, cmap=plt.get_cmap(cmap))       # farbe
        cbar= plt.colorbar() 
        cbar.set_label(zlabel)  # rotation=270)
        plt.xlabel(xlabel)
        plt.ylabel(ylabel)
        plt.title(title)
        plt.show()
    # mycontour

    def phaseplate(self, arr): 
        """ multiply the field by a matrix - same as scalefield but with error handling

        Args:
           arr (float or complex): matrix

        """
        if self.field.shape == arr.shape :
            self.field *= arr
        else:
            print("error: shape mismatch- do nothing")
    # end phaseplate

    def plotamplitude(self):
        """plot the amplitude
      
        Example:
            >>> emf = phase.initphase()
                emf.gaussbeam(example=True)
              emf.plotamplitude()
        """
      
        title = self.getname() + " amplitude"
        self.mycontour(self.getamplitude(), self.getz_vec() * 1e3, self.gety_vec() * 1e3, 
                       zlabel='amplitude ($V/m^3$)', title=title)
    # end plotamplitude 
 
    def plotimag(self):
        """plot the imaginary part of the field

        Example:
            >>> emf = phase.initphase()
                emf.gaussbeam(example=True)
                emf.plotimag()
        """
        title= self.getname()+ " imaginary part"
        self.mycontour(self.getimag(), self.getz_vec() * 1e3, self.gety_vec() * 1e3, 
                       zlabel='imaginary part (a. u.)', title=title)
    # end plotimag

    def plotintensity(self, figure=True):
        """plot the intensity of the field

        Args: 
            figure=True (bool): new figure- goes to mycontour

        Example:
            >>> emf = phase.initphase()
               emf.gaussbeam(example=True)
               emf.plotintensity()
        """
        title= self.getname()+ " intensity"
        self.mycontour(self.getintensity(), self.getz_vec() * 1e3, self.gety_vec() * 1e3, 
                       figure=figure, zlabel='intensity ($W/m^2$)', title=title)
    # end plotintensity

    def plotphase(self, figure=True, param='raw'):
        """plot the phase of the field

        Args:
            figure=True (bool): print new figure - goes to mycontour
            param='raw' (string): the phase style ("raw", "herra", "numpy"), handed over to getphase
                
        Example:
            >>> emf = phase.initphase()
                emf.gaussbeam(example=True)
                emf.plotphase()
        """
        title = self.getname() + " phase"
        self.mycontour(self.getphase(param=param), self.getz_vec() * 1e3, self.gety_vec() * 1e3, 
                       figure=figure, zlabel='phase (rad)', title=title)
    # end plotphase

    def plotphotons(self):
        """plot the field intensity as photons

        Example:
            >>> emf = phase.initphase()
                emf.gaussbeam(example=True)
                emf.plotphotons()
        """
        title= self.getname()+ " photons"
        self.mycontour(self.getphotons(), self.getz_vec() * 1e3, self.gety_vec() * 1e3, 
                       zlabel='photon density ($1photons/s m^2$)', title=title)
    # end plotphotons

    def plotprofile(self, amplitude=False, merge=False, min=False, phase=False, y0=None, ylog=False, z0=None):
        """plot the profiles (hor and vertical) see getprofile, UF nice 2have projections

        Args:
         amplitude=False (bool): amplitude profile (default is intensity) 
         merge=False (bool): single plot for hor and vert profile    
         min=False (bool):   profile at minimum (default is maximum)
         phase=False (bool): phase profile (default is intensity)
         y0=None (int): horizontal profile at y0 (so far as index)
         ylog=False (bool): vertical log scale
         z0=None (int): vertical profile at z0 (so far as index)

        Example:
          >>> emf = phase.initphase()
              emf.gaussbeam(example=True)
              emf.plotprofile()
        """
      
        title= self.getname()+ " profile"
        zp= self.getprofile(amplitude=amplitude, min=min, phase=phase, y0=y0, z=True, z0=z0)
        yp= self.getprofile(amplitude=amplitude, min=min, phase=phase, y0=y0, z=False, z0=z0)
        y= self.gety_vec()
        z= self.getz_vec()

        plt.figure()
        if merge:
            miyz = np.amin([np.amin(y), np.amin(z)])
            mayz = np.amin([np.amax(y), np.amax(z)])
            miyzp= np.amin([np.amin(yp), np.amin(zp)])
            mayzp= np.amin([np.amax(yp), np.amax(zp)])
         
            plt.plot(z*1e3, zp, label='horizontal')
            plt.plot(y*1e3, yp, label='vertical')
            plt.xlabel('yz axis (mm)')
            plt.ylabel('profile ()')
            plt.title(title)
            plt.xlim([miyz*1e3, mayz*1e3])
            plt.ylim([miyzp, mayzp])
            plt.legend()
            if ylog:
                plt.semilogy()
        else :
            plt.subplot(1, 2, 1)
            plt.plot(z*1e3, zp, label='horizontal')
            plt.xlabel('z (mm)')
            plt.ylabel('profile')
            plt.title(title)
            if ylog:
                plt.semilogy()
            plt.subplot(1, 2, 2)
            plt.plot(yp, y * 1e3, label='vertical')
            plt.ylabel('y (mm)')
            plt.xlabel('profile')
            plt.title(title)
            if ylog:
                plt.semilogx()

        plt.show
    # end plotprofile

    def plotreal(self):
        """plot the real part of the field

        Example:
            >>> emf = phase.initphase()
              emf.gaussbeam(example=True)
              emf.plotreal()
        """
      
        title = self.getname() + " real part"
        self.mycontour(self.getreal(), self.getz_vec() * 1e3, self.gety_vec() * 1e3, 
                       zlabel='real part (a. u.)', title=title)
    # end plotreal  
   
    def propagate(self, drift=None):
        """propagate with fresnel or fourier (FFT(-1) -> FFT(1)) depending on sampling

        Args: 
         drift=None (flt): drift in m

        Example:
          >>> emf.propagate(100)
        """ 
        ratio= self.check_sampling(drift=drift)
        if ratio > 1.0:
            self.propfourier(drift)
        else:
            self.propfresnel(drift)
    # end propagate  

    def propfraunhofer(self, drift=None):
        '''propagate using Fraunhofer propagator

        Args: 
           drift=None (flt): drift in m

        Example:
          >>> emf.propfraunhofer(100)
        '''
        field = self.field
        y_vec = self.y_vec
        z_vec = self.z_vec
        wavelength = self.wavelength

        if drift is None:
            print("drift not given - set default drift to 10 m")
            drift = 10.0

        print('------------------ propfraunhofer called ----------------------------')

        k  = 2 * np.pi/self.wavelength
        nz = z_vec.size
        ny = y_vec.size
        zz = (z_vec[-1]- z_vec[0]) * nz / (nz - 1)            # total width, 12.9.2013: corrected for nz/(nz-1)
        yy = (y_vec[-1]- y_vec[0]) * ny / (ny - 1)            # total height,     -"-
  
        print('size (input), z = {:.3f} mm, y = {:.3f} mm '.format(zz * 1e3, yy * 1e3))
        print('drift           = {} m'.format(drift))

        newfield = np.fft.fftshift(np.fft.fft2(field))             # forward 2d fft, centered output
        u0       = np.arange(nz)/(nz-1) - 0.5                      # define the vectors in the image plane     
        v0       = np.arange(ny)/(ny-1) - 0.5   
        uscale   = (drift * self.wavelength) / zz * nz            # why is uscale,vscale of type array[1] ?   
        vscale   = (drift * self.wavelength) / yy * ny            # -> wavelength comes as array[1], solved    
        u        = u0 * uscale
        v        = v0 * vscale
        z0       = z_vec[0]
        y0       = y_vec[0]
        print(' z0 = {:.3f} mm, y0 = {:.3f} mm'.format(z0*1e3, y0*1e3))
        print(' nz = {}, ny = {}'.format(nz, ny))

        scale = np.zeros((nz, ny), dtype=complex)                                     # make a complex array
        for col in np.arange(nz):
            for row in np.arange(ny):
                phase = (u[col]**2 + v[row]**2) * k/(2.0 * drift)             
                phase = phase + (u[col] * z0 + v[row] * y0) * k / drift      # set origin  12.9.2013: changed sign from - to +
                scale[row, col]= complex(np.cos(phase), np.sin(phase)) 
                
        scale1 = complex(np.cos(k * drift), np.sin(k * drift))      # why is this of type array[1] ?
        scale2 = complex(0.0, (wavelength * drift))                 # -> wavelength comes as array[1] -> k is array[1]
        scale3 = newfield.size                                      # python fft is not normalized
        self.field = zz * yy * newfield * scale * scale1 / (scale2 * scale3)
        self.z_vec = u
        self.y_vec = v
        print('------------------ propfraunhofer end ----------------------------')
    # end propfraunhofer

    def propfresnel(self, drift=None):
        """fresnel propagator

        Args:
           drift=None (flt): the drift distance

        Example:
            >>> emf.propfresnel(100)
        """
        field = self.field
        y_vec = self.y_vec
        z_vec = self.z_vec
        wavelength = self.wavelength

        print('------------------ propfresnel called ----------------------------')

        if drift is None:
            print("drift not given - set default drift to 10 m")
            drift = 10.0

        k  = 2* np.pi / self.wavelength
        nz = z_vec.size
        ny = y_vec.size
        zz = (z_vec[-1]- z_vec[0]) * nz / (nz-1)                      # total width, 12.9.2013: corrected for nz/(nz-1)
        yy = (y_vec[-1]- y_vec[0]) * ny / (ny-1)                      # total height,     -"-

        print('width (input) = ', zz*1e3, ' x ', yy*1e3, ' mm^2 ')
        print('drift         = ', drift)

# ------------------------ Multipy input field with phase factor -------)
        driftarr= np.zeros((ny, nz), dtype=complex)                      # make a complex array
        for col in np.arange(nz):
            for row in np.arange(ny):
                phase = k*(z_vec[col]**2 + y_vec[row]**2) / (2.0 * drift)         
                driftarr[row, col] = complex(np.cos(phase), np.sin(phase))
                
        modfield = field * driftarr
        # print, '--------------- FT of Source field ------------------ exp(-i ...)'
       
        newfield = np.fft.fftshift(np.fft.fft2(modfield))               # forward 2d fft, centered output           
        u0       = np.arange(nz)/(nz-1) - 0.5                           # define the vectors in the image plane     
        v0       = np.arange(ny)/(ny-1) - 0.5   
        uscale   = (drift * wavelength) / zz * nz                       # why is uscale,vscale of type array[1] ?   
        vscale   = (drift * wavelength) / yy * ny                       # -> wavelength comes as array[1], solved    
        u        = u0 * uscale
        v        = v0 * vscale
        z0       = z_vec[0]
        y0       = y_vec[0]

        print(' z0 = ', z0, ' y0 = ', y0)
        print(' nz = ', nz, ' ny = ', ny)
# -------------------- Multiply new field with phase factor ----------------

        scale = np.zeros((ny, nz), dtype=complex)                                     # make a complex array
        for col in np.arange(nz):
            for row in np.arange(ny):
                phase = (u[col]**2 + v[row]**2) * k / (2.0 * drift)             
                phase = phase + (u[col]* z0 + v[row] * y0) * k / drift        # set origin  12.9.2013: changed sign from - to +
                scale[row, col]= complex(np.cos(phase), np.sin(phase))   
                
        scale1 = complex(np.cos(k * drift), np.sin(k * drift))      # why is this of type array[1] ?
        scale2 = complex(0.0           , (wavelength * drift))      # -> wavelength comes as array[1] -> k is array[1]
        scale3 = newfield.size                                      # python fft is not normalized
        self.field = zz * yy * newfield * scale * scale1 / (scale2 * scale3)
        self.z_vec = u
        self.y_vec = v

        print('------------------ propfresnel end ----------------------------')
    # propfresnel

    def propfourier(self, drift=None):
        """fourier (transfer function) propagator

        Args:
            drift=None (flt): the drift distance

        ToDo:  
            tested UF 1709 - OK           

        Example:
            >>> emf.propfourier(100)    
        """
        field = self.field
        y_vec = self.y_vec
        z_vec = self.z_vec
        wavelength = self.wavelength

        if drift is None:
            print("drift not given - set default drift to 10 m")
            drift = 10.0
        print('------------ propfourier called drift= {:.3g} m------------'.format(drift))

        k = 2* np.pi/self.wavelength
        nz = z_vec.size
        ny = y_vec.size
        zz = (z_vec[-1]- z_vec[0])
        yy = (y_vec[-1]- y_vec[0])

        print('width = {:.3g} mm, height = {:.3g} mm'.format(zz * 1e3, yy * 1e3))
        u = np.linspace(-0.5, 0.5, nz)               # runs from -0.5..0.. 0.5 
        v = np.linspace(-0.5, 0.5, ny)               # for even and odd values of Ny, Nz 
        u = u * (nz-1)/zz                            # ok with odd number of elements
        v = v * (ny-1)/yy
        print('u = {:.3g} ... {:.3g}'.format(u[0], u[-1]))
        print('--------------- FT of Source field ------------------ exp(-i ...)')
      
        fieldfft = np.fft.fftshift(np.fft.fft2(field))
        print('--------------- Propagator for free space ------------------------')
        phase      = np.zeros((ny, nz))
        propagator = np.zeros((ny, nz), dtype=complex) 
        p0         = drift % wavelength

        for col in np.arange(nz):
            for row in np.arange(ny):
                arg = 1.0 - (u[col] * wavelength)**2 - (v[row] * wavelength)**2
                if arg > 0 :
                    arg = np.sqrt(arg)
                    # numerically more accurate than k * drift* arg
                    phase[row, col] = ((drift * (arg - 1.0)) % wavelength) * k + p0 * k 
                    propagator[row, col] = complex(np.cos(phase[row, col]), np.sin(phase[row, col]))
                else :
                    print('sqrt of neg. argument, evanescent wave, arg= {}, row= {}, col= {}'. format(arg, row, col))
                    arg = np.sqrt(-1.0 * arg)
                    phase[row, col] = -1.0 * k * drift* arg
                    if phase[row, col] < -40 :
                        phase[row, col] = -40
                    propagator[row, col] = complex(np.exp(phase), 0)
        print('--------------- Propagate in Fourier space -----------------------')
        eft = fieldfft * propagator   
        # filter
        # if hanning > 0 :
        #   hanning=

        print('--------------- Inverse FT to get output field ------ exp(+i ...)')
        self.field= np.fft.ifft2(eft)
        print('--------------- propfourier end ----------------------------------')
    # end propfourier

    def resize(self, center=True, interpolate=False, newsize=None, newy_vec=None, newz_vec=None) :
        """resize the field and grid we assume an equidistant quadratic grid. The default is zero padding 
        to newsize and keep the spatial resolution. Interpolate keeps the area and interpolates to newsize. 
        Providing vectors forces the field to be interpolated and zero padded. The routine modifies the 
        vectors and fields.
        The method functions and calling parameters are different from resize.pro!
        
        hint for fast idl fft: N should be built from prime factors 2,3,5,
        to avoid special treatment of the Nyquist frequency odd N
        are recommended good examples:
        3,    9,  27,  81,  243, 729, 2187 
        5,   25, 125, 625, 3125
        15,  75, 375, 1875
        45, 135, 405, 1215
        225
        if N= 3^k3 + 5^k5 then time = a*(3*k3+5*k5) i.e. 243=>15, 225=>16
        i.e. 243 is faster than 225
        
        Args:
           interpolate=False (bool): interpolate, default is zero padding
           newsize=None (int):     new quadratic gridzize 
           center=True (bool):    zeropadding without centering 
           newy_vec=None:       new y_vector
           newz_vec=None:       new z_vector

        Example:
            >>>emf.resize(interpolate=True, newsize=729)
        """
        skip = False            # switch  
        size= self.field.shape
        if size[0] >= size[1]:
            sizeField = size[0]
        else:
            sizeField = size[1]

        if newsize :
            if newsize <= sizeField :
                print('error: newsize < oldsize- return')
                return
    
        if newy_vec is not None :
            print('new y vector')
            skip = True
            z_vec= self.z_vec
            y_vec= self.y_vec
            field= self.field
            self.y_vec= newy_vec
            fre= scipy.interpolate.interp2d(y_vec, z_vec, np.real(field), kind='cubic', fill_value=0.0)
            fim= scipy.interpolate.interp2d(y_vec, z_vec, np.imag(field), kind='cubic', fill_value=0.0)
            self.field= fre(self.y_vec, self.z_vec) + 1j* fim(self.y_vec, self.z_vec)
            
        if newz_vec is not None:
            print('new z vector')
            skip = True
            z_vec = self.z_vec
            y_vec = self.y_vec
            field = self.field
            self.z_vec = newz_vec
            fre = scipy.interpolate.interp2d(y_vec, z_vec, np.real(field), kind='cubic', fill_value=0.0)
            fim = scipy.interpolate.interp2d(y_vec, z_vec, np.imag(field), kind='cubic', fill_value=0.0)
            self.field = fre(self.y_vec, self.z_vec) + 1j * fim(self.y_vec, self.z_vec)
            
        if skip :             # skip rest if vectors provided 
            return

        if interpolate:
            print('-----------------interpolate-----------')
            print('old= ', sizeField, ' new= ', newsize)
            z_vec = self.z_vec
            y_vec = self.y_vec
            field = self.field 

            self.z_vec = np.linspace(z_vec[0], z_vec[-1], newsize)
            self.y_vec = np.linspace(y_vec[0], y_vec[-1], newsize)
            fre = scipy.interpolate.interp2d(y_vec, z_vec, np.real(field), kind='cubic')
            fim = scipy.interpolate.interp2d(y_vec, z_vec, np.imag(field), kind='cubic')
            self.field= fre(self.y_vec, self.z_vec) + 1j* fim(self.y_vec, self.z_vec)
        else :
            print('----------------- Zero padding of field ------')
            print('old= ', sizeField, ' new= ', newsize)

            newarr = np.zeros((newsize, newsize), dtype=complex)  # make a quadratic array filled with 0
         
            if center:
                colshift = (newsize - size[1]) / 2     # the index to shift 
                rowshift = (newsize - size[0]) / 2     # the index to shift
            else :
                colshift = 0
            rowshift = 0
         
            for row in np.arange(self.y_vec.size):  # copy original field
                for col in np.arange(self.z_vec.size):
                    newarr[row + rowshift, col + colshift] = self.field[row, col]   

            # now the vectors
            z0 = self.z_vec[0]                           # the start value
            y0 = self.y_vec[0]                           # the start value
            dz = self.z_vec[1] - z0                      # stepsize
            dy = self.y_vec[1] - y0                      # stepsize

            self.z_vec = (np.arange(newsize)- colshift) * dz + z0
            self.y_vec = (np.arange(newsize)- rowshift) * dy + y0 
            self.field = newarr
    # resize

    def scalefield(self, scaler):  
        """ scale the field - similar to phaseplate

        Args:
           scaler: double or complex number or array
        """
        self.field *= scaler
    # scalefield

    def setField(self, field):  
        """set a new field

        Args:
           field (complex): np complex array
        """
        self.field= field
    # setField

    def setName(self, name):  
        """set a new name

        Args:
           name (string): name
        """
        self.name= name
    # setName

    def setwavelength(self, wavelength):  
        """set a new wavelength in m

        Arg:
           wavelength (double): wavelength in m

        Example:
          >>> emf = phase.initphase()
              emf.setwavelength(1e-10)
              emf.getwavelength()
        """
        self.wavelength= wavelength
    # end setwavelength   

    def sety_vec(self, vec):  
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

    def setz_vec(self, vec):  
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

    def statistics(self, comment=None, amplitude=False, fit=True):
        """show statistics 

        Args:
           comment=None (string): comment string (optional)
           amplitude=False (bool): amplitude statistics - default: intensity
           fit=True (bool): if false skip 2d fit

        Returns:
           statistics (dictionary)

        Example:
            >>> emf = phase.initphase()
                emf.gaussbeam(example=True)
                emf.statistics()
        """

        if amplitude :
            title = "amplitude statistics"
            myfield = self.getamplitude()
            mymaxstr = 'max field       = {:8.3g} V/m ' 
            mytotstr = 'total field     = {:8.3g} Vm '
        else :
            title = "intensity statistics"
            myfield = self.getintensity()
            mymaxstr = 'max intensity   = {:8.3g} W/m^2 ' 
            mytotstr = 'total intensity = {:8.3g} W   '

        if comment :
            title += ' => ' + comment

        z_vec = self.z_vec
        y_vec = self.y_vec
        wavelength = self.wavelength
        zmin = np.amin(z_vec)
        zmax = np.amax(z_vec)
        ymin = np.amin(y_vec)
        ymax = np.amax(y_vec)
        binsize= (z_vec[1] - z_vec[0]) * (y_vec[1] - y_vec[0])

        mymax = np.amax(myfield)                        # photons/m^2
        mymaxidx = np.unravel_index(myfield.argmax(), myfield.shape)
        mysum = np.sum(myfield)
        mytot = np.sum(myfield) * binsize   # sum of all bins*binsize

        if mymax > 0.0 :
            field_n = myfield / mymax
        else :
            field_n = myfield     # normalized field for fit

        if fit :
            # fit= ##gauss2dfit(field_n, stat, z_vec, y_vec) 
            stat = gausfitter2.gaussfit(field_n, rotate=0)  # don't wonder about strange parameter mapping
            zscale = z_vec[1] - z_vec[0]
            yscale = y_vec[1] - y_vec[0]
            bgr = stat[0]
            amp = stat[1]
            y0 = y_vec[0] + stat[2] * yscale
            z0 = z_vec[0] + stat[3] * zscale
            sigmay = np.abs(stat[4] * yscale)  # UF dont know why sometimes negative
            sigmaz = np.abs(stat[5] * zscale)
            zfwhm = sigmaz * 2.35
            yfwhm = sigmay * 2.35
        else :
            print('we do not fit- we search fwhm')
            z0i = mymaxidx[1]
            y0i = mymaxidx[0]
            mymax05 = 0.5 * mymax
            # print('z0i, y0i, mymax05 ',z0i, y0i, mymax05)
            zcut= myfield[y0i, :]  # cut at certain vertical position
            ycut= myfield[:, z0i]
            zidx= np.reshape(np.asarray((zcut >= mymax05).nonzero()), -1)  # where function
            yidx= np.reshape(np.asarray((ycut >= mymax05).nonzero()), -1)
            # print(zidx, len(zidx), np.shape(zidx), type(zidx), zidx.size)
            if zidx.size > 1 :
                zfwhm = z_vec[zidx[-1]] - z_vec[zidx[0]]
                z0 = z_vec[z0i]
            else :
                zfwhm = 0
                z0 = 0
            if yidx.size > 1 :
                yfwhm = y_vec[yidx[-1]] - y_vec[yidx[0]]
                y0 = y_vec[y0i]          
            else :
                yfwhm = 0
                y0 = 0

        print('==============================================================================')
        print(title)
        print('==============================================================================')
        if fit :
            print('z fwhm = {:7.5g} mm, rms = {:.5g} mm'.format(zfwhm * 1e3, sigmaz * 1e3))
            print('y fwhm = {:7.5g} mm, rms = {:.5g} mm'.format(yfwhm * 1e3, sigmay * 1e3))
            print('z0     = {:7.3g} mm'.format(z0 * 1e3))
            print('y0     = {:7.3g} mm'.format(y0 * 1e3))
        else :
            print('z fwhm = {:7.5g} mm'.format(zfwhm * 1e3))
            print('y fwhm = {:7.5g} mm'.format(yfwhm * 1e3))
            print('z0     = {:7.3g} mm'.format(z_vec[z0i] * 1e3))
            print('y0     = {:7.3g} mm'.format(y_vec[z0i] * 1e3))
         
        print('zmin   = {} mm, zmax = {} mm, nz = {}'.format(zmin * 1e3, zmax * 1e3, z_vec.size))
        print('ymin   = {} mm, ymax = {} mm, ny = {}'.format(ymin * 1e3, ymax * 1e3, y_vec.size))
        print('wavelength      = {:.3f} nm'.format(wavelength * 1e9))
        print(mymaxstr.format(mymax))
        print(mytotstr.format(mytot))
        mymaxp = mymax/(1.6e-19 * 1240e-9 / self.wavelength)
        mytotp = mytot/(1.6e-19 * 1240e-9 / self.wavelength)
        if not amplitude:
            print('max intensity   = {:8.3g} photons/s m^2'.format(mymaxp))
            print('total intensity = {:8.3g} photons/s'.format(mytotp))

        print()    
        print('debug: mysum, binsize=', mysum, binsize)
        print('==============================================================================')
        if fit :
            print('result of gauss2dfit in (m):', stat)
            print('==============================================================================')

        mydict = {'total': mytot, 'max': mymax, 'totalp': mytotp, 'maxp': mymaxp, 
                  'yfwhm': yfwhm, 'zfwhm': zfwhm, 'y0':y0, 'z0': z0}
        return mydict
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
            rw (flt): radius in w in m  
            rl (flt): radius in l in m

        Example:
            >>> emf = phase.initphase()
                ans = emf.torus(s1=10, s2=5, thetan= 30, degree=True)
                print(ans)
        """
        if thetan and degree :
            thetan = thetan * np.pi/180. 
        if thetag and degree :
            thetan = np.pi / 2.0 - thetag * np.pi / 180.
        if thetag and not degree :
            thetan = np.pi/2.0 - thetag
        if (np.abs(thetan) >= np.pi / 2.0) or (np.abs(s1) < 1e-10) or (np.abs(s2) < 1e-10) :
            print("error: unphysical input- return")
            print("usage example: ans = emf.torus(s1=10, s2=5, thetag=1e-3)")
            return

        rw = 1.0 / ((1.0 / s1 + 1.0 / s2) * np.cos(thetan) / 2.0)
        rl = 1.0 / ((1.0 / s1 + 1.0 / s2) / (2.0 * np.cos(thetan)))
        thetag = np.pi / 2.0 - thetan

        if verbose :
            print('=========== torus ==============')
            print('s1    = {:10.4g} m'.format(s1))
            print('s2    = {:10.4g} m'.format(s2))
            print('thetan= {:7.4f} = {:7.4f} deg.'.format(thetan, thetan* 180 / np.pi))
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
# a= Myobj()  # create an instance of the object


class initphase(emf):
    def __init__(self):
        super().__init__()
        print("initphase done")

# end
