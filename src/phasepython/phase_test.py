#!/usr/bin/eval python

"""example script - to use phase.py
   
Args:
    filename (str): filename relative to /afs/psi.ch/intranet/SF/Photonics/sscans/'

Example:

"""

import phase
import mycontour
import sys
import numpy as np
import matplotlib.pyplot as plt

def source_link():
    """dummy function to show the source link in sphinx 
    """

# exclude section to allow automatic documentation
if __name__ == "__main__":
    args = len(sys.argv)
    print("number of args: ", args)
    if args > 1: 
        file = sys.argv[1]

plt.close('all')
emf = phase.initphase()
emf.setName("empty")
emf.gaussbeam(example=True)
power = emf.getintensity()
y = emf.gety_vec()
z = emf.getz_vec()
title = emf.getname()
plt.figure()
mycontour.mycontour(power, z*1e3, y*1e3, xlabel='z (mm)', ylabel='y (mm)', zlabel='power density $(W/m^2)$', title=title)
plt.figure()
emf.mycontour(power, z*1e3, y*1e3, xlabel='z (mm)', ylabel='y (mm)', zlabel='power density $(W/m^2)$', title=title)

rawphase = emf.getphase(param="raw")
plt.figure()
emf.mycontour(rawphase, z*1e3, y*1e3, xlabel='z (mm)', ylabel='y (mm)', zlabel='raw phase (rad)', title=title)

numpyphase = emf.getphase(param="numpy")
plt.figure()
emf.mycontour(numpyphase, z*1e3, y*1e3, xlabel='z (mm)', ylabel='y (mm)', zlabel='numpy phase (rad)', title=title)

emf.propfresnel(drift=1)
plt.figure()
power = emf.getintensity()
y = emf.gety_vec()
z = emf.getz_vec()
emf.mycontour(power, z*1e3, y*1e3, xlabel='z (mm)', ylabel='y (mm)', zlabel='power density $(W/m^2)$', title=title)

rawphase = emf.getphase(param="raw")
plt.figure()
emf.mycontour(rawphase, z*1e3, y*1e3, xlabel='z (mm)', ylabel='y (mm)', zlabel='raw phase (rad)', title=title)

numpyphase = emf.getphase(param="numpy")
plt.figure()
emf.mycontour(numpyphase, z*1e3, y*1e3, xlabel='z (mm)', ylabel='y (mm)', zlabel='numpy phase (rad)', title=title)

if __name__ == "__main__":
     print("main")
#    l.plotslope()                 # plot
#    l.plotheight()
