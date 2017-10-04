from gausfitter2 import *
import numpy as np
import matplotlib.pyplot as plt
import mycontour

"""debug the gausfitter2.py
   check the very strange output parameter
   the rotation has not been checked 
"""

def source_link():
    """dummy function to show the source link in sphinx 
    """

nx= 111
ny= 123
x= np.linspace(-10., 10, nx)
y= np.linspace(-10., 10, ny)
z= np.zeros((ny, nx))

height=22.0
amplitude= 33.
xp= -3.5
yp= -2.3
width_x= 4.0
width_y= 1.1
scalex= x[1]-x[0]
scaley= y[1]-y[0]
#print(scalex, scaley)

for row in np.arange(ny) :
   rcen_y= y[row]
   for col in np.arange(nx) :
      rcen_x= x[col]
      z[row,col]= height + amplitude * exp(
            -(((rcen_x - xp)/width_x)**2+
            ((rcen_y - yp)/width_y)**2)/2.)


#params=[height*0.5, amplitude*2., 1, 1, 0.5, 1.5, 10]
stat= gaussfit(z, rotate=0)
# return values
print('height    (inp, fit) {:.2f}, {:.2f}'.format(height,    stat[0]))
print('amplitude (inp, fit) {:.2f}, {:.2f}'.format(amplitude, stat[1]))
print('xp (inp, fit) {:.2f}, {:.2f}'.format(xp, x[0]+stat[3]*scalex))  
print('yp (inp, fit) {:.2f}, {:.2f}'.format(yp, y[0]+stat[2]*scaley))
print('sigmax (inp, fit) {:.2f}, {:.2f}'.format(width_x, stat[5]*scalex))
print('sigmay (inp, fit) {:.2f}, {:.2f}'.format(width_y, stat[4]*scaley*(-1)))  # better use np.abs()
print('stat: ', stat)

if __name__ == "__main__":
   plt.figure()
   mycontour.mycontour(z, x, y)
   plt.show()
