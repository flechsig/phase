from h5_write_surf_error import *
import numpy as np
import matplotlib.pyplot as plt
import mycontour

"""example for the h5_write_surf_error
   
"""


def source_link():
    """dummy function to show the source link in sphinx 
    """

nw= 777
nl= 123
w= np.linspace(-0.5,   0.5,   nw)
l= np.linspace(-0.005, 0.005, nl)
u= np.zeros((nl, nw))

height= 22.0e-9
t1= 0.8

for row in np.arange(nl) :
    for col in np.arange(nw) :
        u[row, col]= height*np.cos(2*np.pi/t1 * w[col])+ height*l[row]/0.005

h5_write_surf_error('surf_err.h5', 'test', u, w, l)

if __name__ == "__main__":
    print("main")
    plt.close('all')
    plt.figure(1, figsize=[13,9], tight_layout=True)
    plt.subplot(2, 2, 1)
    mycontour.mycontour(u*1e9, w*1e3, l*1e3, title='height error', 
                        xlabel='w (mm)', ylabel='l (mm)', zlabel='u (nm)')
    
    plt.subplot(2, 2, 2)
    plt.plot(u[:, 0]*         1e9, l* 1e3, label='min.')
    plt.plot(u[:, int(nw/2)]* 1e9, l* 1e3, label='center')
    plt.plot(u[:, -1]*        1e9, l* 1e3, label='max')
    plt.ylabel('l (mm)')
    plt.xlabel('u (nm)')
    plt.legend()

    plt.subplot(2, 2, 3)
    plt.plot(w* 1e3, u[0,:]*         1e9, label='min.')
    plt.plot(w* 1e3, u[int(nl/2),:]* 1e9, label='center')
    plt.plot(w* 1e3, u[-1,:]*        1e9, label='max.')
    plt.ylabel('u (nm)')
    plt.xlabel('w (mm)')
    plt.legend()

    plt.show()
