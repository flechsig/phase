import numpy as np
import matplotlib.pyplot as plt


def __hyperbola_height(w, a, b, x0, y0, theta) :
   '''hyperbola_height - internal subroutine 

   reduce output hyperbel4python.red
   '''
   theta2= theta
   x2= w
   u1= x0
   v1= y0
   
   ans= (np.cos(theta2)*np.sin(theta2)*a**2*x2+np.cos(theta2)*np.sin(theta2)*
         b**2*x2+np.cos(theta2)*a**2*v1-
         np.sqrt(np.cos(theta2)**4*x2**2-2.0*np.cos(theta2)**3*u1*x2+2.0*
                 np.cos(theta2)**2*np.sin(theta2)**2*x2**2+2.0*
                 np.cos(theta2)**2*np.sin(theta2)*v1*x2-np.cos(theta2)**2*a**2+
                 np.cos(theta2)**2*u1**2-2.0*np.cos(theta2)*np.sin(theta2)**2*u1*x2-2.0*
                 np.cos(theta2)*np.sin(theta2)*u1*v1+np.sin(theta2)**4*x2**2+2.0*
                 np.sin(theta2)**3*v1*x2+np.sin(theta2)**2*b**2+
                 np.sin(theta2)**2*v1**2)*
         a*b-np.sin(theta2)*b**2*u1)/(np.cos(theta2)**2*a**2-np.sin(theta2)**2*b**2)

   u= ans
   return u

def hyperbola(s1=11, s2=33, theta=89, doplot=True, verbose=True) :
   '''define a hyperbolic mirror

   Args:
        s1=11 (float) : entrance arm in m
        s2=33 (float) : virtual exit arm in m
        theta=89 (float) : angle to normal in deg.
        doplot= True (bool) : do a plot
        verbose= True (bool) : print parameters

   Returns:
        result (dictionary):

   Example:
        >>> dict= hyperbola(s1=10, s2=20, theta= 88)
            dir(dict)
   '''

   thetahyp = theta * np.pi/180
   ahyp = abs(s2 - s1) / 2.0                        # semiaxis a (Abstand Scheitel)
   bhyp = np.sqrt(abs(s1 * s2)) * np.cos(thetahyp)  # semiaxis b (Kosinussatz)
   ecc =  np.sqrt(ahyp**2 + bhyp**2)                # eccentricity (Abstand Brennpunkt)
   gammahyp= np.arcsin(s1 * np.sin(np.pi- 2 * thetahyp)/(2 * ecc))  # angle at virtual focus (Sinussatz)
   #print("gammahyp: {:.3f}deg".format(gammahyp*180/np.pi))
   y0hyp= -1.0 * s2 * np.sin(gammahyp)  # Auftreffpunkt Zentralstrahl
   x0hyp= s2 * np.cos(gammahyp)- ecc    # Auftreffpunkt Zentralstrahl
   deltahyp= gammahyp + (np.pi/2 - thetahyp) # slope at x0y0 = rotation angle 

   if verbose :
      print("hyperbola parameters")
      print("input: s1= {:.3f}m, s2= {:.3f}m, theta= {:.3f}deg.".
         format(s1, s2, thetahyp*180/np.pi))
      print("semiaxis a= {:.3f}m".format(ahyp))
      print("semiaxis b= {:.3f}m".format(bhyp))
      print("eccentricity e= {:.3f}m".format(ecc))
      print("x0= {:.3f}, y0= {:.3f}".format(x0hyp, y0hyp))
      print("slope0 = {:.3f}rad = {:.3f}deg".format(deltahyp, deltahyp*180/np.pi))
   # end verbose   
   

   if doplot :
      x= np.linspace(-2*s1, 3*s1, 1001)
      y= np.sqrt(bhyp**2/ahyp**2 * x**2 - bhyp**2)
      slope= np.tan(deltahyp)

      x1= np.linspace(-10, 10, 1001)
      y1= -np.sqrt(-ahyp**2+x0hyp**2 + 2*x0hyp*x1+x1**2)*bhyp/ahyp-y0hyp
      w2= np.linspace(-0.5, 0.5, 1001)
      u2= __hyperbola_height(w2, ahyp, bhyp, -x0hyp, -y0hyp, deltahyp)
      
      rows = 2   # windows to plot
      cols = 2
      figsize = [18, 11]   # figsize in inches
      sp = 0               # init subplot number
      plt.close('all')
      plt.figure(1, figsize=figsize, tight_layout=True)
      
      sp += 1 
      plt.subplot(rows, cols, sp)
      plt.title('hyperbola s1= {:.2f}m, s2= {:.2f}m, theta= {:.2f}deg.'.
                format(s1, s2, theta))
      plt.plot(x, y, label='hyperbola', color= 'red')
      plt.plot(x,  -y, color= 'red')
      plt.plot(-x, -y, color= 'red')
      plt.plot(-x,  y, color= 'red')
      plt.plot([ecc, x0hyp],[0, y0hyp], label='s1', color='blue')
      plt.plot([-ecc, x0hyp], [0, y0hyp], label='s2', color='cyan', linestyle='dashed')
      plt.plot([x0hyp, x0hyp+0.4*(x0hyp+ecc)],[y0hyp, y0hyp+0.4*y0hyp],
               label='beam out', color='cyan')
      plt.plot([x0hyp-10, x0hyp+10],[y0hyp+slope * 10, y0hyp-slope * 10],
               label='tangent', color='green')
      
      plt.legend()
      plt.xlabel('x (m)')
      plt.ylabel('y (m)')

      #plt.figure()
      sp += 1
      plt.subplot(rows, cols, sp)
      plt.title('hyperbola shifted to {:.2f}, {:.2f}'.format(x0hyp,y0hyp))
      plt.plot(x1, y1, label='hyperbola', color= 'red')
      plt.plot([ecc-x0hyp, 0], [-y0hyp, 0], label='s1', color='blue')
      plt.plot([0, 0.4*(x0hyp+ecc)],[0, 0.4*y0hyp],
               label='beam out', color='cyan')
      plt.plot([-10, 10],[slope * 10, -slope * 10],
               label='tangent', color='green')
      
      plt.legend()
      plt.xlabel('x (m)')
      plt.ylabel('y (m)')
      
      sp += 1
      scale= 1e9
      w20= -u2[0]* np.tan(thetahyp)
      w21= -w20
      print(thetahyp*180/np.pi)
      plt.subplot(rows, cols, sp)
      plt.title('hyperbola shifted and rotated, mirror length: {:.2f} m'.format(w2[-1]-w2[0]))
      plt.plot(w2, u2*scale, label='hyperbola', color= 'red')
      plt.plot([w20, 0], [u2[0]*scale, 0], label='s1', color='blue')
      plt.plot([0, w21],[0, u2[0]*scale],
               label='beam out', color='cyan')
      plt.legend()
      plt.xlabel('w (m)')
      plt.ylabel('u (nm)')
      plt.show()
   # end plot
   
   dict= {'s1': s1, 's2': s2, 'theta': theta, 'a': ahyp, 'b': bhyp, 'e': ecc, 'x0' : x0hyp, 'y0': y0hyp,
          'slope0': deltahyp, 'slope0_deg': deltahyp*180/np.pi}

   return dict


if __name__ == '__main__' :
    print('main called')
    pars= hyperbola()
#end     
