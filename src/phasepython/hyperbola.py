import numpy as np
import matplotlib.pyplot as plt



def hyperbola(s1=11, s2=32, theta=89, doplot=True, verbose=True) :
   '''define a hyperbolic mirror

   Args:
        s1=11 (float) : entrance arm in m
        s2=31 (float) : virtual exit arm in m
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
   ahyp = abs(s2 - s1) / 2.0                      # semiaxis a
   bhyp = np.sqrt(abs(s1 * s2)) * np.cos(thetahyp)  # semiaxis b (Kosinussatz)
   ecc =  np.sqrt(ahyp**2 + bhyp**2)  # eccentricity 
   gammahyp= np.arcsin(s1 * np.sin(np.pi- 2 * thetahyp)/(2 * ecc))  # angle at virtual focus (Sinussatz)
   print("gammahyp: {:.3f}deg".format(gammahyp*180/np.pi))
   y0hyp= -1.0 * s2 * np.sin(gammahyp)  
   x0hyp= s2 * np.cos(gammahyp)- ecc
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
      
      plt.close('all')
      plt.figure()
      plt.title('hyperbolic scheme s1= {:.2f}m, s2= {:.2f}m, theta= {:.2f}deg.'.
                format(s1, s2, theta))
      plt.plot(x, y, label='hyperbola', color= 'red')
      plt.plot(x,-y,  color= 'red')
      plt.plot(-x,-y,  color= 'red')
      plt.plot(-x, y,  color= 'red')
      plt.plot([ecc, x0hyp],[0, y0hyp], label='s1', color='blue')
      plt.plot([-ecc, x0hyp], [0, y0hyp], label='s2', color='cyan', linestyle='dashed')
      plt.plot([x0hyp, x0hyp+0.4*(x0hyp+ecc)],[y0hyp, y0hyp+0.4*y0hyp],
               label='beam out', color='cyan')
      plt.plot([x0hyp-10, x0hyp+10],[y0hyp+slope * 10, y0hyp-slope * 10],
               label='tangent', color='green')
      
      plt.legend()
      plt.xlabel('x (m)')
      plt.ylabel('y (m)')
      plt.show()
   # end plot
   
   dict= {'s1': s1, 's2': s2, 'theta': theta, 'a': ahyp, 'b': bhyp, 'e': ecc, 'x0' : x0hyp, 'y0': y0hyp, 'slope0': deltahyp}

   return dict


if __name__ == '__main__' :
    print('main called')
    hyperbola()
#end     
