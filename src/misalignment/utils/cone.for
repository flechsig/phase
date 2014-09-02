      function cone(x,y)
      r=1000.0
      rho=500.0
      s=200.0

      rmrho= r- rho
      rprho= r+ rho

      p2= (s* rprho + 2.0 * x * rmrho)/ 2.0 / s

      cone= p2- sqrt(p2*p2-y*y)
      
      return
      end
