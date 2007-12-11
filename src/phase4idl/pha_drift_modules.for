 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
      module drift_common_mod
      
      integer ianzy0,ianzz0,ianzy,ianzz,ired,imode
      
      real*8     eyre(1024,1024),      eyim(1024,1024)
     &          ,ezre(1024,1024),      ezim(1024,1024)
     &        ,eyre_1(1024,1024),    eyim_1(1024,1024)
     &        ,ezre_1(1024,1024),    ezim_1(1024,1024)
     &      ,eyre_red(1024,1024),  eyim_red(1024,1024)
     &      ,ezre_red(1024,1024),  ezim_red(1024,1024)
      
     &      ,    y(1024),     z(1024)
     &      ,  y_1(1024),   z_1(1024)
     &      ,y_red(1024), z_red(1024)
c Grenzen des Zielgrids ....     
     &      ,ymin, ymax, zmin, zmax
     
     &      ,dy, dz, dy_1, dz_1 ,   distance
     &      , xlam , cc,  pi,  pihalf
     
     &      ,xarg(1024)
     &      ,a1(1024,1024),b1(1024,1024)
     &      ,a2(1024,1024),b2(1024,1024)
     &      ,a11(1024,1024),b11(1024,1024)
     &      ,a111(1024,1024),b111(1024,1024)
     
     &      ,e2l1m,energy
     
      
      complex*16    ey(1024,1024),   ez(1024,1024)
     &           ,ey_1(1024,1024), ez_1(1024,1024)     
     
     &           ,xm1,sqrtm1,fact,quad_fac,quad_fac_1
     
     
      end module drift_common_mod
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77

