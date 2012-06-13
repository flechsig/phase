      subroutine p_m_4(dp5,dp6,h2c)
      implicit real*8(a-h,o-z)
      dimension   dp5(0:4,0:4,0:4,0:4),
     &            dp6(0:4,0:4,0:4,0:4)
      dimension   h2c(0:4,0:4,0:4,0:4)
      h2c(0.,0.,0.,0.)=dp5(0.,0.,0.,0.)*dp6(0.,0.,0.,0.)
      h2c(0.,0.,0.,1.)=dp5(0.,0.,0.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,0.,0.,1.)
      h2c(0.,0.,0.,2.)=dp5(0.,0.,0.,2.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,0.,1.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,0.,0.,2.)
      h2c(0.,0.,0.,3.)=dp5(0.,0.,0.,3.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,0.,2.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,0.,1.)*
     . dp6(0.,0.,0.,2.)+dp5(0.,0.,0.,0.)*dp6(0.,0.,0.,3.)
      h2c(0.,0.,0.,4.)=dp5(0.,0.,0.,4.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,0.,3.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,0.,2.)*
     . dp6(0.,0.,0.,2.)+dp5(0.,0.,0.,1.)*dp6(0.,0.,0.,3.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,0.,0.,4.)
      h2c(0.,0.,1.,0.)=dp5(0.,0.,1.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,0.,1.,0.)
      h2c(0.,0.,1.,1.)=dp5(0.,0.,1.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,0.,1.)*
     . dp6(0.,0.,1.,0.)+dp5(0.,0.,0.,0.)*dp6(0.,0.,1.,1.)
      h2c(0.,0.,1.,2.)=dp5(0.,0.,1.,2.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,1.,1.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,1.,0.)*
     . dp6(0.,0.,0.,2.)+dp5(0.,0.,0.,2.)*dp6(0.,0.,1.,0.)+
     . dp5(0.,0.,0.,1.)*dp6(0.,0.,1.,1.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,0.,1.,2.)
      h2c(0.,0.,1.,3.)=dp5(0.,0.,1.,3.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,1.,2.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,1.,1.)*
     . dp6(0.,0.,0.,2.)+dp5(0.,0.,1.,0.)*dp6(0.,0.,0.,3.)+
     . dp5(0.,0.,0.,3.)*dp6(0.,0.,1.,0.)+dp5(0.,0.,0.,2.)*
     . dp6(0.,0.,1.,1.)+dp5(0.,0.,0.,1.)*dp6(0.,0.,1.,2.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,0.,1.,3.)
      h2c(0.,0.,2.,0.)=dp5(0.,0.,2.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(0.,0.,1.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,0.,2.,0.)
      h2c(0.,0.,2.,1.)=dp5(0.,0.,2.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,2.,0.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,1.,1.)*
     . dp6(0.,0.,1.,0.)+dp5(0.,0.,1.,0.)*dp6(0.,0.,1.,1.)+
     . dp5(0.,0.,0.,1.)*dp6(0.,0.,2.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,0.,2.,1.)
      h2c(0.,0.,2.,2.)=dp5(0.,0.,2.,2.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,2.,1.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,2.,0.)*
     . dp6(0.,0.,0.,2.)+dp5(0.,0.,1.,2.)*dp6(0.,0.,1.,0.)+
     . dp5(0.,0.,1.,1.)*dp6(0.,0.,1.,1.)+dp5(0.,0.,1.,0.)*
     . dp6(0.,0.,1.,2.)+dp5(0.,0.,0.,2.)*dp6(0.,0.,2.,0.)+
     . dp5(0.,0.,0.,1.)*dp6(0.,0.,2.,1.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,0.,2.,2.)
      h2c(0.,0.,3.,0.)=dp5(0.,0.,3.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,2.,0.)*dp6(0.,0.,1.,0.)+dp5(0.,0.,1.,0.)*
     . dp6(0.,0.,2.,0.)+dp5(0.,0.,0.,0.)*dp6(0.,0.,3.,0.)
      h2c(0.,0.,3.,1.)=dp5(0.,0.,3.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,3.,0.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,2.,1.)*
     . dp6(0.,0.,1.,0.)+dp5(0.,0.,2.,0.)*dp6(0.,0.,1.,1.)+
     . dp5(0.,0.,1.,1.)*dp6(0.,0.,2.,0.)+dp5(0.,0.,1.,0.)*
     . dp6(0.,0.,2.,1.)+dp5(0.,0.,0.,1.)*dp6(0.,0.,3.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,0.,3.,1.)
      h2c(0.,0.,4.,0.)=dp5(0.,0.,4.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,3.,0.)*dp6(0.,0.,1.,0.)+dp5(0.,0.,2.,0.)*
     . dp6(0.,0.,2.,0.)+dp5(0.,0.,1.,0.)*dp6(0.,0.,3.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,0.,4.,0.)
      h2c(0.,1.,0.,0.)=dp5(0.,1.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,1.,0.,0.)
      h2c(0.,1.,0.,1.)=dp5(0.,1.,0.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,1.,0.,0.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,0.,1.)*
     . dp6(0.,1.,0.,0.)+dp5(0.,0.,0.,0.)*dp6(0.,1.,0.,1.)
      h2c(0.,1.,0.,2.)=dp5(0.,1.,0.,2.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,1.,0.,1.)*dp6(0.,0.,0.,1.)+dp5(0.,1.,0.,0.)*
     . dp6(0.,0.,0.,2.)+dp5(0.,0.,0.,2.)*dp6(0.,1.,0.,0.)+
     . dp5(0.,0.,0.,1.)*dp6(0.,1.,0.,1.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,1.,0.,2.)
      h2c(0.,1.,0.,3.)=dp5(0.,1.,0.,3.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,1.,0.,2.)*dp6(0.,0.,0.,1.)+dp5(0.,1.,0.,1.)*
     . dp6(0.,0.,0.,2.)+dp5(0.,1.,0.,0.)*dp6(0.,0.,0.,3.)+
     . dp5(0.,0.,0.,3.)*dp6(0.,1.,0.,0.)+dp5(0.,0.,0.,2.)*
     . dp6(0.,1.,0.,1.)+dp5(0.,0.,0.,1.)*dp6(0.,1.,0.,2.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,1.,0.,3.)
      h2c(0.,1.,1.,0.)=dp5(0.,1.,1.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,1.,0.,0.)*dp6(0.,0.,1.,0.)+dp5(0.,0.,1.,0.)*
     . dp6(0.,1.,0.,0.)+dp5(0.,0.,0.,0.)*dp6(0.,1.,1.,0.)
      h2c(0.,1.,1.,1.)=dp5(0.,1.,1.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,1.,1.,0.)*dp6(0.,0.,0.,1.)+dp5(0.,1.,0.,1.)*
     . dp6(0.,0.,1.,0.)+dp5(0.,1.,0.,0.)*dp6(0.,0.,1.,1.)+
     . dp5(0.,0.,1.,1.)*dp6(0.,1.,0.,0.)+dp5(0.,0.,1.,0.)*
     . dp6(0.,1.,0.,1.)+dp5(0.,0.,0.,1.)*dp6(0.,1.,1.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,1.,1.,1.)
      h2c(0.,1.,1.,2.)=dp5(0.,1.,1.,2.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,1.,1.,1.)*dp6(0.,0.,0.,1.)+dp5(0.,1.,1.,0.)*
     . dp6(0.,0.,0.,2.)+dp5(0.,1.,0.,2.)*dp6(0.,0.,1.,0.)+
     . dp5(0.,1.,0.,1.)*dp6(0.,0.,1.,1.)+dp5(0.,1.,0.,0.)*
     . dp6(0.,0.,1.,2.)+dp5(0.,0.,1.,2.)*dp6(0.,1.,0.,0.)+
     . dp5(0.,0.,1.,1.)*dp6(0.,1.,0.,1.)+dp5(0.,0.,1.,0.)*
     . dp6(0.,1.,0.,2.)+dp5(0.,0.,0.,2.)*dp6(0.,1.,1.,0.)+
     . dp5(0.,0.,0.,1.)*dp6(0.,1.,1.,1.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,1.,1.,2.)
      h2c(0.,1.,2.,0.)=dp5(0.,1.,2.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,1.,1.,0.)*dp6(0.,0.,1.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(0.,0.,2.,0.)+dp5(0.,0.,2.,0.)*dp6(0.,1.,0.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(0.,1.,1.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,1.,2.,0.)
      h2c(0.,1.,2.,1.)=dp5(0.,1.,2.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,1.,2.,0.)*dp6(0.,0.,0.,1.)+dp5(0.,1.,1.,1.)*
     . dp6(0.,0.,1.,0.)+dp5(0.,1.,1.,0.)*dp6(0.,0.,1.,1.)+
     . dp5(0.,1.,0.,1.)*dp6(0.,0.,2.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(0.,0.,2.,1.)+dp5(0.,0.,2.,1.)*dp6(0.,1.,0.,0.)+
     . dp5(0.,0.,2.,0.)*dp6(0.,1.,0.,1.)+dp5(0.,0.,1.,1.)*
     . dp6(0.,1.,1.,0.)+dp5(0.,0.,1.,0.)*dp6(0.,1.,1.,1.)+
     . dp5(0.,0.,0.,1.)*dp6(0.,1.,2.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,1.,2.,1.)
      h2c(0.,1.,3.,0.)=dp5(0.,1.,3.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,1.,2.,0.)*dp6(0.,0.,1.,0.)+dp5(0.,1.,1.,0.)*
     . dp6(0.,0.,2.,0.)+dp5(0.,1.,0.,0.)*dp6(0.,0.,3.,0.)+
     . dp5(0.,0.,3.,0.)*dp6(0.,1.,0.,0.)+dp5(0.,0.,2.,0.)*
     . dp6(0.,1.,1.,0.)+dp5(0.,0.,1.,0.)*dp6(0.,1.,2.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,1.,3.,0.)
      h2c(0.,2.,0.,0.)=dp5(0.,2.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,1.,0.,0.)*dp6(0.,1.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,2.,0.,0.)
      h2c(0.,2.,0.,1.)=dp5(0.,2.,0.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,2.,0.,0.)*dp6(0.,0.,0.,1.)+dp5(0.,1.,0.,1.)*
     . dp6(0.,1.,0.,0.)+dp5(0.,1.,0.,0.)*dp6(0.,1.,0.,1.)+
     . dp5(0.,0.,0.,1.)*dp6(0.,2.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,2.,0.,1.)
      h2c(0.,2.,0.,2.)=dp5(0.,2.,0.,2.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,2.,0.,1.)*dp6(0.,0.,0.,1.)+dp5(0.,2.,0.,0.)*
     . dp6(0.,0.,0.,2.)+dp5(0.,1.,0.,2.)*dp6(0.,1.,0.,0.)+
     . dp5(0.,1.,0.,1.)*dp6(0.,1.,0.,1.)+dp5(0.,1.,0.,0.)*
     . dp6(0.,1.,0.,2.)+dp5(0.,0.,0.,2.)*dp6(0.,2.,0.,0.)+
     . dp5(0.,0.,0.,1.)*dp6(0.,2.,0.,1.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,2.,0.,2.)
      h2c(0.,2.,1.,0.)=dp5(0.,2.,1.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,2.,0.,0.)*dp6(0.,0.,1.,0.)+dp5(0.,1.,1.,0.)*
     . dp6(0.,1.,0.,0.)+dp5(0.,1.,0.,0.)*dp6(0.,1.,1.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(0.,2.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,2.,1.,0.)
      h2c(0.,2.,1.,1.)=dp5(0.,2.,1.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,2.,1.,0.)*dp6(0.,0.,0.,1.)+dp5(0.,2.,0.,1.)*
     . dp6(0.,0.,1.,0.)+dp5(0.,2.,0.,0.)*dp6(0.,0.,1.,1.)+
     . dp5(0.,1.,1.,1.)*dp6(0.,1.,0.,0.)+dp5(0.,1.,1.,0.)*
     . dp6(0.,1.,0.,1.)+dp5(0.,1.,0.,1.)*dp6(0.,1.,1.,0.)+
     . dp5(0.,1.,0.,0.)*dp6(0.,1.,1.,1.)+dp5(0.,0.,1.,1.)*
     . dp6(0.,2.,0.,0.)+dp5(0.,0.,1.,0.)*dp6(0.,2.,0.,1.)+
     . dp5(0.,0.,0.,1.)*dp6(0.,2.,1.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,2.,1.,1.)
      h2c(0.,2.,2.,0.)=dp5(0.,2.,2.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,2.,1.,0.)*dp6(0.,0.,1.,0.)+dp5(0.,2.,0.,0.)*
     . dp6(0.,0.,2.,0.)+dp5(0.,1.,2.,0.)*dp6(0.,1.,0.,0.)+
     . dp5(0.,1.,1.,0.)*dp6(0.,1.,1.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(0.,1.,2.,0.)+dp5(0.,0.,2.,0.)*dp6(0.,2.,0.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(0.,2.,1.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(0.,2.,2.,0.)
      h2c(0.,3.,0.,0.)=dp5(0.,3.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,2.,0.,0.)*dp6(0.,1.,0.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(0.,2.,0.,0.)+dp5(0.,0.,0.,0.)*dp6(0.,3.,0.,0.)
      h2c(0.,3.,0.,1.)=dp5(0.,3.,0.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,3.,0.,0.)*dp6(0.,0.,0.,1.)+dp5(0.,2.,0.,1.)*
     . dp6(0.,1.,0.,0.)+dp5(0.,2.,0.,0.)*dp6(0.,1.,0.,1.)+
     . dp5(0.,1.,0.,1.)*dp6(0.,2.,0.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(0.,2.,0.,1.)+dp5(0.,0.,0.,1.)*dp6(0.,3.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,3.,0.,1.)
      h2c(0.,3.,1.,0.)=dp5(0.,3.,1.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,3.,0.,0.)*dp6(0.,0.,1.,0.)+dp5(0.,2.,1.,0.)*
     . dp6(0.,1.,0.,0.)+dp5(0.,2.,0.,0.)*dp6(0.,1.,1.,0.)+
     . dp5(0.,1.,1.,0.)*dp6(0.,2.,0.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(0.,2.,1.,0.)+dp5(0.,0.,1.,0.)*dp6(0.,3.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,3.,1.,0.)
      h2c(0.,4.,0.,0.)=dp5(0.,4.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,3.,0.,0.)*dp6(0.,1.,0.,0.)+dp5(0.,2.,0.,0.)*
     . dp6(0.,2.,0.,0.)+dp5(0.,1.,0.,0.)*dp6(0.,3.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(0.,4.,0.,0.)
      h2c(1.,0.,0.,0.)=dp5(1.,0.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(1.,0.,0.,0.)
      h2c(1.,0.,0.,1.)=dp5(1.,0.,0.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,0.,0.)*dp6(0.,0.,0.,1.)+dp5(0.,0.,0.,1.)*
     . dp6(1.,0.,0.,0.)+dp5(0.,0.,0.,0.)*dp6(1.,0.,0.,1.)
      h2c(1.,0.,0.,2.)=dp5(1.,0.,0.,2.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,0.,1.)*dp6(0.,0.,0.,1.)+dp5(1.,0.,0.,0.)*
     . dp6(0.,0.,0.,2.)+dp5(0.,0.,0.,2.)*dp6(1.,0.,0.,0.)+
     . dp5(0.,0.,0.,1.)*dp6(1.,0.,0.,1.)+dp5(0.,0.,0.,0.)*
     . dp6(1.,0.,0.,2.)
      h2c(1.,0.,0.,3.)=dp5(1.,0.,0.,3.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,0.,2.)*dp6(0.,0.,0.,1.)+dp5(1.,0.,0.,1.)*
     . dp6(0.,0.,0.,2.)+dp5(1.,0.,0.,0.)*dp6(0.,0.,0.,3.)+
     . dp5(0.,0.,0.,3.)*dp6(1.,0.,0.,0.)+dp5(0.,0.,0.,2.)*
     . dp6(1.,0.,0.,1.)+dp5(0.,0.,0.,1.)*dp6(1.,0.,0.,2.)+
     . dp5(0.,0.,0.,0.)*dp6(1.,0.,0.,3.)
      h2c(1.,0.,1.,0.)=dp5(1.,0.,1.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,0.,0.)*dp6(0.,0.,1.,0.)+dp5(0.,0.,1.,0.)*
     . dp6(1.,0.,0.,0.)+dp5(0.,0.,0.,0.)*dp6(1.,0.,1.,0.)
      h2c(1.,0.,1.,1.)=dp5(1.,0.,1.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,1.,0.)*dp6(0.,0.,0.,1.)+dp5(1.,0.,0.,1.)*
     . dp6(0.,0.,1.,0.)+dp5(1.,0.,0.,0.)*dp6(0.,0.,1.,1.)+
     . dp5(0.,0.,1.,1.)*dp6(1.,0.,0.,0.)+dp5(0.,0.,1.,0.)*
     . dp6(1.,0.,0.,1.)+dp5(0.,0.,0.,1.)*dp6(1.,0.,1.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(1.,0.,1.,1.)
      h2c(1.,0.,1.,2.)=dp5(1.,0.,1.,2.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,1.,1.)*dp6(0.,0.,0.,1.)+dp5(1.,0.,1.,0.)*
     . dp6(0.,0.,0.,2.)+dp5(1.,0.,0.,2.)*dp6(0.,0.,1.,0.)+
     . dp5(1.,0.,0.,1.)*dp6(0.,0.,1.,1.)+dp5(1.,0.,0.,0.)*
     . dp6(0.,0.,1.,2.)+dp5(0.,0.,1.,2.)*dp6(1.,0.,0.,0.)+
     . dp5(0.,0.,1.,1.)*dp6(1.,0.,0.,1.)+dp5(0.,0.,1.,0.)*
     . dp6(1.,0.,0.,2.)+dp5(0.,0.,0.,2.)*dp6(1.,0.,1.,0.)+
     . dp5(0.,0.,0.,1.)*dp6(1.,0.,1.,1.)+dp5(0.,0.,0.,0.)*
     . dp6(1.,0.,1.,2.)
      h2c(1.,0.,2.,0.)=dp5(1.,0.,2.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,1.,0.)*dp6(0.,0.,1.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(0.,0.,2.,0.)+dp5(0.,0.,2.,0.)*dp6(1.,0.,0.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(1.,0.,1.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(1.,0.,2.,0.)
      h2c(1.,0.,2.,1.)=dp5(1.,0.,2.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,2.,0.)*dp6(0.,0.,0.,1.)+dp5(1.,0.,1.,1.)*
     . dp6(0.,0.,1.,0.)+dp5(1.,0.,1.,0.)*dp6(0.,0.,1.,1.)+
     . dp5(1.,0.,0.,1.)*dp6(0.,0.,2.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(0.,0.,2.,1.)+dp5(0.,0.,2.,1.)*dp6(1.,0.,0.,0.)+
     . dp5(0.,0.,2.,0.)*dp6(1.,0.,0.,1.)+dp5(0.,0.,1.,1.)*
     . dp6(1.,0.,1.,0.)+dp5(0.,0.,1.,0.)*dp6(1.,0.,1.,1.)+
     . dp5(0.,0.,0.,1.)*dp6(1.,0.,2.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(1.,0.,2.,1.)
      h2c(1.,0.,3.,0.)=dp5(1.,0.,3.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,2.,0.)*dp6(0.,0.,1.,0.)+dp5(1.,0.,1.,0.)*
     . dp6(0.,0.,2.,0.)+dp5(1.,0.,0.,0.)*dp6(0.,0.,3.,0.)+
     . dp5(0.,0.,3.,0.)*dp6(1.,0.,0.,0.)+dp5(0.,0.,2.,0.)*
     . dp6(1.,0.,1.,0.)+dp5(0.,0.,1.,0.)*dp6(1.,0.,2.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(1.,0.,3.,0.)
      h2c(1.,1.,0.,0.)=dp5(1.,1.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,0.,0.)*dp6(0.,1.,0.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(1.,0.,0.,0.)+dp5(0.,0.,0.,0.)*dp6(1.,1.,0.,0.)
      h2c(1.,1.,0.,1.)=dp5(1.,1.,0.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,1.,0.,0.)*dp6(0.,0.,0.,1.)+dp5(1.,0.,0.,1.)*
     . dp6(0.,1.,0.,0.)+dp5(1.,0.,0.,0.)*dp6(0.,1.,0.,1.)+
     . dp5(0.,1.,0.,1.)*dp6(1.,0.,0.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(1.,0.,0.,1.)+dp5(0.,0.,0.,1.)*dp6(1.,1.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(1.,1.,0.,1.)
      h2c(1.,1.,0.,2.)=dp5(1.,1.,0.,2.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,1.,0.,1.)*dp6(0.,0.,0.,1.)+dp5(1.,1.,0.,0.)*
     . dp6(0.,0.,0.,2.)+dp5(1.,0.,0.,2.)*dp6(0.,1.,0.,0.)+
     . dp5(1.,0.,0.,1.)*dp6(0.,1.,0.,1.)+dp5(1.,0.,0.,0.)*
     . dp6(0.,1.,0.,2.)+dp5(0.,1.,0.,2.)*dp6(1.,0.,0.,0.)+
     . dp5(0.,1.,0.,1.)*dp6(1.,0.,0.,1.)+dp5(0.,1.,0.,0.)*
     . dp6(1.,0.,0.,2.)+dp5(0.,0.,0.,2.)*dp6(1.,1.,0.,0.)+
     . dp5(0.,0.,0.,1.)*dp6(1.,1.,0.,1.)+dp5(0.,0.,0.,0.)*
     . dp6(1.,1.,0.,2.)
      h2c(1.,1.,1.,0.)=dp5(1.,1.,1.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,1.,0.,0.)*dp6(0.,0.,1.,0.)+dp5(1.,0.,1.,0.)*
     . dp6(0.,1.,0.,0.)+dp5(1.,0.,0.,0.)*dp6(0.,1.,1.,0.)+
     . dp5(0.,1.,1.,0.)*dp6(1.,0.,0.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(1.,0.,1.,0.)+dp5(0.,0.,1.,0.)*dp6(1.,1.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(1.,1.,1.,0.)
      h2c(1.,1.,1.,1.)=dp5(1.,1.,1.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,1.,1.,0.)*dp6(0.,0.,0.,1.)+dp5(1.,1.,0.,1.)*
     . dp6(0.,0.,1.,0.)+dp5(1.,1.,0.,0.)*dp6(0.,0.,1.,1.)+
     . dp5(1.,0.,1.,1.)*dp6(0.,1.,0.,0.)+dp5(1.,0.,1.,0.)*
     . dp6(0.,1.,0.,1.)+dp5(1.,0.,0.,1.)*dp6(0.,1.,1.,0.)+
     . dp5(1.,0.,0.,0.)*dp6(0.,1.,1.,1.)+dp5(0.,1.,1.,1.)*
     . dp6(1.,0.,0.,0.)+dp5(0.,1.,1.,0.)*dp6(1.,0.,0.,1.)+
     . dp5(0.,1.,0.,1.)*dp6(1.,0.,1.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(1.,0.,1.,1.)+dp5(0.,0.,1.,1.)*dp6(1.,1.,0.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(1.,1.,0.,1.)+dp5(0.,0.,0.,1.)*
     . dp6(1.,1.,1.,0.)+dp5(0.,0.,0.,0.)*dp6(1.,1.,1.,1.)
      h2c(1.,1.,2.,0.)=dp5(1.,1.,2.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,1.,1.,0.)*dp6(0.,0.,1.,0.)+dp5(1.,1.,0.,0.)*
     . dp6(0.,0.,2.,0.)+dp5(1.,0.,2.,0.)*dp6(0.,1.,0.,0.)+
     . dp5(1.,0.,1.,0.)*dp6(0.,1.,1.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(0.,1.,2.,0.)+dp5(0.,1.,2.,0.)*dp6(1.,0.,0.,0.)+
     . dp5(0.,1.,1.,0.)*dp6(1.,0.,1.,0.)+dp5(0.,1.,0.,0.)*
     . dp6(1.,0.,2.,0.)+dp5(0.,0.,2.,0.)*dp6(1.,1.,0.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(1.,1.,1.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(1.,1.,2.,0.)
      h2c(1.,2.,0.,0.)=dp5(1.,2.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,1.,0.,0.)*dp6(0.,1.,0.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(0.,2.,0.,0.)+dp5(0.,2.,0.,0.)*dp6(1.,0.,0.,0.)+
     . dp5(0.,1.,0.,0.)*dp6(1.,1.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(1.,2.,0.,0.)
      h2c(1.,2.,0.,1.)=dp5(1.,2.,0.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,2.,0.,0.)*dp6(0.,0.,0.,1.)+dp5(1.,1.,0.,1.)*
     . dp6(0.,1.,0.,0.)+dp5(1.,1.,0.,0.)*dp6(0.,1.,0.,1.)+
     . dp5(1.,0.,0.,1.)*dp6(0.,2.,0.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(0.,2.,0.,1.)+dp5(0.,2.,0.,1.)*dp6(1.,0.,0.,0.)+
     . dp5(0.,2.,0.,0.)*dp6(1.,0.,0.,1.)+dp5(0.,1.,0.,1.)*
     . dp6(1.,1.,0.,0.)+dp5(0.,1.,0.,0.)*dp6(1.,1.,0.,1.)+
     . dp5(0.,0.,0.,1.)*dp6(1.,2.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(1.,2.,0.,1.)
      h2c(1.,2.,1.,0.)=dp5(1.,2.,1.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,2.,0.,0.)*dp6(0.,0.,1.,0.)+dp5(1.,1.,1.,0.)*
     . dp6(0.,1.,0.,0.)+dp5(1.,1.,0.,0.)*dp6(0.,1.,1.,0.)+
     . dp5(1.,0.,1.,0.)*dp6(0.,2.,0.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(0.,2.,1.,0.)+dp5(0.,2.,1.,0.)*dp6(1.,0.,0.,0.)+
     . dp5(0.,2.,0.,0.)*dp6(1.,0.,1.,0.)+dp5(0.,1.,1.,0.)*
     . dp6(1.,1.,0.,0.)+dp5(0.,1.,0.,0.)*dp6(1.,1.,1.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(1.,2.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(1.,2.,1.,0.)
      h2c(1.,3.,0.,0.)=dp5(1.,3.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,2.,0.,0.)*dp6(0.,1.,0.,0.)+dp5(1.,1.,0.,0.)*
     . dp6(0.,2.,0.,0.)+dp5(1.,0.,0.,0.)*dp6(0.,3.,0.,0.)+
     . dp5(0.,3.,0.,0.)*dp6(1.,0.,0.,0.)+dp5(0.,2.,0.,0.)*
     . dp6(1.,1.,0.,0.)+dp5(0.,1.,0.,0.)*dp6(1.,2.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(1.,3.,0.,0.)
      h2c(2.,0.,0.,0.)=dp5(2.,0.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(1.,0.,0.,0.)*dp6(1.,0.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(2.,0.,0.,0.)
      h2c(2.,0.,0.,1.)=dp5(2.,0.,0.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(2.,0.,0.,0.)*dp6(0.,0.,0.,1.)+dp5(1.,0.,0.,1.)*
     . dp6(1.,0.,0.,0.)+dp5(1.,0.,0.,0.)*dp6(1.,0.,0.,1.)+
     . dp5(0.,0.,0.,1.)*dp6(2.,0.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(2.,0.,0.,1.)
      h2c(2.,0.,0.,2.)=dp5(2.,0.,0.,2.)*dp6(0.,0.,0.,0.)+
     . dp5(2.,0.,0.,1.)*dp6(0.,0.,0.,1.)+dp5(2.,0.,0.,0.)*
     . dp6(0.,0.,0.,2.)+dp5(1.,0.,0.,2.)*dp6(1.,0.,0.,0.)+
     . dp5(1.,0.,0.,1.)*dp6(1.,0.,0.,1.)+dp5(1.,0.,0.,0.)*
     . dp6(1.,0.,0.,2.)+dp5(0.,0.,0.,2.)*dp6(2.,0.,0.,0.)+
     . dp5(0.,0.,0.,1.)*dp6(2.,0.,0.,1.)+dp5(0.,0.,0.,0.)*
     . dp6(2.,0.,0.,2.)
      h2c(2.,0.,1.,0.)=dp5(2.,0.,1.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(2.,0.,0.,0.)*dp6(0.,0.,1.,0.)+dp5(1.,0.,1.,0.)*
     . dp6(1.,0.,0.,0.)+dp5(1.,0.,0.,0.)*dp6(1.,0.,1.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(2.,0.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(2.,0.,1.,0.)
      h2c(2.,0.,1.,1.)=dp5(2.,0.,1.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(2.,0.,1.,0.)*dp6(0.,0.,0.,1.)+dp5(2.,0.,0.,1.)*
     . dp6(0.,0.,1.,0.)+dp5(2.,0.,0.,0.)*dp6(0.,0.,1.,1.)+
     . dp5(1.,0.,1.,1.)*dp6(1.,0.,0.,0.)+dp5(1.,0.,1.,0.)*
     . dp6(1.,0.,0.,1.)+dp5(1.,0.,0.,1.)*dp6(1.,0.,1.,0.)+
     . dp5(1.,0.,0.,0.)*dp6(1.,0.,1.,1.)+dp5(0.,0.,1.,1.)*
     . dp6(2.,0.,0.,0.)+dp5(0.,0.,1.,0.)*dp6(2.,0.,0.,1.)+
     . dp5(0.,0.,0.,1.)*dp6(2.,0.,1.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(2.,0.,1.,1.)
      h2c(2.,0.,2.,0.)=dp5(2.,0.,2.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(2.,0.,1.,0.)*dp6(0.,0.,1.,0.)+dp5(2.,0.,0.,0.)*
     . dp6(0.,0.,2.,0.)+dp5(1.,0.,2.,0.)*dp6(1.,0.,0.,0.)+
     . dp5(1.,0.,1.,0.)*dp6(1.,0.,1.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(1.,0.,2.,0.)+dp5(0.,0.,2.,0.)*dp6(2.,0.,0.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(2.,0.,1.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(2.,0.,2.,0.)
      h2c(2.,1.,0.,0.)=dp5(2.,1.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(2.,0.,0.,0.)*dp6(0.,1.,0.,0.)+dp5(1.,1.,0.,0.)*
     . dp6(1.,0.,0.,0.)+dp5(1.,0.,0.,0.)*dp6(1.,1.,0.,0.)+
     . dp5(0.,1.,0.,0.)*dp6(2.,0.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(2.,1.,0.,0.)
      h2c(2.,1.,0.,1.)=dp5(2.,1.,0.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(2.,1.,0.,0.)*dp6(0.,0.,0.,1.)+dp5(2.,0.,0.,1.)*
     . dp6(0.,1.,0.,0.)+dp5(2.,0.,0.,0.)*dp6(0.,1.,0.,1.)+
     . dp5(1.,1.,0.,1.)*dp6(1.,0.,0.,0.)+dp5(1.,1.,0.,0.)*
     . dp6(1.,0.,0.,1.)+dp5(1.,0.,0.,1.)*dp6(1.,1.,0.,0.)+
     . dp5(1.,0.,0.,0.)*dp6(1.,1.,0.,1.)+dp5(0.,1.,0.,1.)*
     . dp6(2.,0.,0.,0.)+dp5(0.,1.,0.,0.)*dp6(2.,0.,0.,1.)+
     . dp5(0.,0.,0.,1.)*dp6(2.,1.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(2.,1.,0.,1.)
      h2c(2.,1.,1.,0.)=dp5(2.,1.,1.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(2.,1.,0.,0.)*dp6(0.,0.,1.,0.)+dp5(2.,0.,1.,0.)*
     . dp6(0.,1.,0.,0.)+dp5(2.,0.,0.,0.)*dp6(0.,1.,1.,0.)+
     . dp5(1.,1.,1.,0.)*dp6(1.,0.,0.,0.)+dp5(1.,1.,0.,0.)*
     . dp6(1.,0.,1.,0.)+dp5(1.,0.,1.,0.)*dp6(1.,1.,0.,0.)+
     . dp5(1.,0.,0.,0.)*dp6(1.,1.,1.,0.)+dp5(0.,1.,1.,0.)*
     . dp6(2.,0.,0.,0.)+dp5(0.,1.,0.,0.)*dp6(2.,0.,1.,0.)+
     . dp5(0.,0.,1.,0.)*dp6(2.,1.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(2.,1.,1.,0.)
      h2c(2.,2.,0.,0.)=dp5(2.,2.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(2.,1.,0.,0.)*dp6(0.,1.,0.,0.)+dp5(2.,0.,0.,0.)*
     . dp6(0.,2.,0.,0.)+dp5(1.,2.,0.,0.)*dp6(1.,0.,0.,0.)+
     . dp5(1.,1.,0.,0.)*dp6(1.,1.,0.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(1.,2.,0.,0.)+dp5(0.,2.,0.,0.)*dp6(2.,0.,0.,0.)+
     . dp5(0.,1.,0.,0.)*dp6(2.,1.,0.,0.)+dp5(0.,0.,0.,0.)*
     . dp6(2.,2.,0.,0.)
      h2c(3.,0.,0.,0.)=dp5(3.,0.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(2.,0.,0.,0.)*dp6(1.,0.,0.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(2.,0.,0.,0.)+dp5(0.,0.,0.,0.)*dp6(3.,0.,0.,0.)
      h2c(3.,0.,0.,1.)=dp5(3.,0.,0.,1.)*dp6(0.,0.,0.,0.)+
     . dp5(3.,0.,0.,0.)*dp6(0.,0.,0.,1.)+dp5(2.,0.,0.,1.)*
     . dp6(1.,0.,0.,0.)+dp5(2.,0.,0.,0.)*dp6(1.,0.,0.,1.)+
     . dp5(1.,0.,0.,1.)*dp6(2.,0.,0.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(2.,0.,0.,1.)+dp5(0.,0.,0.,1.)*dp6(3.,0.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(3.,0.,0.,1.)
      h2c(3.,0.,1.,0.)=dp5(3.,0.,1.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(3.,0.,0.,0.)*dp6(0.,0.,1.,0.)+dp5(2.,0.,1.,0.)*
     . dp6(1.,0.,0.,0.)+dp5(2.,0.,0.,0.)*dp6(1.,0.,1.,0.)+
     . dp5(1.,0.,1.,0.)*dp6(2.,0.,0.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(2.,0.,1.,0.)+dp5(0.,0.,1.,0.)*dp6(3.,0.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(3.,0.,1.,0.)
      h2c(3.,1.,0.,0.)=dp5(3.,1.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(3.,0.,0.,0.)*dp6(0.,1.,0.,0.)+dp5(2.,1.,0.,0.)*
     . dp6(1.,0.,0.,0.)+dp5(2.,0.,0.,0.)*dp6(1.,1.,0.,0.)+
     . dp5(1.,1.,0.,0.)*dp6(2.,0.,0.,0.)+dp5(1.,0.,0.,0.)*
     . dp6(2.,1.,0.,0.)+dp5(0.,1.,0.,0.)*dp6(3.,0.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(3.,1.,0.,0.)
      h2c(4.,0.,0.,0.)=dp5(4.,0.,0.,0.)*dp6(0.,0.,0.,0.)+
     . dp5(3.,0.,0.,0.)*dp6(1.,0.,0.,0.)+dp5(2.,0.,0.,0.)*
     . dp6(2.,0.,0.,0.)+dp5(1.,0.,0.,0.)*dp6(3.,0.,0.,0.)+
     . dp5(0.,0.,0.,0.)*dp6(4.,0.,0.,0.)
      return
      end