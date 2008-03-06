c     phase4idl_fortran_master.f

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c     MASTER-ifort-File  for lib_phase4idl.so
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c     All used Subroutines ...    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
      include 'pha_helper_functions.f'
      include 'pha_SrcWFGauss.f'
      include 'pha_Drift.f'
c      include 'save_me.f'
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c         pha_driftroutines.for ist bereits als 
c         "fp/include/ATnT_port3_driftroutines.o" kompiliert ...
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
     
c     Test-Program
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
      include 'test_phase.f'
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
    