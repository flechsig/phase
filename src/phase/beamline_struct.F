c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/beamline_struct.F
c$$$ Date      : <04 Apr 12 11:54:45 flechsig> 
c$$$ Time-stamp: <04 Apr 12 13:40:21 flechsig> 
c$$$ Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c----------------------------------------------------------------
c                define the beamline struct in fortran
c                use same name as in phase.h with a leading "f"
c                sort substructures in alphabetic order
c----------------------------------------------------------------

#define FMAP70TYPE real*8
#define FMAP7TYPE  real*8


c-----main structure BeamlineType -----
      structure/fBeamlineType/
         record/felementtype/elementlist(1) 
         record/frtsourcetype/rtsource
         FMAP70TYPE lmap, rmap, M_StoI, M_ItoS                          
         FMAP7TYPE ypc1, zpc1, dypc, dzpc
         FMAP7TYPE wc, xlc, fdetc, fdetphc, fdet1phc, fdet1phca, fdet1phcb
         record/fxlenmaptype/xlm 
         record/fRayType/raysout(1) 
         record/fRESULTType/RESULT
c     unsigned int beamlineOK, elementzahl, position, hormapsloaded, localalloc
         integer beamlineOK, elementzahl, position, hormapsloaded, localalloc
         record/fOptionsType/BLOptions
         real*8 deltalambdafactor, xlen0
         record/fsources/src
         record/fTmpMapType/tp(1)
         record/fphaseset/filenames
      end structure 

      structure/felementtype/
         integer dummy
      end structure
      
      structure/fgeometrytype/
         real*8 sina,cosa,sinb,cosb,
     &        r,rp,xdens(0:4),xlam
         integer idefl   
      end structure 
      
      structure/foptionstype/
         integer dummy
      end structure
      
      structure/fphaseset/
         integer dummy
      end structure
      
      structure/fraytype/
         integer dummy
      end structure
      
      structure/fresulttype/
         integer dummy
      end structure
      
      structure/frtsourcetype/
         integer dummy
      end structure
      
      structure/fsources/
         integer dummy
      end structure
      
      structure/ftmpmaptype/
         integer dummy
      end structure
      
      structure/fxlenmaptype/
         integer dummy
      end structure
      
 c     end beamline_struct.F
      
