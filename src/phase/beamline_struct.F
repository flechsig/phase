c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/beamline_struct.F
c$$$ Date      : <04 Apr 12 11:54:45 flechsig> 
c$$$ Time-stamp: <04 Apr 12 13:26:33 flechsig> 
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

c----- main structure BeamlineType -----
      structure/fBeamlineType/
          record/felementtype/elementlist(1) 
          record/frtsourcetype/rtsource
          FMAP70TYPE lmap, rmap, M_StoI, M_ItoS;                          
          FMAP7TYPE ypc1, zpc1, dypc, dzpc;
          FMAP7TYPE wc, xlc, fdetc, fdetphc, fdet1phc, fdet1phca, fdet1phcb;
          record/fxlenmaptype/xlm 
          record/fRayType/raysout(1) 
          record/fRESULTType/RESULT
          unsigned int beamlineOK, elementzahl, position, hormapsloaded, localalloc 
          record/fOptionsType/BLOptions
          real*8 deltalambdafactor, xlen0
         record/fsources/src;
         record/fTmpMapType/tp(1)
         record/fPHASEset/filenames
      end structure 

      structure/felementtype/
      end structure

      structure/fgeometrytype/
         real*8 sina,cosa,sinb,cosb,
     &        r,rp,xdens(0:4),xlam
         integer idefl   
      end structure 

      structure/foptionstype/
      end structure

      structure/fPHASEset/
      end structure

      structure/fraytype/
      end structure

      structure/fresulttype/
      end structure

      structure/frtsourcetype/
      end structure

      structure/fsources/
      end structure

      structure/ftmpmaptype/
      end structure

      structure/fxlenmaptype/
      end structure
      
c end beamline_struct.F
