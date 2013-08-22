c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase4idl/pha4idlFORTRANdefines.h.f
c$$$ Date      : <16 Aug 13 09:36:19 flechsig> 
c$$$ Time-stamp: <22 Aug 13 14:30:25 flechsig> 
c$$$ Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

cc /// pha4idlFORTRANdefines.h.f

c UF: by including this file the fortran routines with _nostructs become independent from the c- code- the field dimensions are statically defined and not via configure

cc MaxDim is the Dimension of the fieldsw in the src4-structure
#define MaxDim 2048
cc BigMaxDim is for internal temporary fields
#define BigMaxDim  2048
#define BIGMAXDIM2 4096
cc 6 * MaxDim = 12288
#define STACK4SPLINE 12288
cc 2*MaxDim*MaxDim+16 = 8388624
#define STACK4FFT 8388624
c end
