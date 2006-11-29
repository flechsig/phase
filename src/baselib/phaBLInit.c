/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasecommon/phaBLInit.c */
/*  Date      : <06 Mar 06 16:14:16 flechsig>  */
/*  Time-stamp: <13 Mar 06 08:51:11 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#include "../phase/common.h"
#include "../phase/mirrorpck.h"
#include "../phase/geometrypck.h"
#include "../phase/phase_struct.h"

#include "phabasedefs.h"
#include "phabasestructs.h"
#include "idl_export.h"

/*
  IDL wrapper for phaBLInit
  allocate the memory for a struct BeamlineType
  ! returns the Adress of the allocated memory as IDL_LONG !
  obviously IDL does not like to work directly with pointers
  which were allocated by c
*/
IDL_LONG phaBLInitIDL()
{
   return (IDL_LONG)xmalloc(sizeof(struct BeamlineType));
}

/*
  alternative phaBLInit
*/
void *phaBLInitf()
{
   return xmalloc(sizeof(struct BeamlineType));
}


/*
   return a pointer to an empty beamline struct
*/
int phaBLInit(void **blptr)
{
  void *zeiger;
  zeiger= xmalloc(sizeof(struct BeamlineType));
  *blptr= zeiger;
  return 1;
}

/* end */
