#include "../phase/common.h"
#include "../phase/mirrorpck.h"
#include "../phase/geometrypck.h"
#include "../phase/phase_struct.h"


#include "phabasedefs.h"
#include "phabasestructs.h"
#include "idl_export.h"

#define PLrttype                0x400   /* 1024 */  
#define PLphspacetype           0x800   /* 2048 */ 
#define PLautoscale            0x1000   /* 4096 */


void FreeResultMem(struct RESULTType *Re)  
/* macht den Speicher frei */
/* uwe 8.8.96 */
{
   if (((Re->typ & PLrttype) > 0) && (Re->RESUnion.Rays != NULL))
      free(Re->RESUnion.Rays); 
   if (((Re->typ & PLphspacetype) > 0) && (Re->RESUnion.PSD.y != NULL))
   {  /* da der Speicher gemeinsam allociert wird teste ich nur einmal */
        free(Re->RESUnion.PSD.y);  
        free(Re->RESUnion.PSD.z);  
        free(Re->RESUnion.PSD.psd);  
   }
   Re->typ= 0;
} /* end freeResultmem */
