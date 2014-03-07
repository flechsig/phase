/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/error.h */
/*  Date      : <26 Oct 04 13:48:42 flechsig>  */
/*  Time-stamp: <26 Oct 04 13:48:47 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@psi.ch */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef SIC_ERROR_H
#define SIC_ERROR_H 1

#include <common.h>

BEGIN_C_DECLS

extern const char *program_name;
extern void set_program_name (const char *argv0);

extern void sic_info         (const char *message);
extern void sic_warning      (const char *message);
extern void sic_error        (const char *message);
extern void sic_fatal        (const char *message);

END_C_DECLS

#endif /* !SIC_ERROR_H */

