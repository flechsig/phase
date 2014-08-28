 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/common.h */
 /* Date      : <28 Aug 14 16:43:59 flechsig>  */
 /* Time-stamp: <28 Aug 14 16:47:14 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */
 
// ******************************************************************************
//
//   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
//                      Paul Scherrer Institut Villigen, Switzerland
//   
//   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
//          Uwe Flechsig,    uwe.flechsig@psi.ch
//
// ------------------------------------------------------------------------------
//
//   This file is part of PHASE.
//
//   PHASE is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, version 3 of the License, or
//   (at your option) any later version.
//
//   PHASE is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
//
// ******************************************************************************

#ifndef SIC_COMMON_H
#define SIC_COMMON_H 1

#if HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stdio.h>
#include <sys/types.h>

#if STDC_HEADERS
#  include <stdlib.h>
#  include <string.h>
#elif HAVE_STRINGS_H
#  include <strings.h>
#endif /*STDC_HEADERS*/

#if HAVE_UNISTD_H
#  include <unistd.h>
#endif

#if HAVE_ERRNO_H
#  include <errno.h>
#endif /*HAVE_ERRNO_H*/
#ifndef errno
/* Some systems #define this! */
extern int errno;
#endif


#ifdef __cplusplus
#  define BEGIN_C_DECLS         extern "C" {
#  define END_C_DECLS           }
#else
#  define BEGIN_C_DECLS
#  define END_C_DECLS
#endif

#define XCALLOC(type, num)                                  \
        ((type *) xcalloc ((num), sizeof(type)))
#define XMALLOC(type, num)                                  \
        ((type *) xmalloc ((num) * sizeof(type)))
#define XREALLOC(type, p, num)                              \
        ((type *) xrealloc ((p), (num) * sizeof(type)))
#define XFREE(stale)                            do {        \
        if (stale) { free (stale);  stale = 0; }            \
                                                } while (0)

BEGIN_C_DECLS

extern void *xcalloc    (size_t num, size_t size);
extern void *xmalloc    (size_t num);
extern void *xrealloc   (void *p, size_t num);
extern char *xstrdup    (const char *string);
extern char *xstrerror  (int errnum);

END_C_DECLS





#endif /* !SIC_COMMON_H */


