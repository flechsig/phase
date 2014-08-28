/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/error.c */
/*  Date      : <26 Oct 04 13:49:18 flechsig>  */
/*  Time-stamp: <28 Aug 14 16:50:22 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@psi.ch */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

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


#if HAVE_CONFIG_H
#include "config.h"
#endif

#include "error.h"

static void error (int exit_status, const char *mode, 
                   const char *message);

static void
error (int exit_status, const char *mode, const char *message)
{
  fprintf (stderr, "%s: %s.\n", mode, message);

  if (exit_status >= 0)
    exit (exit_status);
}

void
sic_info (const char *message)
{
  error (-1, "info", message);
}

void
sic_warning (const char *message)
{
  error (-1, "warning", message);
}

void
sic_error (const char *message)
{
  error (-1, "ERROR", message);
}

void
sic_fatal (const char *message)
{
#ifndef QTGUI
  error (EXIT_FAILURE, "FATAL", message);
#endif
}

//const char *program_name = NULL;

/* failed with the posix version of basename */
/* UF 25.12.07 apply basename to a copy      */
/*void set_program_name (const char *path)
{
  char *pathcopy;
#ifndef QTGUI  
  pathcopy= xstrdup(path);
  /*  if (!program_name) */
/*  program_name = (const char *)basename(pathcopy);
#endif
} */


