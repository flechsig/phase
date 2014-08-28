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

void *
xmalloc (size_t num)
{
  void *new = malloc (num);
  if (!new)
    sic_fatal ("Memory exhausted");
  return new;
}

void *
xrealloc (void *p, size_t num)
{
  void *new;

  if (!p)
    return xmalloc (num);

  new = realloc (p, num);
  if (!new)
    sic_fatal ("Memory exhausted");

  return new;
}

void *
xcalloc (size_t num, size_t size)
{
  void *new = xmalloc (num * size);
  bzero (new, num * size);
  return new;
}

