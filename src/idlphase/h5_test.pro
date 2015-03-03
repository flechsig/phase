;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/h5_test.pro
;  Date      : <18 Jul 13 09:04:13 flechsig> 
;  Time-stamp: <03 Mar 15 16:40:43 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

; ******************************************************************************
;
;   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
;                      Paul Scherrer Institut Villigen, Switzerland
;   
;   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
;          Uwe Flechsig,    uwe.flechsig@psi.ch
;
; ------------------------------------------------------------------------------
;
;   This file is part of PHASE.
;
;   PHASE is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, version 3 of the License, or
;   (at your option) any later version.
;
;   PHASE is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with PHASE (src/LICENSE).  If not, see <http:;www.gnu.org/licenses/>. 
;
; ******************************************************************************

function h5_test, fname, verbose=verbose
;+
; NAME:
;   h5_test
;
; PURPOSE:
;   test if hdf5 file
;
; CATEGORY:
;   hdf5
;
; CALLING SEQUENCE:
;
; INPUTS:
;   filename as string
;
; KEYWORD PARAMETERS:
;   /verbose
;
; OUTPUTS:
;   1 if hdf5 found else 0
;
; EXAMPLE:
;   idl> a=h5_test('abc.h5', /verbose)
;   idl> print, a
;
; MODIFICATION HISTORY:
;   UF Jul 2013
;-

if n_elements(fname) eq 0 then $
  fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.phase_hdf5.h5'
if n_elements(verbose) ne 0 then verbose=verbose else verbose=0

cmd= 'file -bL '+ fname
spawn, cmd, result
if result eq 'Hierarchical Data Format (version 5) data' then begin
    if verbose then print, '>>', fname, '<< is a hdf5 file' 
    ret= 1
endif else begin
    if verbose then print, '>>',fname, '<< is not a hdf5 file' 
    ret= 0
endelse

return, ret
end

