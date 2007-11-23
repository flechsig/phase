;$Id$
;
; Copyright (c) 2002-2003, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;

;+
; NAME:
;	GET_CALLEXT_EXLIB
;
; PURPOSE:
;	Return the name of the sharable library containing the external
;	C code used by the IDL CALL_EXTERNAL examples. If necessary,
;	builds the library.
;
; CATEGORY:
;	Dynamic Linking Examples
;
; CALLING SEQUENCE
;	LibName = GET_CALL_EXAMPLES
;
; INPUTS:
;	None
;
; OUTPUTS:
;	Scalar string containing path to IDL CALL_EXTERNAL
;	examples sharable library.
;
; KEYWORDS:
;	VERBOSE
;	If set, cause the underlying MAKE_DLL to show the commands it
;	executes to build the sharable library, and all of the output
;	produced by those commands. If not set, this routine does its
;	work silently.
;
; SIDE EFFECTS
;	On the first call in an IDL session, the sharable library is
;	build using the MAKE_DLL procedure.
;
; RESTRICTIONS
;	For MAKE_DLL to be successful, a C compiler compatible with the
;	one described in the !MAKE_DLL system variable must be installed
;	on the system.
;
; PROCEDURE:
;	Caches the library path in a private COMMON block. On the first
;	call, builds the library and sets its name in the COMMON block for
;	following calls.
;
; Modification History:
;	AB, 11 April 2002
;-

function get_callext_exlib, VERBOSE=verbose
  common GET_CALLEXT_EXLIB_BLK, shlib

  ; Build sharable library if this is the first call or lib doesn't exist
  build_lib = n_elements(shlib) eq 0
  if (not build_lib) then build_lib = not FILE_TEST(shlib, /READ)
  if (build_lib) then begin
    ; Location of the CALL_EXTERNAL files from the IDL distribution
    call_ex_dir = FILEPATH('', SUBDIRECTORY=[ 'external','call_external','C' ])

    ; Use MAKE_DLL to build the widget_call_ex sharable library in the
    ; !MAKE_DLL.COMPILE_DIRECTORY directory.
    ;
    ; Normally, you wouldn't use VERBOSE, or SHOW_ALL_OUTPUT once your
    ; work is debugged, but as a learning exercize it can be useful to
    ; see all the underlying work that gets done. If the user specified
    ; VERBOSE, then use those keywords to show what MAKE_DLL is doing.
    source = [ 'phaBLInit','incr_struct', 'simple_vars', 'string_array', $
	       'sum_array', 'sum_2d_array' ]
    export_rtns = [ 'phaBLInit','incr_struct_natural',  'incr_struct', $
		    'simple_vars_natural',  'simple_vars', $
		    'string_array_natural', 'string_array', $
		    'sum_array_natural',    'sum_array', $
		    'sum_2d_array_natural', 'sum_2d_array' ]

    MAKE_DLL, source, 'call_external_examples', export_rtns, $
	  INPUT_DIR=call_ex_dir, DLL_PATH=shlib, $
	  VERBOSE=verbose, SHOW_ALL_OUTPUT=verbose
  endif

  return, shlib
end
