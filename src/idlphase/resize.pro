;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <12 Sep 13 11:40:57 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro resize,  y_vec=y_vec, z_vec=z_vec, field=field, Ninter=Ninter, Nzero=Nzero, nocenter=nocenter     
;          
;+
; NAME:
;
;
; PURPOSE:
;   Interpolate between residuals
;   Zero pad  to size to Nzero
;   keep pattern centered
;   we assume equidistant stepzize, and a quadratic image (nx=ny)
;
; CATEGORY:
;   phase_calc
;
;
; CALLING SEQUENCE:
;  
;
;
; INPUTS:
;   
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;   field:      input field, idl complex array,
;   y_vec:      input vector vertical, (required) in m
;   z_vec:      input vector horizontal (required) in m
;   Ninter:     Interpolate Ninter - fold  ,  e.g. Ninter = 2 : two
;                                    times
;   nocenter:   do not center the field 
;   Nzero:      enlarge field to size Nzero and fill with 0
;
; OUTPUTS:
;   see keyword parameters
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;   no
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
; PROCEDURE:
;   
;
;
; EXAMPLE:
;   
;
;
; MODIFICATION HISTORY:
;    27.8.2013 RF 
;    Sep 13 UF add nocenter, simplify zeropadding assuming equidistant steps
;-                 


u1    = 'usage: resize, y_vec=y_vec, z_vec=z_vec, field=field, Ninter=Ninter, Nzero=Nzero '    
usage = u1

print, '------------------ resize called ----------------------------'

if n_elements(z_vec)  eq 0 then begin print, usage & return & endif
if n_elements(y_vec)  eq 0 then begin print, usage & return & endif
if n_elements(field)  eq 0 then begin print, usage & return & endif
if n_elements(Ninter) eq 0 then begin Ninter = 0 & endif
if n_elements(Nzero)  eq 0 then begin Nzero  = 0 & endif

print, '------------------resize start caculation -----------------'

size = size(field)

if (size[1] ge size[2]) then sizeField = size[1] else sizeField = size[2]

if (Nzero ne 0) then begin
    help, field
    print, ' Nzero = ',Nzero, ' sizeField = ',sizeField
    
    if (Nzero ge sizeField) then begin
        print, '----------------- Zero padding of field ------', Nzero

        Null       = dcomplexarr(NZero, NZero)  ;; make a quadratic array filled with 0
        s          = size(field, /DIM)          ;; s gives the dimensions of field
        zshift     = (NZero-s[0])/2             ;; the index to shift 
        yshift     = (Nzero-s[1])/2             ;; the index to shift
        Null[0,0]  = field                      ;; copy original field  
        if n_elements(nocenter) eq 0 then shift(Null, zshift, yshift) ;; shift the field to the center
        field      = Null

;; now the vectors
        z0 = z_vec[0]                           ;; the start value
        y0 = y_vec[0]                           ;; the start value
        dz = z_vec[1] - z0                      ;; stepsize
        dy = y_vec[1] - y0                      ;; stepsize
        
        if n_elements(nocenter) eq 0 then begin ;; center
            z_vec= (dindgen(NZero)- zshift) * dz + z0
            y_vec= (dindgen(NZero)- yshift) * dy + y0 
        endif else begin
            z_vec= dindgen(NZero) * dz + z0
            y_vec= dindgen(NZero) * dy + y0
        endelse

    endif  ;; Nzero ge sizeField
endif  ;; Nzero ge 0

;-----------------------------------------------  interpolate

if (Ninter ne 0 ) then begin 
    s     = size(field, /DIM)        ;; s gives the dimensions of field
    print, 'Dim of zero padded array = ', s, ' Interpolate by ', Ninter
    
    x= dindgen(Ninter *(S[0]) +1) / Ninter   ;; UF ?? Ninter * S[0} - 1
    y= dindgen(Ninter *(S[1]) +1) / Ninter 
    field= interpolate(field, x, y, /GRID)
    
    z_vec= interpolate(z_vec, x, /GRID)
    y_vec= interpolate(y_vec, y, /GRID)
    
END

print, '------------------ resize end ----------------------------'

return
end
;; end
