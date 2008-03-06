

;;; FILE:  phaBeamline.pro




PRO phaAddOptElement, bl, OptElement 

;bl         = { pha4idlBeamlineType }
;OptElement = { pha4idlOptElement }


; Set NAME of new element
bl.ElementList(bl.NumElements).elementname=byte(OptElement.name)
; Set NAME of mirror data
bl.ElementList(bl.NumElements).MDat       =OptElement.MDat
; Set NAME of geometry
bl.ElementList(bl.NumElements).GDat       =OptElement.GDat
; increment num of element
bl.NumElements++


END




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaReadBLFile, blfname

bl = { pha4idlBeamlineType }

bl.blfname = blfname


lib = !phalib
func= 'pha4idlReadBLFile'

result = 1
result = call_external(lib,func,$
		 bl.blfname , bl , $ ; elno , $; test, $ ; Variablelist                      
		/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)
;print,'error code : ',result

bl.fnamesrc4ezre = pha4idlByteArray2String(bl.src.so4.fsource4a)
bl.fnamesrc4ezim = pha4idlByteArray2String(bl.src.so4.fsource4b)
bl.fnamesrc4eyre = pha4idlByteArray2String(bl.src.so4.fsource4c)
bl.fnamesrc4eyim = pha4idlByteArray2String(bl.src.so4.fsource4d)
bl.fnamesrc6 = pha4idlByteArray2String(bl.src.so6.fsource6)


return,bl
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaWriteBLFile, bl
;  bl = { pha4idlBeamlineType }

lib = !phalib
func= 'pha4idlWriteBLFile'

;np=n_params()
;if np             eq 0 then fname='unnamed.phase'
;if strlen(fname) eq 0 then fname='unnamed.phase'

; dieser offset wird benoetigt, und zwar exakt dieser!
; warum? keine ahnung ... bleibt zu klaeren
offset=32

bl.src.so4.fsource4a = bytarr(80) ;; Null setzen
;tmp=byte(bl.fnamesrc4ezre)
;sz=((size(byte(bl.fnamesrc4ezre)))(1))
bl.src.so4.fsource4a(offset:offset+((size(byte(bl.fnamesrc4ezre)))(1))-1) = byte(bl.fnamesrc4ezre);tmp

bl.src.so4.fsource4b = bytarr(80) ;; Null setzen
bl.src.so4.fsource4b(offset:offset+((size(byte(bl.fnamesrc4ezim)))(1))-1) = byte(bl.fnamesrc4ezim)

bl.src.so4.fsource4c = bytarr(80) ;; Null setzen
bl.src.so4.fsource4c(offset:offset+((size(byte(bl.fnamesrc4eyre)))(1))-1) = byte(bl.fnamesrc4eyre)

bl.src.so4.fsource4d = bytarr(80) ;; Null setzen
bl.src.so4.fsource4d(offset:offset+((size(byte(bl.fnamesrc4eyim)))(1))-1) = byte(bl.fnamesrc4eyim)

 bl.src.so6.fsource6 = bytarr(80) ;; Null setzen
 bl.src.so6.fsource6(offset:offset+((size(byte(bl.fnamesrc6)))(1))-1) = byte(bl.fnamesrc6)


result = 1
result = call_external(lib,func,$
		 bl.blfname , bl , $ ; elno , $; test, $ ; Variablelist                      
		/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)
;print,'error code : ',result



END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



