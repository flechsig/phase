
;
; AUTHOR :  Torsten.Leitner@email.de
;     (c) 2007/2008
;
; FILE : phaBeamline.pro
;
; PRODCEDURES :
;                 phaAddOptElement
;                 phaWriteBLFile
;
; FUNCTIONS :
;                 phaNewBeamline
;                 phaReadBLFile
;


;+
; NAME:
;	phaNewBeamline
;
; PURPOSE:
;       Init new beamline structure
;
; CATEGORY:
;	func : pha4idl - beamline
;
; CALLING SEQUENCE:
;	beamline = phaNewBeamline(blfname)
;
; INPUTS:
;     	blfname:	name of the beamline(-file)
;
; OUTPUTS:
;     	beamline:	pha4idl beamline structure (see phainit_structures.pro)
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      March 28, 2008, TL, added help
;
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewBeamline, blfname
;input        -none-
;output     bl : struct pha4idlBeamlineType

if n_params() eq 0 then blfname=string('Unnamed')

bl          = { pha4idlBeamlineType }
bl.blfname = blfname
bl.fnamesrc4ezre = blfname+'-ezrec'
bl.fnamesrc4ezim = blfname+'-ezimc'
bl.fnamesrc4eyre = blfname+'-eyrec'
bl.fnamesrc4eyim = blfname+'-eyimc'
bl.fnamesrc6 = blfname+'-so6'

; defaultvalues
bl.raynumber = 225



return,bl
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;+
; NAME:
;	phaAddOptElement, bl, OptElement 
;
; PURPOSE:
;       Add new optical element to beamline
;
; CATEGORY:
;	pro : pha4idl - beamline
;
; CALLING SEQUENCE:
;	AddOptElement, bl, OptElement
;
; INPUTS:
;     	bl:		beamline structure
;	OptElement	OptElement structure to add to the beamline
;
; OUTPUTS:
;     	bl:		beamline structure with new opt. element
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      March 28, 2008, TL, added help
;
;-

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



;+
; NAME:
;	phaReadBLFile
;
; PURPOSE:
;       Read beamline from file
;
; CATEGORY:
;	func : pha4idl - beamline
;
; CALLING SEQUENCE:
;	beamline = phaReadBLFile(blfname)
;
; INPUTS:
;     	blfname:	name of the beamlinefile
;
; OUTPUTS:
;     	beamline:	pha4idl beamline structure (see phainit_structures.pro)
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      March 28, 2008, TL, added help
;
;-

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


;+
; NAME:
;	phaWriteBLFile
;
; PURPOSE:
;       Write beamline to file
;
; CATEGORY:
;	pro : pha4idl - beamline
;
; CALLING SEQUENCE:
;	beamline = phaWriteBLFile(blfname)
;
; INPUTS:
;     	blfname:	name of the beamlinefile
;
; OUTPUTS:
;     	beamlinefile on hdd named 'blfname'
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      March 28, 2008, TL, added help
;
;-

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



