; Name: UsefullTools.pro


;;
PRO WaitForEnter
;; wait for until <ENTER> is pressed
tmp=''
read,tmp,prompt='Press enter to continue ... '
END



;;
FUNCTION pha4idlByteArray2String, ba
;; Converts a bytearray (char[] in C) to an idlstring of same length
;
; a=size(x)
; -> array:  a(0)=#dim - a(1)=dim_1 - a(i)=dim_i - a(#dim)=dim_#dim 
;         - a(#dim+1)= type code - a(#dim+2)= tot.elements in all dims

sdim=size(ba)

if sdim(0) ne 1 then begin 
            print,'*** error in function: ByteArray2String ***'
            print,'Only 1D bytearrays are supported, Dimension was : ', sdim(0)
            return, '*** error in function: ByteArray2String ***'
endif else begin
    slen=sdim(1)
endelse

if sdim(2) ne 1 then begin 
            print,'*** warning from function: ByteArray2String ***'
            print,'Input argument NOT of type bytearray.'
endif


;; kill all leading whitespaces
k=0
kstart=0
while k lt slen do begin
            kstart=k
            if ba(k) ne 0 then k=slen+1
            k=k+1
endwhile

s=replicate(' ',slen)

s=string( ba(kstart:slen-1) )

return, s
END

