;  File      : /afs/psi.ch/user/f/flechsig/tmp/X/OL/App/lib/X_OL_IDL/updatehelp.pro
;  Date      : <04 Aug 05 16:43:01 flechsig> 
;  Time-stamp: <18 Jan 08 17:01:40 flechsig> 
;  Author    : Uwe Flechsig, flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

;; idl procedure to update all help files
;; usage: 1) go to idl derectory in your cvs copy
;;           $ cd ~/phase/src/idlphase
;;        2) start idl
;;           $ idl
;;        3) compile the script
;;           IDL> .r updatehelp
;;        4) run the script
;;           IDL> updatehelp
;;        5) install the update
;;           IDL> $ make install
;;
;; !! this program should not be installed in the idl searchpath !!

pro updatehelp

pathlist= ['']

spawn, 'pwd', start

for i=0, n_elements(pathlist)- 1 do begin
    path= start + pathlist[i]
    helpfile= path+'/help.html'
    print, 'make helpfile: ', helpfile
    my_mk_html_help, path, helpfile 
endfor

print, 'done -> help files updated!'

return
end
;; end 
