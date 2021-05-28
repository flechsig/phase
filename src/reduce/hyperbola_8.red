% -*-latex-*-
% in "hyperbola_8.red";
%---------------------------------------------------------------
%            
%    Berechnung der Oberflaechenfunktion eines Hyperboloids 
%
%_______________________________________________________________
% UF 210518: take file TOROID_8.PAS from J. Bahrdt
% convert it to unix with dos2unix - add some comments
% add hyperbel spezifische inputs

% UF order of output variables
order w,l,yi,zi,yp,zp,yp2,zp2,yp3,zp3;
factor w,l,yi,zi,yp,zp,yp2,zp2,yp3,zp3;
% UF allfac: ausklammern wenn moeglich  
off allfac;

%---------------------------------------------------------
% order of mirror coordinates
%---------------------------------------------------------
ord:=7;
%---------------------------------------------------------
let abs(a0)=a0;
let abs(b0)=b0;
% Rechnung
% hyperbelformel mit a und b, a und b sind immer > 0
sigma0:= (w0/a0)^2-(u0/b0)^2-(l0/b0)^2-1.0;
% Verschiebung
w0:= w1- w00;
u0:= u1- u00;
% drehungs gleichungen einsetzen in die alten sachen
w1 :=  w * cos(phi) + u2 * sin(phi);
u1 := -w * sin(phi) + u2 * cos(phi);
l0 := l;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ergebnis sigma0 u(w,l)
u := rhs(second(solve(sigma0,u2)));  

% vereinfache wurzelausdruck
let sin(phi)^2 = 1- cos(phi)^2;
% vereinfache wurzelausdruck mit neuen konstanten
let -cos(phi)^2*a0^2-cos(phi)^2*b0^2+b0^2=l22;
let -2*cos(phi)*b0^2*w00+2*sin(phi)*b0^2*u00=w11;
let -cos(phi)*w00*w11+sin(phi)*u00*w11+2*b0^2*l22=c33;
let -sqrt(2)*cos(phi)*sin(phi)*a0^2-sqrt(2)*cos(phi)*sin(phi)*b0^2=w12;
let -sqrt(2)*cos(phi)*a0^2*u00+sqrt(2)*sin(phi)*b0^2*w00=c44;

% weiter mit code von Johannes
% matrix mit Koeffizienten a1
      FOR N1 := 0:ord
DO << FOR N2 := 0:ord-N1
DO << a1(n1,n2):=sub(w=0,l=0,df(u,w,n1,l,n2))/
		((FOR J:=1:N1 PRODUCT J)*
		 (FOR J:=1:N2 PRODUCT J));
>> >>;

%------------- OUTPUT ----------------------------------------

let cos(phi)= cosa;
let sin(phi)= sina;

off echo$
on fort$
out "hyperbola_8.F"$
write "      subroutine hyperbola_8(a0,b0,w00,u00,phi,a)";
write "      real*8 a0, b0, w00, u00, phi, a";
write "      real*8 cosa,sina,l22,w11,c33,w12";
write "      dimension a(0:8,0:8)";
write "      cosa= cos(phi)";
write "      sina= sin(phi)";
write "      l22=-cosa**2*(a0**2+b0**2)+b0**2";
write "      w11=2*b0**2*(u00*sina-w00*cosa)";
write "      c33=-cosa*w00*w11+sina*u00*w11+2*b0**2*l22";
write "      w12=-sqrt(2.0)*cosa*sina*a0**2-sqrt(2.0)*cosa*sina*b0**2";
write "      c44=-sqrt(2.0)*cosa*a0**2*u00+sqrt(2.0)*sina*b0**2*w00";
begin
integer n1,n2;
for n1:=0:ord
 do << for n2:=0:ord-n1
 do << 
 write a(n1,n2):=a1(n1,n2);
>> >>;
end;
write "      return";
write "      end";
shut "hyperbola_8.F"$
off fort$
on echo$

