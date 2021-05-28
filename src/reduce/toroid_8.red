% -*-latex-*-
%---------------------------------------------------------------
%            
%    Berechnung der Oberflaechenfunktion eines Toroids 
%
%_______________________________________________________________
% UF 210518: take file TOROID_8.PAS from J. Bahrdt
% convert it to unix with dos2unix - add some comments

order w,l,yi,zi,yp,zp,yp2,zp2,yp3,zp3;
factor w,l,yi,zi,yp,zp,yp2,zp2,yp3,zp3;
off allfac;

operator dq0,dq1,dq2,
         dq00,dq11,dq22;

%---------------------------------------------------------
% order of mirror coordinates
%---------------------------------------------------------
ord:=8;
%---------------------------------------------------------

%u:=R-sqrt((R-rho+sqrt(rho**2-l**2))**2-w**2);
%u:=R-sqrt(c0+c1*sqrt(rho**2-l**2)-w**2-l**2);
u:=R-sqrt(c0+c1*sqrt(rho**2-l2)-w2-l2);

      FOR N1 := 0:ord/2
DO << FOR N2 := 0:ord/2-N1
DO << a1(n1,n2):=sub(w2=0,l2=0,df(u,w2,n1,l2,n2))/
		((FOR J:=1:N1 PRODUCT J)*
		 (FOR J:=1:N2 PRODUCT J));
>> >>;

%------------- OUTPUT ----------------------------------------

let abs(rho)=rho;
let abs(r)=r;

let c0=r**2-2*r*rho+2*rho**2;
let c1=2*(r-rho);

off echo$
on fort$
out "toroid_8.for"$
write "      subroutine toroid_8(R,rho,a)";
write "      implicit real*8(a-h,o-z)";
write "      dimension a(0:8,0:8)";
begin
integer n1,n2;
for n1:=0:ord/2
 do << for n2:=0:ord/2-n1
 do << 
 write a(2*n1,2*n2):=a1(n1,n2);
>> >>;
end;
write "      return";
write "      end";
shut "toroid_8.for"$
off fort$
on echo$

