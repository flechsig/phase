% -*-latex-*-
% in "hyperbola_goldberg2d.red";
%---------------------------------------------------------------
%            
%    Berechnung der Oberflaechenfunktion eines Hyperboloids 
%    nach goldberg
%
%_______________________________________________________________
% resetreduce;
% UF allfac: ausklammern wenn moeglich 
on allfac;
a := (q-p)/2;
d := (p+q)/2;
val1 := sin(theta) * (p*q + d * cos(theta) * w);
val2 := sin(theta)**2 * p*q * (p*q + 2*d*cos(theta) * w + w**2);
val3 := (cos(theta)**2 * d**2 - p*q) * l**2;
val4 := sin(theta)**2 * p*q/a - cos(theta)**2 * a;
u := (-1)*(val1 - sqrt(val2 - val3)) / val4;
let cos(theta)^2 = 1- sin(theta)^2;

%let p*q/(q-p)=f;
%a20 := df(u,w,2,l,0)/2;
%a02 := df(u,w,0,l,2)/2;
%a12 := df(u,w,1,l,2)/2;
%a22 := df(u,w,2,l,2)/4;
%a04 := df(u,w,0,l,4)/24;
%a14 := df(u,w,1,l,4)/24;
%a32 := df(u,w,3,l,2)/12;
%a50 := df(u,w,5,l,0)/120;
%a60 := df(u,w,6,l,0)/720;
%a42 := df(u,w,4,l,2)/48;
%a06 := df(u,w,0,l,6)/720;
a70 := df(u,w,7,l,0)/5040;  
% 70 geht nicht wegen heap overflow versuche -td 10000
%a80 := df(u,w,8,l,0)/40320;

let w=0;
let l=0;
let abs(p)=p;
let abs(q)=q;
%let abs(p*q)=p*q;
let abs(sin(theta))=sin(theta);


%a20;
%let p*q/(q-p)=f;
%a02;

let cos(theta)= cost;
let sin(theta)= sint;
let p*q= pq;
let abs(sint)= sint;
let abs(cost)= cost;
let abs(pq)=pq;

off echo$
on fort$
out "hyperbola_goldberg2d_coeff.f"$
write "      coefficients aij";
write "      cost= cos(theta)";
write "      sint= sin(theta)";
write "      pq= p * q";
%write a20:=a20;
%write a02:=a02;
%write a12:=a12;
%write a22:=a22;
%write a04:=a04;
%write a14:=a14;
%write a32:=a32;
%write a50:=a50;
%write a60:=a60;
%write a42:=a42;
%write a06:=a06;
write a70:=a70;
%write a80:=a80;

shut "hyperbola_goldberg2d_coeff.f"$
off fort$
on echo$
