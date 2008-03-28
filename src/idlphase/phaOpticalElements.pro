
; file: phaOpticalElements.pro





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOptElement, name
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

if n_params() eq 0    then name=string('Unnamed')
if name eq string('') then name=string('Unnamed')


OptElement = { pha4idlOptElementType }
OptElement.name = name

return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaOptElementDefGeo, OptELement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut
      ;all contents of struct GDat - geometry

; Set struct GDat:{gdatset}
OptElement.GDat.theta0    = theta0
OptElement.GDat.r         = r
OptElement.GDat.rp        = rp
OptElement.GDat.xdens(0)  = xdens0
OptElement.GDat.xdens(1)  = xdens1
OptElement.GDat.xdens(2)  = xdens2
OptElement.GDat.xdens(3)  = xdens3
OptElement.GDat.xdens(4)  = xdens4
OptElement.GDat.lambda    = lambda
OptElement.GDat.inout     = inout
OptElement.GDat.iflag     = iflag
OptElement.GDat.azimut    = azimut

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaOptElementDefMDat, OptELement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art
      ;all contents of struct MDat - geometry

; set struct MDat:{mdatset}
OptElement.MDat.r1        = r1
OptElement.MDat.r2        = r2
OptElement.MDat.alpha     = alpha
OptElement.MDat.rmi       = rmi
OptElement.MDat.rho       = rho
OptElement.MDat.iflagmi   = iflagmi
OptElement.MDat.w1        = w1
OptElement.MDat.w2        = w2
OptElement.MDat.l1        = l1
OptElement.MDat.l2        = l2
OptElement.MDat.slopew    = slopew
OptElement.MDat.slopel    = slopel
OptElement.MDat.du        = du
OptElement.MDat.dw        = dw
OptElement.MDat.dl        = dl
OptElement.MDat.dRu       = dRu
OptElement.MDat.dRw       = dRw
OptElement.MDat.dRl       = dRl
OptElement.MDat.Art       = Art

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO phaDefineOpticalElement , OptElement $; returns: OptElement  pha4idlOptElement structure
 ;     ,ElementName $;
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut  $; struct GDat - geometry
      ,r1,r2,alpha,rmi,rho,iflagmi,w1,w2,l1,l2,slopew,slopel,du,dw,dl,dRu,dRw,dRl,Art ; struct MDat - mirror

; total arguments: 31

;OptElement = { pha4idlOptElementType }  ; 'zero' opt.element

np=n_params()

; Set NAME of new element
;OptElement.name = ElementName

; Set struct GDat:{gdatset}
phaOptElementDefGeo, OptELement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; set struct MDat:{mdatset}
phaOptElementDefMDat, OptELement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOECoefficientFile, name
; mirror from file
Art = !phaOptElGeneral
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType
OptElement = phaNewOptElement(name)
OptElement.MDat.Art = Art
return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEPlaneGrating, name, lambda, rsrc, rimg, theta, difforder, Ngrat,  $
                              du,dRu, dw,dRw, dl,dRl, wmin,wmax,wslope, lmin,lmax,lslope
Art = !phaOptElPlaneGrating
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

; define struct GDat:{gdatset}
theta0=double(theta)
r=double(rsrc)
rp=double(rimg)
xdens0=double(Ngrat)
xdens1=double(0)
xdens2=double(0)
xdens3=double(0)
xdens4=double(0)
lambda=double(lambda)
inout=long(difforder)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(rsrc)
r2=double(rimg)
alpha=double(theta)
rmi=double(0) 
rho=double(0)
iflagmi=long(0)
w1=double(wmin)
w2=double(wmax)
l1=double(lmin)
l2=double(lmax)
slopew=double(wslope)
slopel=double(lslope)
du=double(du)
dw=double(dw)
dl=double(dl)
dRu=double(dRu)
dRw=double(dRw)
dRl=double(dRl)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art

return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEPlaneVLSGrating, name, lambda, rsrc, rimg, theta, difforder,Ngrat, Ngrat1,Ngrat2,Ngrat3,Ngrat4, $
                              du,dRu, dw,dRw, dl,dRl, wmin,wmax,wslope, lmin,lmax,lslope
Art = !phaOptElPlaneVLSGrating
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

; define struct GDat:{gdatset}
theta0=double(theta)
r=double(rsrc)
rp=double(rimg)
xdens0=double(Ngrat)
xdens1=double(Ngrat1)
xdens2=double(Ngrat2)
xdens3=double(Ngrat3)
xdens4=double(Ngrat4)
lambda=double(lambda)
inout=long(difforder)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(rsrc)
r2=double(rimg)
alpha=double(theta)
rmi=double(0) 
rho=double(0)
iflagmi=long(0)
w1=double(wmin)
w2=double(wmax)
l1=double(lmin)
l2=double(lmax)
slopew=double(wslope)
slopel=double(lslope)
du=double(du)
dw=double(dw)
dl=double(dl)
dRu=double(dRu)
dRw=double(dRw)
dRl=double(dRl)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEPlaneMirror, name, lambda, rsrc, rimg, theta,  $
                              du,dRu, dw,dRw, dl,dRl, wmin,wmax,wslope, lmin,lmax,lslope
Art = !phaOptElPlaneMirror
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

; define struct GDat:{gdatset}
theta0=double(theta)
r=double(rsrc)
rp=double(rimg)
xdens0=double(0)
xdens1=double(0)
xdens2=double(0)
xdens3=double(0)
xdens4=double(0)
lambda=double(lambda)
inout=long(0)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(rsrc)
r2=double(rimg)
alpha=double(theta)
rmi=double(0)
rho=double(0)
iflagmi=long(0)
w1=double(wmin)
w2=double(wmax)
l1=double(lmin)
l2=double(lmax)
slopew=double(wslope)
slopel=double(lslope)
du=double(du)
dw=double(dw)
dl=double(dl)
dRu=double(dRu)
dRw=double(dRw)
dRl=double(dRl)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOESlit, name, lambda, wmin, wmax, lmin, lmax
Art = !phaOptElSlit
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

; define struct GDat:{gdatset}
theta0=double(0)
r=double(0)
rp=double(0)
xdens0=double(0)
xdens1=double(0)
xdens2=double(0)
xdens3=double(0)
xdens4=double(0)
lambda=double(lambda)
inout=long(0)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(0)
r2=double(0)
alpha=double(0)
rmi=double(0)
rho=double(0)
iflagmi=long(0)
w1=double(wmin)
w2=double(wmax)
l1=double(lmin)
l2=double(lmax)
slopew=double(0)
slopel=double(0)
du=double(0)
dw=double(0)
dl=double(0)
dRu=double(0)
dRw=double(0)
dRl=double(0)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEDrift, name, lambda, rsrc, rimg
Art = !phaOptElDrift
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

; define struct GDat:{gdatset}
theta0=double(0)
r=double(rsrc)
rp=double(rimg)
xdens0=double(0)
xdens1=double(0)
xdens2=double(0)
xdens3=double(0)
xdens4=double(0)
lambda=double(lambda)
inout=long(0)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(rsrc)
r2=double(rimg)
alpha=double(0)
rmi=double(0)
rho=double(0)
iflagmi=long(0)
w1=double(0)
w2=double(0)
l1=double(0)
l2=double(0)
slopew=double(0)
slopel=double(0)
du=double(0)
dw=double(0)
dl=double(0)
dRu=double(0)
dRw=double(0)
dRl=double(0)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEToroidalMirror, name, lambda, rsrc, rimg, theta,  $
                              du,dRu, dw,dRw, dl,dRl, wmin,wmax,wslope, lmin,lmax,lslope
Art = !phaOptElToroidalMirror
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

x=2*rsrc*rimg / (rsrc+rimg) 
y=cos(theta*3.14159265358979323846/180.0)
rmi = x/y
rho = x*y

; define struct GDat:{gdatset}
theta0=double(theta)
r=double(rsrc)
rp=double(rimg)
xdens0=double(0)
xdens1=double(0)
xdens2=double(0)
xdens3=double(0)
xdens4=double(0)
lambda=double(lambda)
inout=long(0)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(rsrc)
r2=double(rimg)
alpha=double(theta)
rmi=double(rmi)
rho=double(rho)
iflagmi=long(0)
w1=double(wmin)
w2=double(wmax)
l1=double(lmin)
l2=double(lmax)
slopew=double(wslope)
slopel=double(lslope)
du=double(du)
dw=double(dw)
dl=double(dl)
dRu=double(dRu)
dRw=double(dRw)
dRl=double(dRl)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEToroidalGrating, name, lambda, rsrc, rimg, theta,  $
			difforder,Ngrat, $
                      du,dRu, dw,dRw, dl,dRl, wmin,wmax,wslope, lmin,lmax,lslope
Art = !phaOptElToroidalGrating
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

x=2*rsrc*rimg / (rsrc+rimg) 
y=cos(theta*3.14159265358979323846/180.0)
rmi = x/y
rho = x*y

; define struct GDat:{gdatset}
theta0=double(theta)
r=double(rsrc)
rp=double(rimg)
xdens0=double(Ngrat)
xdens1=double(0)
xdens2=double(0)
xdens3=double(0)
xdens4=double(0)
lambda=double(lambda)
inout=long(difforder)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(rsrc)
r2=double(rimg)
alpha=double(theta)
rmi=double(rmi)
rho=double(rho)
iflagmi=long(0)
w1=double(wmin)
w2=double(wmax)
l1=double(lmin)
l2=double(lmax)
slopew=double(wslope)
slopel=double(lslope)
du=double(du)
dw=double(dw)
dl=double(dl)
dRu=double(dRu)
dRw=double(dRw)
dRl=double(dRl)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEToroidalVLSGrating,  name, lambda, rsrc, rimg, theta, $
	    difforder,Ngrat, Ngrat1,Ngrat2,Ngrat3,Ngrat4, $
            du,dRu, dw,dRw, dl,dRl, wmin,wmax,wslope, lmin,lmax,lslope
Art = !phaOptElToroidalVLSGrating
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

x=2*rsrc*rimg / (rsrc+rimg) 
y=cos(theta*3.14159265358979323846/180.0)
rmi = x/y
rho = x*y

; define struct GDat:{gdatset}
theta0=double(theta)
r=double(rsrc)
rp=double(rimg)
xdens0=double(Ngrat)
xdens1=double(Ngrat1)
xdens2=double(Ngrat2)
xdens3=double(Ngrat3)
xdens4=double(Ngrat4)
lambda=double(lambda)
inout=long(difforder)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(rsrc)
r2=double(rimg)
alpha=double(theta)
rmi=double(rmi)
rho=double(rho)
iflagmi=long(0)
w1=double(wmin)
w2=double(wmax)
l1=double(lmin)
l2=double(lmax)
slopew=double(wslope)
slopel=double(lslope)
du=double(du)
dw=double(dw)
dl=double(dl)
dRu=double(dRu)
dRw=double(dRw)
dRl=double(dRl)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOECone, name, lambda, rsrc, rimg, theta, $
	  du,dRu, dw,dRw, dl,dRl, wmin,wmax,wslope, lmin,lmax,lslope
Art = !phaOptElCone
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

x=2*rsrc*rimg / (rsrc+rimg) 
y=cos(theta*3.14159265358979323846/180.0)
rmi = x/y
rho = x*y

; define struct GDat:{gdatset}
theta0=double(theta)
r=double(rsrc)
rp=double(rimg)
xdens0=double(0)
xdens1=double(0)
xdens2=double(0)
xdens3=double(0)
xdens4=double(0)
lambda=double(lambda)
inout=long(0)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(rsrc)
r2=double(rimg)
alpha=double(theta)
rmi=double(rmi)
rho=double(rho)
iflagmi=long(0)
w1=double(wmin)
w2=double(wmax)
l1=double(lmin)
l2=double(lmax)
slopew=double(wslope)
slopel=double(lslope)
du=double(du)
dw=double(dw)
dl=double(dl)
dRu=double(dRu)
dRw=double(dRw)
dRl=double(dRl)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEEllipticalMirror, name, lambda, rsrc, rimg, theta, $
	  du,dRu, dw,dRw, dl,dRl, wmin,wmax,wslope, lmin,lmax,lslope
Art = !phaOptElEllipticalMirror
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

; define struct GDat:{gdatset}
theta0=double(theta)
r=double(rsrc)
rp=double(rimg)
xdens0=double(0)
xdens1=double(0)
xdens2=double(0)
xdens3=double(0)
xdens4=double(0)
lambda=double(lambda)
inout=long(0)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(0)
r2=double(0)
alpha=double(theta)
rmi=double(rmi)
rho=double(rho)
iflagmi=long(0)
w1=double(wmin)
w2=double(wmax)
l1=double(lmin)
l2=double(lmax)
slopew=double(wslope)
slopel=double(lslope)
du=double(du)
dw=double(dw)
dl=double(dl)
dRu=double(dRu)
dRw=double(dRw)
dRl=double(dRl)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEPlaneEllipticalMirror, name, lambda, rsrc, rimg, theta, $
	  du,dRu, dw,dRw, dl,dRl, wmin,wmax,wslope, lmin,lmax,lslope
Art = !phaOptElPlaneEllipticalMirror
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

; define struct GDat:{gdatset}
theta0=double(theta)
r=double(rsrc)
rp=double(rimg)
xdens0=double(0)
xdens1=double(0)
xdens2=double(0)
xdens3=double(0)
xdens4=double(0)
lambda=double(lambda)
inout=long(0)
iflag=long(0)
azimut=double(0)

; define struct MDat:{mdatset}
r1=double(0)
r2=double(0)
alpha=double(theta)
rmi=double(rmi)
rho=double(rho)
iflagmi=long(0)
w1=double(wmin)
w2=double(wmax)
l1=double(lmin)
l2=double(lmax)
slopew=double(wslope)
slopel=double(lslope)
du=double(du)
dw=double(dw)
dl=double(dl)
dRu=double(dRu)
dRw=double(dRw)
dRl=double(dRl)

; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEPlaneEllipticalGrating, name
Art = !phaOptElPlaneEllipticalGrating
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement(name)

; define struct GDat:{gdatset}
theta0=0
r=0
rp=0
xdens0=0
xdens1=0
xdens2=0
xdens3=0
xdens4=0
lambda=0
inout=0
iflag=0
azimut=0

; define struct MDat:{mdatset}
r1=0
r2=0
alpha=0
rmi=0
rho=0
iflagmi=0
w1=0
w2=0
l1=0
l2=0
slopew=0
slopel=0
du=0
dw=0
dl=0
dRu=0
dRw=0
dRl=0


; Set/Write struct GDat:{gdatset}
phaOptElementDefGeo, OptElement $
      ,theta0,r,rp,xdens0,xdens1,xdens2,xdens3,xdens4,lambda,inout,iflag,azimut

; Set/Write struct MDat:{mdatset}
phaOptElementDefMDat, OptElement $
      ,r1,r2,alpha,rmi,rho,iflagmi $
      ,w1,w2,l1,l2,slopew,slopel,du,dw,dl $
      ,dRu,dRw,dRl,Art


return,OptElement
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;












