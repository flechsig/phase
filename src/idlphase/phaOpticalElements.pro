
; file: phaOpticalElements.pro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOECoefficientFile, name
Art = !phaOptElGeneral
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEPlaneGrating, name
Art = !phaOptElPlaneGrating
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEPlaneVLSGrating, name
Art = !phaOptElPlaneVLSGrating
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEPlaneMirror, name
Art = !phaOptElPlaneMirror
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOESlit, name
Art = !phaOptElSlit
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEDrift, name
Art = !phaOptElDrift
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEToroidalMirror, name
Art = !phaOptElToroidalMirror
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEToroidalGrating, name
Art = !phaOptElToroidalGrating
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEToroidalVLSGrating, name
Art = !phaOptElToroidalVLSGrating
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOECone, name
Art = !phaOptElCone
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEEllipticalMirror, name
Art = !phaOptElEllipticalMirror
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEPlaneEllipticalMirror, name
Art = !phaOptElPlaneEllipticalMirror
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOEPlaneEllipticalGrating, name
Art = !phaOptElPlaneEllipticalGrating
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

OptElement = phaNewOptElement, name

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

