


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION phaNewOptElement, name
;input      name        : idl_string
;output     mirror      : struct pha4idlOptElementType

if n_params() eq 0 then blfname=string('Unnamed')

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

