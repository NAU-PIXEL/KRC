function partcond, parr,konin,xyo, labin=labin,idout=idout
;_Titl  PARTCOND  Particulate conductivity for cemented materials
; parr  in.  fltarr(11)  Parameters, described below. Will create if needed
; konin in.  intarr(*)   Control kodes. Use [16] if nothing else
; xyo   out_ fltarr(n,2) Central angle and  delta Temperature
; labin out_ strarr(11)  Labels for input parameters
; idout out_ strarr(2)   Brief one-word IDs for output parameters
; func. out. fltarr(n,2) or scalar
;     after @43,-4 [nB,2] 0]=volume fraction cement  1]=net conductivity
;     after @44,-2 scalar:  net conductivity 
;_Desc   Based on ~/xtex/themis/cement.tex
; Do several forms of solution to avoid blunders
;_Calls 
;_Hist 2008nov22-dec06 Hugh Kieffer
; 2009feb27 HK option for no print
; 2010apr20 HK add keywords  labin  and  idout 
; 2010jul14 HK Include treatment of Knudsen number
;_End

common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;;SETCOLOR,init=856  & nkc=kcc[2] & nkl=kcc[3] ; initialize lines and colors
ptitl='partcond' & version ='2010apr20'

labin=['edit: flag -=stop >1=help ','Rg: grain radius, mu m' $
,'T: temperature, K','kh: grain thermal conductivity' $
,'k0: bulk gas cond.  J/m.s.K','kc: cement cond.  =SI' $
,'emis: grain emissivity','G: host grain cond. factor -1=ave -2=geomean' $
,'P: Pressure in Pascal','Dm: molecule collision diameter, m' $
,'B: Cement contact angle, radian', 'vvvv:--numerical parameters' $
,'flag: test integrals','beta: SMX limit radian' $
,'nLo: steps along cement [Int]','nHi: steps along gap [Int]' $
,'DP: Flag, double precision','B1: Initial B','Brat: ratio for loop']

idout=['CemVol','NetCond']

parr0=[0.,100., 250.,0.937, 0.003, 2., .98, 0.9, 500.,4.65E-10, 1.e-2 $
,77.77, 0., 0.1, 30, 100, 0., 1.e-3, 1.1 ] ; defaults

kons=konin 
nlabr=n_elements(labin)
if n_elements(parr) ne nlabr then begin 
    parr=parr0                  ; create parr array with defaults
    if kons[0] ne -4 then begin ; if doing some real action
        ii=where(kons eq 16,j)  ; look for a kon to edit it
        if j eq 0 then kons=[16,konin] ; ensure some edit done
    endif
endif
ii=where(kons eq 16) & i=ii[0] ; first modify of parr
ii=where(kons eq 20) & j=ii[0] ; first transfer of variables
if i ne 0 and j ne 0 then kons=[20,konin]; always initiate variables from parr

if n_elements(kcc) eq 0 then SETCOLOR,/init ; required to define items in common.

idr=strarr(nlabr)
for i=0,nlabr-1 do begin 
    j=strpos(labin[i],':')
    idr[i]=strmid(labin[i],0,j)
endfor

prior=['1','1']                 ; impossible values, to initiate MAKE99
;^^^^^^^^^^^^^^^^^^^^^^ inputs
sigb=5.67051E-8                 ; Stephan-Boltzman constant, W m$^{-2}$ K$^{-4}$
kb=1.380658D-23                ; Boltzman constant, J/K
noop=60 & koop=70 & qqq=findgen(2,2) & knet=-1. & outt=0  & dop=1B ; insurance
qab=findgen(2,2) & xyo=findgen(2,2) & ddd=findgen(2) ; insurance
!except=2
;===============================================================================
kite=' '              ; type definitions
kon=123 & goto,dokon               ; start auto-seq on
ask: ;---------------------------------------- interactive parameter change
konl=kon
if lkon then begin              ; auto-sequence
    kkon=kkon+1                 ; increment to next item
    kon=kons(kkon)              ; get action from list
    if koop ge noop-1 and dop then print,'Doing -------------->',kon ; Notice 
    if kkon eq lastkon then lkon=0B ; turn off further auto-processing
endif else begin
    sureask: lkon=0B                 ; forced request for action
    READ, kon, prompt=ptitl+' Enter selection: 99=help 0=stop 123=auto> '
endelse
dokon: kitel=kite               ; remember prior action to use in subtitle
kite=ptitl+'@'+strtrim(kon,2)   ; follows date in subtitle      fils[2]+' '+
kip=kite+'    '                 ; for possible use by HISTFAST
if kon ge 850 and kon le 859 then begin ; set to set of high-contrast colors

 SETCOLOR,init=kon & nkc=kcc[2] & nkl=kcc[3] & goto,ask & end
case kon of ;...................................................................
-4: return,outt ; return 43 [nB,2] 0]=volume fraction cement  1]=net conductivity
-2: return,knet ;< return 44  net conductivity 
 0: stop                        ;-

123: begin & lkon=1b & kkon=-1   ;- start auto-script 
lastkon=n_elements(kons)-1 & end ; last preset command 

125: begin & kons=[20,4,42,44,126,-3] ; Loop over set of Bval
bval=bone
noop=fix(alog(1./bval)/alog(brat)) ; number of intervals to 57 deg
koop=0 
looplab=['bval','knet','dxx','dal2','si1','si2','si32' $
,'g1','g2','g3','hgas','hrad']
qqq=fltarr(noop,n_elements(looplab)) 
lastkon=n_elements(kons)-1      ; Start auto-sequence
lkon=1b & kkon=-1 & end         ; Start band Looping 

126: begin ;+ loop sequence
qqq[koop,*]=[bval,knet,dxx,dal2,si1,si2,si32,g1,g2,g3,hgas,hrad]
kkon=-1                         ; start at the beginning of loop
if koop lt noop-1 then begin    ; loop for another
    koop=koop+1
    bval=bval*brat
endif else begin                ; done loop
    help,vp,vq,cgas,crad
    xx=qqq[*,0]                 ; Bval
    kons=[51,-1,52,-1,53,-1,54]             ; set to look at results
    lastkon=n_elements(kons)-1  ; new auto-sequence
endelse
end

127: begin & kons=[43,128,-3] ; Loop kc  REQ 43 
t1=systime(1)
vloop=[0.5,1.,2.,4.,6.,12.]     ; Kc values
noop=n_elements(vloop)          ; number of intervals to 57 deg
koop=0                          ; for first loop
kc=vloop[koop]                  ; update loop value
if !dbug then print,'127',koop,kc
looplab=['kc']
qq3=fltarr(nall,noop)           ; to hold results
lastkon=n_elements(kons)-1      ; Start auto-sequence
lkon=1b & kkon=-1 & end         ; Start band Looping 

128: begin ;+ loop sequence
qq3[*,koop]=www[*,3]            ; save result
if koop lt noop-1 then begin    ; loop for another
    kkon=-1                     ; start at the beginning of loop
    koop=koop+1
    kc=vloop[koop]              ; update loop value
endif else begin                ; done loop
    t2=systime(1)
    print,'Time=',t2-t1
    print,'Do 437 and 438 for plot'
endelse
if !dbug then print,'128',koop,kc
end

131: begin & kons=[20,2,43,-1,431,-1,432,-1,433,-1,434,-1,435,-1,436] ; std
parr[[3,4]] = [0.937,0.003]; set to Paper II fig 5 values
end

132: kons=[4,42,44,51,-1,57,-1,577] ; Details for one Bval
;;; 1x == Modify values; Guides; commitment ..................................

;15: GETPAN,'Integer values',pari,-1,999,labs=labi ;- modify any of a set
16: begin & GETPAN,'PARTCOND: Float values',parr,0.,0.,labs=labin ;- Modify parr
    kon=20 & goto,dokon & end

;18: help,ss,dd,t1,t2              ; Guides and help

20: begin ; Set limits and constants. Auto after 16
dop=parr[0] ne 0                ; print kons and output
rg=parr[1]*1.e-6                ; grain radius, m
temk=parr[2]                    ; temperature K  
kh=parr[3]                      ; grain thermal conductivity    
k0=parr[4]                      ; gas bulk thermal conductivity
kc=parr[5]                      ; cement thermal conductivity
emis=parr[6]                    ; grain emissivity
gfin=parr[7]                    ; grain cond. factor
pres=parr[8]                    ; pressure in Pascal
cold=parr[9]                    ; molecule collisional diameter, m
bcem=parr[10]                   ; central angle to cement contact, radians
; -------[11] numerical parameters
tint=parr[12]                   ; flag: test integrals zero to beta
beta=parr[13]                   ; beta, upper limit for: small-angle approx=SAX
nlo=fix(parr[14])               ; number of steps below B
nhi=fix(parr[15])               ;  " " above B
dod=parr[16] ne 0               ; do in double precision
bone=parr[17]                   ; Initial B
brat=parr[18]                   ; ratio for loop
ctit=MAKEKEYVAL(idr[1:8],ST0(parr[1:8],/nojoin))
stit=MAKEKEYVAL(idr[13:15],ST0(fix(parr[13:15]),/nojoin))
bval=bcem                       ; insurance
capc=8.*(emis/(2-emis))*sigb*temk^3 ; radiative constant C
gfac=gfin
if gfin le -2 then gfac=1./beta else if gfin le 0 then gfac=(1./beta^2 +1.)/2. $
 else if gfin lt 1. then gfac=1./beta^gfin
; option for integration interval.
xo=0. &  xb=bval &  xp=!pi/2.   ; physical limits
if dod then begin     ; initiate double precision
    xo=0.D0 & xb=double(bval) & xp=!dpi/2.d0
endif
if tint then begin              ; integrate parts over 0:beta or beta:2*beta
    xp=beta                     ; must be small for SMX
    xb=xp/2.                    ; keep upper limit in small-angle range 
endif
if xb gt !dtor*45. then message,'contact angle > 45 ',/info
lc=2.*rg                        ; pore dimension in the corners
l1= lc/3. ; average distance between uncemented part of sphere, low limit
mfp=kb*temk/(sqrt(2.)*!pi*cold^2*pres) ; mean free path
Kn=mfp/[l1,lc]                  ; Knudsen number
gxi=(alog10(1./Kn)-2.15)/0.55   ; Xi
qq=k0/(1.+exp(-gxi)) ; k_gas relation
kg=qq[0]                        ; gas conductivity between spheres
kgc=qq[1]                       ; " " in open corners
zo=cos(xo) & zb=cos(xb) &  zp=cos(xp) ; transformed limits
f1a=1.                          ; denom constant in X
f1c=kc/(gfac*kh)                ; constant in del_c/U
f1b=kc/(gfac*kh) -1.            ; constant in del_c/Uy & denom coef of z in X
va=-rg*capc/kh                  ; a in integral form, coef of z^2
vb=(kg+capc*rg)/kh-1.           ; b in integral form, coef of z
vc=1.                           ; c in integral form, constant
si4=2./!pi -0.5                 ; Corner area I4
h2k=0.5*!pi*rg              ; factor to convert H_t/(U 2 pi R^2) to conductivity
cgas=h2k*kgc/rg*si4             ; corner gas  conductivity
crad=h2k*capc *si4              ; corner rad. conductivity
cork=cgas+crad                  ; corner conductivity
if dop then print,'Corner gas,rad,sum=',cgas,crad,cork
nall=nlo+nhi
end

2: begin  ; Crude cement only
noop=fix(alog(beta/bone)/alog(brat))
bb=bone*brat^findgen(noop) ; B from bone to 1.  uniform in log
; bb=(beta/nlo)*(1.+findgen(nlo)) ; set of many B's uniformly spaced up to beta
lab2=['GeoMean','Average',strtrim(gfin,2),' " * 2',' " * 4','1/sqrt B','1/B^0.9']
nk2=n_elements(lab2)
krat=fltarr(noop,nk2)             ; to hold each case
for k=0,nk2-1 do begin 
    case k of 
        0: gfak=1./bb           ; geometric mean
        1: gfak=(1.+1./bb^2)/2. ; averag
        2: gfak=1.              ; fixed
        3: gfak=2.*gfak         ; double
        4: gfak=2.*gfak         ; and double again
        5: gfak=(1./bb^0.5) > 1.    ; 1/sqrt B
        6: gfak=(1./bb^0.9) > 1.    ; 0.9 root B
    endcase
    krat[*,k]=!pi/(kh/kc+(4./bb^2 -1.)/gfak) ; Eq. kkh
endfor
    vuc=(3.*!pi)/16*bb^4 ; cement proportion
    ya=min(krat,max=yb)
topt='Cement plate: conduction only:  kc/kh= '+strtrim(kc/kh,2)
plot,vuc,krat[*,0] ,yran=[ya,yb*1.1],/xlog,/ylog,/nodata $
,xtit='cement fraction ',ytit=' k / kh ',title=topt
for k=0,nk2-1 do begin 
    i=k mod 6
    oplot,vuc,krat[*,k],line=i, thick=1.+1.*(k/6)
    CURVEGUIDE,k,lab2[k],i, thick=1.+1.*(k/6)
endfor
end

4: begin ; Solved integrals 
; based on analytic integrations of I1,I2,I3 in Eq. HU
; integral  I1: limits are x=0:B,   z=1:cos B
; integration limits are cos 0 : cos B
if f1b eq 0 then begin ; no cos in denominator. Intergal is sin^2/2
    sinx=sin(xb)
    si1=sinx^2/2.
endif else begin
    uXlo=f1a+f1b*zo             ;  X at lower limit of z
    uXhi=f1a+f1b*zb             ;  "    upper "
    if min([uXlo,uXhi]) le 0. then message,'some X negative'
    si1=(-1./f1b^2)*((uXhi-uXlo)-f1a*(alog(abs(uXhi)) -alog(abs(uXlo)) ))
endelse
; integral  I2: limits are x=B:,   z=cos B :0
bsm4ac= vb^2-4.*va*vc           ; b^2-4ac
if bsm4ac lt 0 then message,'b^2 < 4ac not coded'
sqb=sqrt(bsm4ac)                ; sqrt (b^2-4ac)
twoa=2.*va                      ; 2a
vp=(-vb+sqb)/twoa               ; quadratic root p
vq=(-vb-sqb)/twoa               ; quadratic root q
dxx1=1./(va*(vp-vq))            ; leading factor in Eq. dxox
dxx2=alog(abs((zp-vp)/(zp-vq)))- alog(abs((zb-vp)/(zb-vq))) ; ln part  "
dxx=dxx1*dxx2                   ; Eq. dxox evaluated at limits
bigXlo= (va*zb +vb)*zb +vc      ; X for lower limit
bigXhi= (va*zp +vb)*zp +vc      ; X for upper limit
dal2= alog(abs(bigXhi))-alog(abs(bigXlo)) ; delta ln |X|, in Eq. i2
hi1=(1./twoa)*dal2              ; first part Eq. i2
hi2=(vb/twoa)*dxx               ; 2nd part "
si2= hi2-hi1                  ; integral i2 at limits, including negative sign
; I3 solved  First part is same as I2 limits are x=B:,   z=cos B :0
twoasq= 2.*va^2                 ; 2a^2
fac32= (vb^2-2.*va*vc)/twoasq   ; factor for integral; Eq. i32,  b2-2ac / 2a^2
si32=(zp-zb)/va - (vb/twoasq)*dal2 + fac32*dxx ; Eq. i32 evaluated at limits
si3=si2+si32                    ; Integral I3
sss=[si1,si2,si3]               ; solved integrals
end

42: begin                       ; Simplist numerical integration
dxl=(xb-xo)/nlo                ; delta x over 0:B
dxh=(xp-xb)/nhi                ; delta x over B: hi (normally pi/2)
xyo=fltarr(nlo+nhi,2) 
;I1 ,cemented part, if any. Reduced to sin x, 0:B 
g1=0.
if xb gt xo then begin  ; I1: some cement
    sum=0.
    for k=0,nlo-1 do begin      ; each step across cement
        xx=xo+(k+0.5)*dxl       ; middle of step interval
        cosx=cos(xx)             ; cos(x)
; next line to avoid /0 for small angles
        if not dod and xx lt 1.e-3 then omcx=xx^2/2. else omcx=1.-cosx
        delcou=1./(1.+f1c*cos(xx)/omcx)
        func=sin(xx)*cosx/(1.+f1b*cosx) ; function inside integral
        sum=sum+func
        xyo[k,*]=[xx,delcou]
    endfor
    g1=sum*dxl
endif
g2=0. & g3=0. & g23=0.
labq=['denom','func','fun3','kg/y','C+kg/y','kh/(R-y)','funq']
if xb lt xp then begin ; I2 and I3
    qab=fltarr(nhi,n_elements(labq))
    for k=0,nhi-1 do begin        ; 
        xx=xb+(k+0.5)*dxh       ; middle of step interval
        sinx=sin(xx)
        cosx=cos(xx)
        denom=vc+vb*cosx+va*cosx^2 ; denominator
        delou=(1.-cosx)/denom   ; Eq. delU
        func=(sinx*cosx)/denom  ; function inside I2
        fun3=cosx*func          ; function inside I3
        g2=g2+func*dxh          ; integrate I2
        g3=g3+fun3*dxh          ; integrate I3
        xyo[k+nlo,*]=[xx,delou] ; store angle, del/U
        y=rg*(1.-cosx)          ; y
        qa=capc+kg/y            ; term 1 in alternate
        qb=kh/(rg*cosx)         ; term 2 " " 
        funq=sinx*cosx*qa*qb/(qa+qb) ; function inside alternate
        g23=g23+funq*dxh        ; 2nd integral in Eq. Ht
        qab[k,*]=[denom,func,fun3,kg/y,qa,qb,funq]
    endfor
endif
ggg=[g1,g2,g3]              ; numeric integration I1,I2,I3
end

425: begin                       ; Simplist numerical integration 2010jul
; does Eq HN  2010jul14
dxl=(xb-xo)/nlo                ; delta x over 0:B
dxh=(xp-xb)/nhi                ; delta x over B: hi (normally pi/2)
xyo=fltarr(nlo+nhi,2) 
;I1 ,cemented part, if any. Reduced to sin x, 0:B 
g1=0.
if xb gt xo then begin  ; I1: some cement
    sum=0.
    for k=0,nlo-1 do begin      ; each step across cement
        xx=xo+(k+0.5)*dxl       ; middle of step interval
        cosx=cos(xx)             ; cos(x)
        gfak=gfin               ; grain cond. factor
        if      gfin le -2 then gfak=1./xx $ ; geometric mean
        else if gfin le  0 then gfak=(1./xx^2 +1.)/2. $ ;average
        else if gfin lt 1. then gfak=1./xx^gfin ; to a power
        f1x=kc/(Gfak*kh)
; next line to avoid /0 for small angles
        if not dod and xx lt 1.e-3 then omcx=xx^2/2. else omcx=1.-cosx
        delcou=1./(1.+f1x*cosx/omcx) ; del_c / U   Eq. delcu
        func=sin(xx)*cosx/(1.+f1x*cosx) ; J1
        sum=sum+func
        xyo[k,*]=[xx,delcou]
    endfor
    g1=sum*kc*dxl/Rg
endif
g2=0. & g3=0. & g23=0.
dx2=dxh/rg ; multiplier of J2 sum
dx3=capc*dhx ; multiplier of J3 sum
labq=['KN','kg','vbx','denom','J2','Jr']
if xb lt xp then begin ; I2 and I3
    qab=fltarr(nhi,n_elements(labq))
    for k=0,nhi-1 do begin        ; 
        xx=xb+(k+0.5)*dxh       ; middle of step interval
        sinx=sin(xx)
        cosx=cos(xx)
        omsx=1.-sinx^2
        lx=2.*rg*(omsx-.66666*cosx^3)/omsx ; verticle path between spheres
        knx=mfp/lx              ; Knudsen number in this annular zone
        kgx=k0/(1.+exp((2.15-alog10(1./Knx))/0.55)) ; pore gas conductivity
        vbx=(kgx+capc*rg)/kh-1. ; elements of denom
; next line to avoid /0 for small angles
        if not dod and xx lt 1.e-3 then omcx=xx^2/2. else omcx=1.-cosx
        denom=vc+vbx*cosx+va*cosx^2 ; denominator in J2 and Jr
        sod=scx/denom           ; same in J2 and Jr
        f2=kgx*sod              ; J2
        funr=omcx*sod           ; Jr
        g2=g2+dx2*f2            ; sum J2
        g3=g3+dx3*funr          ; sum Jr
        xyo[k+nlo,*]=[xx,delou] ; store angle, del/U
        qab[k,*]=[knx,kgx,vbx,denom,f2,funr]
    endfor
endif
ggg=[g1,g2,g3]              ; numeric integration I1,I2,I3
end

43: begin ; Numerical integration for all B
; first evaluate the parts: cement, gas, radiation, for all radii
; Then integrate the cement outward and the others inward
; Sum to get the results for all values of B
nall=nlo+nhi
labv=['B','G','Knud','kg','denom','cement','gas','rad']
vvv=fltarr(nall,n_elements(labv))
dxa=(xp-xo)/nall
if !dbug then print,'43',koop,kc
; separate d sin x into  cos x  dx  moving dx to outside the sum
for k=0,nall-1 do begin          ; all steps of bval
    xx=xo+(k+0.5)*dxa           ; middle of step interval, radian
    sinx=sin(xx)                ; sin x
    cosx=cos(xx)                ; cos x.   Also d sin x / dx
    scx=sinx*cosx               ; sin x * cos x
    gfak=gfin                   ; grain cond. factor
    if      gfin le -2 then gfak=1./xx $ ; geometric mean
    else if gfin le  0 then gfak=(1./xx^2 +1.)/2. $ ;average
    else if gfin lt 1. then gfak=1./xx^gfin  ; to a power
    g1b=kc/(gfak*kh) -1.        ; constant in del_c/Uy & denom coef of z in X
    omsx=1.-sinx^2
    lx=2.*rg*(omsx-.66666*cosx^3)/omsx ; verticle path between spheres
    knx=mfp/lx                  ; Knudsen number in this annular zone
    kgx=k0/(1.+exp((2.15-alog10(1./Knx))/0.55)) ; pore gas conductivity
    vbx=(kgx+capc*rg)/kh-1.     ; b in integral form, coef of z
; next line to avoid /0 for small angles
    if not dod and xx lt 1.e-3 then omcx=xx^2/2. else omcx=1.-cosx
    f1=scx/(1.+g1b*cosx)        ; J1
    denom=vc+vbx*cosx+va*cosx^2 ; denominator in J2 and Jr
    sod=scx/denom               ; same in J2 and Jr
    f2=kgx*sod                  ; J2
    funr=omcx*sod               ; Jr
    vvv[k,*]=[xx,gfak,knx,kgx,denom,f1,f2,funr]
endfor
; do the integration
ice=where(labv eq 'cement',k) & ice=ice[0] & if k ne 1 then goto,halt
labw=[labv[ice:*],'knet'] & nw=n_elements(labw)
www=fltarr(nall,nw)
q=dxa*h2k                  ; factor to get conductivity
www[*,0]=(q*kc/Rg)*CUMSUM(vvv[*,ice+0]) ; cement: integrate out
www[*,1]=   (q/Rg)*CUMSUM(vvv[*,ice+1],/rev) ; gas: integrate in
www[*,2]=(q*capc) *CUMSUM(vvv[*,ice+2],/rev) ; radiation: integrate in 
www[*,3]=total(www[*,0:2],2)+cork ; total conduction
qq=cos(vvv[*,0]) ; cos B
vus=9.*((1.-qq^2)/2.- (1.-qq^3)/3.) ; V_s following Eq B = 6
ii= where(vvv[*,0] le !pi/4.,j4) & j4=j4-1 ; last without cylinder intersect
ii= where(vvv[*,0] lt 0.1,j)    ; angles: single precision cut point
if j ge 1 then vus[ii]=(9./8.)*vvv[ii,0]^4 ; small angle formula
vus=vus/(1.+vus)                ; fraction of solid volume
outt=reform([vus,www[*,3]],nall,2)
end

4302: begin & print,ggg ; Test of 42 & 43
 help,hgas,hrad,cgas,crad,cork,htu,knet         
w2=fltarr(nall,nw)
w2[*,0]=CUMSUM(vvv[*,ice+0])
w2[*,1]=CUMSUM(vvv[*,ice+1],/rev)
w2[*,2]=CUMSUM(vvv[*,ice+2],/rev)
print,w2[20,*]
end

431: CHART,vvv,parti=labv,dlin=1, title=ctit,xin=vvv[*,0] $; CHART: vvv
,xtit='Central angle to edge of cement, [radian]'
4312: CHART,vvv,parti=labv,dlin=1, title=ctit,xin=vus $;+ vrs vol.
,xtit='Cement volume as fraction of solids'
432: CHART,www[0:j4,*],parti=labw,dlin=1, title=ctit,xin=vvv[0:j4,0]  $; CHART www
,xtit='Central angle to edge of cement, [radian]'
4322: CHART,www[0:j4,*],parti=labw,dlin=1, title=ctit,xin=vus[0:j4]  $;+ vrs vol.
,xtit='Cement volume as fraction of solids'
433: plot,vvv[0:j4,0],www[0:j4,3],xtit='B, radian',ytit='k_t' ; Plot k_t
434: plot,100.*vus[0:j4],www[0:j4,3] $ ;+ vrs vol.
,xtit='Cement volume as % of solids' $
,ytit='Thermal Conductivity  W /(m K)',titl=ctit
435: begin & ii=where(vus lt 0.33) ; Plot 5A1
plot,100.*vus[ii],www[ii,3],xtit='Cement volume as % of solids',titl=ctit $ 
  ,xran=[0.,33],yran=[0.,2.0],ytit='Thermal Conductivity  W /(m K)' &  end
436:  begin & ii=where(vus lt 0.01) ;+ 5B1
plot,100.*vus[ii],www[ii,3],xtit='Cement volume as % of solids' $ 
,xran=[1.e-4,1.],yran=[0.,0.4],ytit='Thermal Conductivity  W /(m K)' $
  ,titl=ctit,/xlog,psym=-4  & end

437: begin  ; Plot 5A
plot,vus,vus,xtit='Cement volume as % of solids',/nodata $ 
,xran=[0.,33.],yran=[0.,2.0],ytit='Thermal Conductivity  W /(m K)',titl=ctit
for k=0,noop-1 do begin
    oplot,100.*vus,qq3[*,k],line=k
    CURVEGUIDE,k,string(vloop[k],form='(f4.1)'),k
endfor & end
4372: for k=0,noop-1 do oplot,100.*vus,qq3[*,k],line=k;+ 2nd set

438:  begin & ii=where(vus lt 0.01) ;+ 5B
plot,vus,vus,xtit='Cement volume as % of solids',/nodata ,titl=ctit,/xlog $ 
,xran=[1.e-4,1.],yran=[0.,0.4],ytit='Thermal Conductivity  W /(m K)' 
for k=0,noop-1 do begin
    oplot,100.*vus[*],qq3[*,k],line=k
    CURVEGUIDE,k,string(vloop[k],form='(f4.1)'),k
endfor & end

44: begin ; Sum parts REQ 4,42
fff=[kc/rg,kg/rg+capc,-capc ,kg/rg+capc]  ; factors for integrals Ix
if 2 gt 3 then ccc=sss else ccc=ggg ; CHOOSE NUM.INT
ppp=fff*[ccc,si4]               ; all parts of Eq. HU  CHOOSE NUM.INT
hgas=(kg/rg)*ccc[1]             ; gas conduction annulus
hrad=capc*(ccc[1]-ccc[2])       ; rad conduction annulus
htu=total(ppp)                  ; Eq. HU
;keach=
knet=h2k*htu             ; k_t, net conductivity
if abs(parr[0]) ge 2. then begin 
    help,capc,xo,xb,xp,zo,zb,zp
    print,'        I1'
    help,f1a,f1b,uXlo,uXhi
    print,'        I2' 
    help,va,vb,vc,bsm4ac,sqb,twoa,vp,vq,dxx1,dxx2,dxx,bigxlo,bigxhi
    help,dal2,hi1,hi2
    print,'        I3' 
    help,twoasq,fac32,si32
    help,dxl,sum,dxh 
    help,hgas,hrad,cgas,crad,cork,htu,knet
endif
if abs(parr[0]) ge 1. then begin 
    Print,'g23 and p2+p3',g23,ppp[1]+ppp[2]
    print,'   How           I1             I2          I3'
;      Solved   4.76837e-07      4.10327      727.603
    print,'integral ',si1,si2,si3, '  < I2 and I3 wrong'
    print,'Numeric  ',g1,g2,g3
endif
if tint then begin ; compute SMX integrals
    gsq=2.*(1+f1b)/f1b
    if f1b gt -1 and f1b lt 0. then gsq=-gsq ; ensure positive
    epsq= xb^2
    lgsq=alog(gsq)
    if f1b gt 0 then brak=alog(abs(gsq-epsq))-lgsq $ ; part in []
    else brak=alog(abs(gsq+epsq))-lgsq
    if f1b gt -1 and f1b lt 0. then s9=brak/f1b else s9=-brak/f1b
; treating cos x as 1.
    sb=sin(xb) & sp=sin(xp)
    s1=sb^2 /( 2.*kc/(gfac*kh))
    twoc2=2.*(kg+capc*rg)/kh
    s2=(sp^2-sb^2)/twoc2
    s3=(xp^2-xb^2)/twoc2
    if xp gt .1 then message,'SMX beta is too big',/con
    print,'SMX      ',s1,s2,s3
    print,'SMX+ cosx',s9
endif
if abs(parr[0]) ge 1. then begin
    print,'Term  W/m*K ',h2k*ppp
    print,'Corner gas and rad.=',cgas,crad
    print,'k_tot  W/m*K=',knet
endif
end

51: plot,xyo[*,0],xyo[*,1],psym=-4 $ ; Plot del/U vrs B
,xtit='B angle',ytit='del / u', titl='Bval='+string(xb)
52: CHART,qqq,parti=looplab,dlin=1,titl=ctit ; CHART,qqq

;53: CHARM,qqq,parti=looplab,titl=ctit, psy=4 ; CHARM,qqq
53: CLOT,qqq,looplab,titl=['step','Normalized',ctit],locc=locc,yran=[1,1]

54: plot,qqq[*,0],qqq[*,1],psym=-4,/ylog,/xlog,xtit='bval',ytit='knet';+ knet

56: CHART,qab, parti=labq,xtit='step',titl=ctit ; CHART qab
561: plot,qab[*,5]/qab[*,4],/ylog,xtit='step',ytit='kh/(R-y) / C+kg/y' ;+ qb/qa
562: plot,qab[*,5]*qab[*,4],/ylog,xtit='step',ytit='kh/(R-y) * C+kg/y' ;+ qb*qa
 ;57: CHARM,qab, parti=labq,xtit='step',titl=ctit,psy=4; CHARM qab
57: CLOT,qab,labq,titl=['step','Normalized',ctit], locc=locc,yran=[1,1]; CLOT qab
;577: CHARM,alog10(qab), parti=labq,xtit='step',psy=7,titl='alog10()' ;+ log10
577: CLOT,alog10(qab),labq,titl=['step','Normalized Log_10',ctit], locc=locc,yran=[1,1] ;+ log10
58:  SUBTITLE,id=stit ; add Subtitle 



77: begin ; oldstuf:
npd=5                            ; points per decade
md=4.99                            ; decades
a1=1.D-5                        ; minimum angle, radians
af=10.D0^(1.D0/npd)             ; factor between successive angles
nj=fix(npd*md)
aa=a1
dd=dblarr(nj,7)
kcoh=1. ; k_c / k_h
for i=0,nj-1 do begin
    ca=cos(aa) & sa=sin(aa) 
    qq=(1.d0/2.D0)*(1.D0-ca^2) -(1.D0/3.D0)*(1.D0-ca^3); analytic solution
    qq2= aa^4/8.                ; small-angle approximation
    sigc= !pi*sa^2              ; sigma c = cylinder cross-section
    Vol=(!pi/2.)*aa^4           ; normalized volume of one cylinder
    Y=vol/sigc                  ; average thickness of cement
    rkk=(sigc/y)/(1.+kcoh*sqrt(sigc/!pi)*((1.-Y)/Y)) ; k_net / k_cement
    dd[i,*]=[aa,sa,QQ,qq2,sigc,vol,rkk]
    aa=aa*af ; increase central angle
endfor

plot,dd[*,1],dd[*,3],/xlog,/ylog, psym=-4,xtit='Normalize radius of cylinder' $
,ytit='Normalized volume of fillet'

ii=where(dd[*,2] gt 0)
oplot,dd[ii,1],dd[ii,2], line=2, color=kkc[2]
pause,-1
  plot,dd[*,5],dd[*,6],/xlog,/ylog, psym=-4 $
,xtit='Normalized volume of fillet',ytit='k/k_c'
end

else: begin & KON91,ptitl,prior,hold,kon,kons,kitel ;=KON99 < needed by MAKE99
;  begin & ire9=KON99(ptitl,prior,hold,m9,v9,kon,kons,kitel, dd,bbb,log,avv)
;      if ire9 eq 1 then goto,sureask & if ire9 eq 2 then goto,halt
      if kon eq 99 then begin 
;          print,'15: Subset: pari= ',ST0(pari)
          print,'16: Floats: parr= ',ST0(parr)
      endif & end
endcase
goto, ask

halt:  print,'SOME ERROR CONDITION at kon=',kon,'.  Any key to Go'
i=get_kbrd(1) & goto,sureask
end
