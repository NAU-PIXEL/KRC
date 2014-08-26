function krclayer, fd,id,ld, ggg, flab=flab, glab=glab
;_Titl  KRCLAYER  Compute and print KRC layer depth table from KRCCOM values
; fd     in.  KRCCOM floats
; id     in.  KRCCOM integers
; ld     in.  KRCCOM logicals
; ggg   out.  fltarr(n1,2) diffusivity and convergence factor (no time doubling)
; flab  out_  strarr(7)  Short labels for columns of function
; glab  out_  strarr(2)  Short labels for columns of ggg
; func. out. fltarr (N1,7) of:  0=thickness in local scale  1=thickness in m
; 2=Center depth in Top scale   3=Center depth in m   4=Mass above layer center 
; 5=Mass above layer bottom     6=Depth to bottom in local scale
;_Desc
; stability requirements. delt_t < tlay^2/(2*diffu)
; stability factor: tlay^2/(2*delt_t*diffi)
;_Calls  none
;_Hist 2008apr13 Hugh Kieffer Convert Fortran from tday.f and tdisk.f
; 2014feb13 HK Omit obsolete layer indices. Add total mass above bottom of layer
; 2014may02 HK Add ggg output
;_End      .comp krclayer

flab=['Thick,scaled','Thick,m','Center,scaled','center,m' $
,'MassAboveCen','MassAboveBot','BotDeptLocal']

glab=['diffusivity','convergence']

; indices here are 1 less than in KRC helplist
n1    =id[0]
N2    =id[1]
IC    =id[7]
N24   =id[5]
SKRC  =fd[2]
COND2 =fd[3]
DENS2 =fd[4]
PERIOD=fd[5]
SPHT  =fd[6]
DENS  =fd[7]
SPHT2 =fd[15]
RLAY  =fd[32]
FLAY  =fd[33]
CONVF =fd[34]
LOCAL =ld[14]

COND=SKRC*SKRC/(DENS*SPHT)      ; surface conductivity
ti2=sqrt(cond2*dens2*spht2)     ; lower thermal inertia
PERSEC = PERIOD * 86400.        ; get solar period in seconds
;DTIM=PERSEC/N2                  ; size of one time step
DIFFU=COND /(DENS*SPHT)         ; surface diffusivity
DIFF2=COND2/(DENS2*SPHT2)       ; lower diffusivity
SCAL1=SQRT(DIFFU*PERSEC/!PI)    ; surficial diurnal skin depth
SCAL2=SQRT(DIFF2*PERSEC/!PI)    ; lower diurnal skin depth
;help,n1,n2,ic,local,skrc,cond2,dens2,period,spht,dens,spht2,rlay,flay,convf

diffi=replicate(DIFFU,n1) ; upper diffusivity
if ic gt 1 and ic lt n1 then diffi[ic-1:*]=diff2 ; lower

yy=flay*rlay^indgen(n1) ; factors in layer thickness

IF LOCAL then tlay=yy*SQRT (diffi   *PERSEC/!PI )  $
         else tlay=yy*SQRT (diffi[0]*PERSEC/!PI )
x=fltarr(n1)                    ; to hold depths
X[0]=-TLAY[0]/2.                ; x is depth to layer center, [cm]
for I=1,N1-1 do X[i]= X[I-1]+ (TLAY[i]+TLAY[I-1])/2.


; print layer depth table
Q2=DENS*SPHT
fmt1='('' Conductiv.='',E10.3,''  Dens*Cp='',E10.3,''  Diffu.='',E10.3,''  Scale='',E10.3)'
fmt2='(''Beginning at layer  '',i2,''  At  '',f8.4,'' m.   Inertia=  '',f8.1)'
print,COND,Q2,DIFFU,SCAL1, form=fmt1
IF IC GT 2  AND IC LT N1-1 then begin 
    Q2=DENS2*SPHT2
    Q6=X[IC-2]+TLAY[IC-2]/2.    ; depth to top of 2nd material
    print,ic,q6,ti2,form=fmt2    ; IC announcement
    print,COND2,Q2,diff2,scal2, form=fmt1 ; lower layer properties
ENDIF
print,'        ___THICKNESS____     ______CENTER_DEPTH________   _____BOTTOM______ '
print,' LAYER  LocScale   meter   TopScale     meter    kg/m^2    kg/m^2   LocScale'

q6=0.                           ; mass above center of layer
rhop=0.                         ; density of prior layer                       
rho=0.                          ; density of current layer
scale=scal1                     ; top layer properties
sumd=0.                         ; thermal scales above bottom of layer
burd=0.                         ; mass burden above bottom of the layer
out=fltarr(n1,7)                 ; array to be returned
for I=0,N1-1 do begin
    if i eq 1 then rho=dens
    if i eq ic-1 then begin
        rho=dens2
        scale=scal2
    endif
    Q2 = TLAY[I]/SCALE          ; thickness in units of local scale
    Q4 = X[I]/SCAL1             ; center-depth in units of surface scale
    if i ge 1 then begin 
        Q6=Q6+0.5*(TLAY[I-1]*rhop+TLAY[I]*rho) ; layer-center columnar mass
        sumd=sumd+q2
        burd=burd+TLAY[I]*rho
    endif
    print,I+1,Q2,TLAY[I],q4,X[I],q6,burd,sumd $
      ,form='(I5,F10.4,F10.4,g10.4,f10.4,2G10.3,f10.3)'
    rhop=rho                    ; will be density of prior layer
    out[i,*]=[Q2,TLAY[I],q4,X[I],q6,burd,sumd]
endfor
;        print,'Bottom layers for time doubling:  ',N1K[0:kkk], form='(a,10I5)'

ggg=reform([diffi,tlay^2/((2.*persec/n2)*diffi)],n1,2.,/over)

; Compute the layer indices used for Type 52. Algebra same as in tdisk.f
; Last step is to subtract 1 to make 0-based.
NI52=N24                         ; room for layers
Print,' Lowest layer that would be stored in Type 52 is',N24+1

return,out
end
