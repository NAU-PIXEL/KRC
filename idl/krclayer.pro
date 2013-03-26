function krclayer, fd,id,ld, ii
;_Titl  KRCLAYER  Compute center depth of KRC layers
; fd   in.  KRCCOM floats
; id   in.  KRCCOM integers
; ld   in.  KRCCOM logicals
; ii   out. indices in Type 52 item 5 layers
; func. out. fltarr of layer center depths, in meters
;_Hist 2008apr13 Hugh Kieffer Convert Fortran from tday.f and tdisk.f
;_End

; indices here are 1 less than in KRC helplist
n1    =id[0]
N2    =id[1]
IC    =id[7]
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
print,'        ___THICKNESS____     ______CENTER_DEPTH________    BOTTOM '
print,' LAYER  LocScale   meter   TopScale     meter    kg/m^2   LocScale'

q6=0.                           ; mass above center of layer
rhop=0.                         ; density of prior layer                       
rho=0.                          ; density of current layer
scale=scal1                     ; top layer properties
sumd=0.                         ; thermal scales above bottom of layer
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
    endif
    print,I+1,Q2,TLAY[I],q4,X[I],q6,sumd $
      ,form='(I5,F10.4,F10.4,g10.4,f10.4,G10.3,f10.3)'
    rhop=rho                    ; will be density of prior layer
endfor
;        print,'Bottom layers for time doubling:  ',N1K[0:kkk], form='(a,10I5)'


; Compute the layer indices used for Type 52. Algebra same as in tdisk.f
; Last step is to subtract 1 to make 0-based.
NI52=11                         ; room for layers
ii=intarr(ni52)                 ; to hold indices
J=1<((N1-1)/NI52)               ; smaller interval, at least 1
I=N1-1-J*NI52                   ; number of larger intervals, might be negative
K=NI52-(0>I)                    ; number of smaller intervals
help,j,i,k
if k gt 0 then for I=0,K-1 do II[i]=(1+J*(I+1))<N1 $; load with smaller intervals
else begin
    ii[0]=1+j
    k=1
endelse
IF K LT NI52 THEN for I=K,NI52-1 do II[I]=II[K-1]+(J+1)*(I+1-K) ; load with larger intervals
print,'ii=',ii                  ;-------------------- temp
ii=ii-1                         ; convert to zero-based

return,x
end
