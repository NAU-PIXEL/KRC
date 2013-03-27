function sp09a,tin,rgin,ppin, phin,kcin, labp=labp, desc=desc, dbug=dbug 
;_Titl  SP09a  Sylvain Piqueux unconsolidated particulate thermal conductivity 
; tin   in.   fltarr(1+) Temperature in Kelvin,      - yields default set 
; rgin  in.   fltarr(1+) Grain radius in micrometer, - yields default set
; ppin  in.   fltarr(1+) Pressure in Pascal,         - yields default set
; phin  in_   fltarr(1+) Porr fraction,              - yields default set
; kcin  in_   float  Optional contact point conductivity. Default is 0.001
; labp  out_  Strarr(m=16) Brief labels for output
; desc  out_  strarr(m) Description of each item
; dbug  in_   Debug flag: 1 will print, 3 will also stop
; func. out.  fltarr(nrow,m)    m items as identified in labp  
;  row are in order of possible loops [pressure,temperature, phi,GrainRadius] 
;_Desc
; Based on JGR article 2009: 114(E13) 9005+ 
; Any or all of the inputs can be vectors or scalars. Output order of nrow
; will be effective [Pressure, temperature, pore fraction, radius]
;_Hist 2010apr29 HK Modify spspread.pro to follow Piqueux90a
;_End

if tin[0]  gt 0. then temkk=tin else temkk=[150.,230.,315] ; T in kelvin
if rgin[0] gt 0. then rgg=rgin  else rgg=[10.,50.,200.] ; radii
if ppin[0] gt 0. then ppa=ppin  else ppa=[60,400,1200]  ; P in Pascal
if phin[0] gt 0. then phii=phin else phii=[0.30,0.35,0.4,0.45]
if n_params() ge 5 then kcon=kcin else kcon=0.001 ; point contact conductivity
if kcon lt 0. then kcon=0.001 ; ensure positive

nr=n_elements(rgg)  
nt=n_elements(temkk)
nh=n_elements(phii) 
np=n_elements(ppa)
; SC simple cubic=face-centered phi=0.476
; cc close cubic                phi=0.259
ccc=[ $ ; this is Table 1
 6.621e-4, 2.147e+2, -9.985e-2, -4.056e+2, 7.063e+0, 2.046e+2, 8.954e+0 $ ;CC
,1.316e-5, 2.559e-1,  8.832e-2, -8.477e-1, 4.240e-1, 7.316e-1, 6.968e-1 $ ;SC
,4.116e-5, 7.407e+1,  1.108e+1,  6.248e+2, 3.299e+2, 2.095e+2, 5.366e+2 $ ;CC
,1.840e-3, 2.250e+2,  3.957e+1,  4.491e+3, 2.871e+3, 3.621e+3, 5.016e+3]  ;SC
;    a=0      b=1        c=2       d=3       e=4       f=5       g=6
ccc=reform(ccc,7,4,/over)  ; 2]=kgas to K_CC   3]=kgas to K_SC

;-----------
boltz = 1.3806503E-23           ; m2 kg s-2 k-1 Boltzman gas constant k
sbcon =5.67051E-8               ; Stephan-Boltzman constant
;COLLD = 4.65E-10               ; CO2 molecular diameter  Hugh
; vvvvvvvvvvvvvvv based on formulation of  Sylvain  Piqueux
colld =4.6E-10                  ; theta, collisional diameter of  CO2, m Sylvain
phicc = 0.259                   ; porosity of cubic-packed spheres
phisc=0.476                     ; " just before Eq 27
f9=0.08                         ; fixed parameter i Eq. 9
e9=0.99                         ; "
kgrain=0.937                    ; solid conductivity, Fused Silica [27.3]
;^^^^^^^^^^^^^^^^^^^^^^^

labp=['R um','Phi','T','Pa','MFP','K0','Pore','Kn','xi' $
,'Kgas','KeffCC','KeffSC','keff','krad','knet','kVasa']
Desc=['Grain radius, micrometer. Input','Porosity. Input' $
,'Temperature, K. Input','Pressure, Pa. Input' $
,'Mean Free path, m. Eq 7','Bulk gas conductivity. Eq 14' $
,'Typical length of Pore. Eq 11','Knudsen Number, Eq 7' $
,'Exponent in kgas. Eq 29,30,31','Gas thermal conductitivy, Eq. 32' $
,'Effective gas conductivity, close cubic Eq 25' $
,'Effective gas conductivity, simple cubic' $
,'Effective gas conductivity, interpolated to porosity Eq 27' $
,'Radiative part of conductivity','Total conductivity' $
,'Vasavada 99 relation for radiative term']

nl=n_elements(labp) 
ddd=fltarr(np,nt,nh,nr,nl)
; frequently used terms
mfpcon=boltz/(sqrt(2.)*!pi*colld^2) ; Eq. 7 constant in MFP
epr=e9/(e9+0.5*(1.-e9))         ; Eq. 4 epsilon prime
q=epr*(1.-f9)                   ; used in Eq 3
psi=(2.*f9+q)/(2.*(1.-f9)-q)    ; Eq. 3
krcon=4.e-6*psi*sbcon             ; factor in Eq 2., include micron to meter

;loops structured to yield row order for defaults
for i=0,np-1 do begin           ; pressure loop
  pasc=ppa[i]                   ;  P in pascal
  for j=0,nt-1 do begin         ; temperature loop
    tk=temkk[j]               
    mfp=mfpcon*tk/pasc          ; Eq. 7: L Meanfree path
    k0=418.4*(4.608e-5* sqrt(tk))/(1.+6212.*10^(-10./tk)/tk) ;Eq. 14 k0
    krv=0.000922*(1.+1.48*(tk/350.)^3) ;S krad Vasavada 99
      for k=0,nh-1 do begin     ; porosity loop
        phi=phii[k]           
        for m=0,nr-1 do begin   ; grain size loop
          rg=rgg[m]             ; grain radius in micron
          krad=krcon*rg*tk^3    ; Eq.2 krad 
          pore= rg*1.e-6*(phi/(1.-phi))^0.33 ; Eq. 11 
          knu=mfp/pore          ; Eq. 7  Knudsen number
          ookn=pore/mfp         ; 1/Knudsen number
          xi= (alog10(ookn) -2.15)/0.55 ; Eq. 29,30,31 xi, function of KN^-1
          kgas=k0/(1.+exp(-xi))  ; Eq. 32  Kgas
;;K_xC = (((gk+e)k+c*k)+a)  / (((fk+d)k+b)k+1)
          cc=ccc[*,2]
          khn=((((cc[6]*kgas)+cc[4])*kgas+cc[2])*kgas+cc[0])
          khd=((((cc[5]*kgas)+cc[3])*kgas+cc[1])*kgas+1.)
          kcc=khn/khd           ;26
          cc=ccc[*,3]
          khn=((((cc[6]*kgas)+cc[4])*kgas+cc[2])*kgas+cc[0])
          khd=((((cc[5]*kgas)+cc[3])*kgas+cc[1])*kgas+1.)
          ksc=khn/khd           ;26
          keff=kcc+(ksc-kcc)*(phi-phicc)/(phisc-phicc) ;27, corrected
          knet=kcon+krad+keff    ;

ddd[i,j,k,m,*]=[rg,phi,tk,pasc,mfp,k0,pore,knu,xi,kgas,kcc,ksc,keff,krad,knet,krv]
       endfor
    endfor
  endfor
endfor

ng=np*nt*nh*nr                  ; total number of rows
ddd=reform(ddd,ng,nl,/over)            ; make 2-D array for output
if keyword_set(dbug) then begin 
    help, tin,rgin,ppin, phin,kcin,ddd,labp,desc
    j=0 
    qq=reform(ddd[j,*])
    fmt='(i3,a9,g12.5,2x,a)' 
    print,'  i     name       value  Description'
    print,-2,'kcontact',kcon,'Contact conductivity; assumed',format=fmt
    print,-1,'kgrain',kgrain,'Solid grain conductivity; assumed',format=fmt

    for i=0,nl-1 do print,i,labp[i],qq[i],desc[i],format=fmt
    if ng gt 1 then CHART,ddd,parti=labp
    if abs(dbug) ge 3 then stop
endif
return,ddd
end
