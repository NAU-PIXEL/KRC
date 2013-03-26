function koft, tin, kode, arg3,arg4, ref, tlmm, got=got
;_Titl  KOFT   Thermal conductivity of minerals
; tin   in. fltarr(n) or scalar   Request temperatures in Kelvin
; kode  in. integer Which material/version. See case statement below
;             1:10   Uses an analytic form. 1:7 defined
;             11:19 Interpolates data points and also returns them. 11:16 defined
;             21:24  Extends data points returned in prior call to lower T'
;               21  using linear extentions of lowest two point
;               22  using 1/(A+BT) for lowest two point
;               24  Fits 3 or more points to 1/(A+BT)
; arg3  in. int or float   Varies with kode. Ignored if not listed here  
;             kode 1,2,4:7,11:19 Index of material
;             kode 24, fltarr(2), 0]=tolerance for fit 1]=frac for 1st try
; arg4  Varies  Input for: kode 3: Conductivity at 25C
;                          kode 4, Cond. at 0C
;              Output for: kode=11:19 (n,2) 0]= Kelvin 1]=conductivity
;              Input  for: kode=21:24. The output from kode=11:19, may be fewer
; ref   out. string  Reference for this method
; tlmm  out_  fltarr(4) 0]=low applicable   1]=low measure 
;                       2]=high measure     3]=high applicable
;                   these are somewhat arbitrary for some materials
; got   out_ fltarr(4) kode=20+  [A,B,#amoeba iteration, final error]
;               [3] negative indicates some failure, and func. may be constant
; func. out. fltarr(n) Thermal conductivity at tin in SI units: W / m / K
;            If invalid input, will return negative scalar 
;           -1=invalid kode  -2=Invalid sub index  -3=Too few pairs for extension
;_Calls  FIT_ABX  spl_init  spl_interp
; Note, web-based table of H2O Ice values in specheat.pro  
;_Hist 2010feb11-Mar21 Hugh Kieffer  Start with Clauser & Huenges 1995
; 2010jul13 HK Clean up error handling
;_End

nin=n_elements(tin)  & lin=nin-1 ; number of request points
tzc=273.15                      ; 0C in K
jpkc= 4.184e3                   ; joules per kilo-calorie
ia3=fix(arg3)                   ; material index for several kodes
siz=size(arg4)                  ; needed for 21:24
if siz[0] eq 2 then num4=siz[1] else num4=0 ; Number of T,k pairs input

case kode of                    ; which material or relation

1: begin & ref='_Horai70b'; Horai 70b 1/k = alpha + beta T
; based on measurements by Birch 1940
mater=['Dunite','Pyroxenite','Diabase','Gabbro','Anorthosite' $
,'Albitite','Granite'] ; from text after Eq 3b, ignore uncertainty
 ddd=[89,.20,100,.13,191,.02,204,0.003,238,-.07,206,.05,143,.16]; alpha,beta
; alpha in: cm sec degC /cal   beta in: cm sec /cal
nm=n_elements(mater) 
i=n_elements(ddd) & nd=i/nm
ddd=reform(ddd,nd,nm)
if ia3 lt 0 or ia3 ge nm then goto, badi
ref=mater[ia3]+ref
dd=ddd[*,ia3]
out=1./(dd[0]+dd[1]*(tin-300.)) ; cal / cm sec K
out=out*(0.01* jpkc) ; to SI
tlim=[-150,20,800,850]+tzc
end

2: begin & ref='_Zoth88' ; Eq 3a = Zoth & Hanel (1988) 
; Clauser 95 Table 1, from Zoth88 Fig 10.1:10.6
mater=['SaltRocks','Limestones','Metamorphics','AcidRocks','BasicRocks' $
,'Ultra-basics','Aveof5types']
ddd=[-20,40,-2.11,2960, 0,500,0.13,1073, 0,1200,0.75,705, 0,1400,0.64,807 $
,50,1100,1.18,474, 20,1380,0.73,1293, 0,800,0.70,770]; Tc-range, A B
nm=n_elements(mater) 
i=n_elements(ddd) & nd=i/nm
ddd=reform(ddd,nd,nm)
if ia3 lt 0 or ia3 ge nm then goto, badi
ref=mater[ia3]+ref
dd=ddd[*,ia3]                     ; values for sub-index material
tc=tin-tzc                      ; convert to Celsius
out=dd[2]+dd[3]/(350.+tc)
tlim=[-150,dd[0:1],dd[1]+50]+tzc
end

3: begin & ref='Sass92'    ; Eq 3c = Sass 92
lam25=arg4                      ; conductivity at 25C
tc=tin-tzc
lam0=lam25*(1.007+25.*(0.0037-0.0074/lam25))
out=lam0/(1.007+tc*(0.0036-0.0072/lam0))
tlim=[-150,50,200,250]+tzc
end

4: begin & ref='_Clauser95'; Clauser95 Eq 3b
mater=['gneiss','metabasite'] ; from text after Eq 3b, ignore uncertainty
ddd=[0.16, 0.37E-3, 0.33,0.22e-3]; D and E
nm=n_elements(mater) 
i=n_elements(ddd) & nd=i/nm
ddd=reform(ddd,nd,nm)
if ia3 lt 0 or ia3 ge nm then goto, badi
ref=mater[ia3]+ref
dd=ddd[*,ia3] 
out=1./(dd[0]+dd[1]*tin)
tlim=[-150,50,200,250]+tzc
end

5: begin & ref='_Seipold98'     ; Seipold98
mater=['paragneiss','amphibolite'] ; from FIg 5 caption
ddd=[0.282,6.17E-4,1.91E-9, 0.378,1.96E-4,0.24E-9  ]; A,B,C
nm=n_elements(mater) 
i=n_elements(ddd) & nd=i/nm
ddd=reform(ddd,nd,nm)
if ia3 lt 0 or ia3 ge nm then goto, badi
ref=mater[ia3]+ref
dd=ddd[*,ia3]
tc=tin-tzc 
out=1./(dd[0]+dd[1]*tc) +dd[2]*tc^3
tlim=[-150,20,800,850]+tzc
end

6:  begin & ref='_Vosteen03'    ; Vosteen
mater=['xtaline','seds']
nm=n_elements(mater)
ddd=[.003,.0042,500., .0043,.0039,300] ; a,b,Tmax
ddd=reform(ddd,3,nm)
if ia3 lt 0 or ia3 ge nm then goto, badi
ref=mater[ia3]+ref
dd=ddd[*,ia3]                     ; a,b,Tmax
tc=tin-tzc
lam0=arg4                       ; conductivity at 0C
out=lam0/(0.99+tc*(dd[0]-dd[1]/lam0) )
tlim=[-150,0,dd[2],dd[2]+50]+tzc
end

7: begin & ref='_Pet95' ; Petrunin 1995
; extract points at 200K and 800K on figs 1 and 2
; 1/k = A+BT in M K/W
mater=['labradorite','microcline','garnet','pyroxenite','SynGalGarnet' $
,'garnet','SynOlivine','olivine','alpha-SiO2' $ ; minerals
,'Granite','Diorite','Diabase','Dunite','Gabbro' $ ; Rocks
,'Serpentenite','Eclogite','mica']
ddd=[586,635, 474,573, 227,428, 203,405, 155,430, 262,284, 185,325 $
,165,300, 064,268 $
,228,500, 278,595, 313,544, 190,450, 500,510, 397,645, 418,575,365,520]
; ddd is 1000* 1/k at 200 and 800K 
nm=n_elements(mater)
ddd=reform(ddd,2,nm)
if ia3 lt 0 or ia3 ge nm then goto, badi
ref=mater[ia3]+ref
dd=0.001*float(ddd[*,ia3])        ; 2 Y values
b=(dd[1]-dd[0])/600.
a=dd[0]-b*200.
out=1./(a+b*tin)                ; 
if !dbug then begin 
    plot,dd,dd,xran=[0,1000],yran=[0,700],/nodata
    for i=0,8 do plots,[200,800],ddd[*,i]
    PAUSE,-1
    plot,dd,dd,xran=[0,1000],yran=[0,700],/nodata
    for i=9,nm-1 do plots,[200,800],ddd[*,i]
    stop
endif
tlim=[123.,200.,800.,900.]
end

;^^^^ Analytic ^^^^^^     vvvvvv data points vvvvvv

11: begin & ref='_Kana68'
; Table 3 from Kanamori 68, but only first row from orig. table
nt=9
ddx=300. +100*findgen(nt) ; Tk
mater=['FusedSilica','quartz001','quartz010','olinive','periclase']
ddd=[ $  ; J/m-s-C ? , from where?  Orig is e-3 cal/cm-s-deg
   2.74, 3.22, 3.58, 3.85, 4.12, 4.43, 4.92, 5.55, 6.30 $ ; cal; last estim.
 ,13.93, 8.20, 6.24, 4.81, 3.91, 3.56, 3.98, 4.56, 5.15 $ ; qtz 001
 , 7.49, 4.71, 3.83, 3.29, 2.91, 2.79, 2.75, 3.39, 4.03 $ ; qtz 010
 , 5.07, 4.73, 4.23, 3.89, 3.86, 3.98, 4.23, 4.77, 5.44 $ ; olivine
 , 60.0,46.05,34.12,27.21,23.19,19.63,17.12,15.61,14.32 ] ; periclase 1st estim.
nm=n_elements(mater) 
ddd=reform(ddd,nt,nm)
ddd[*,0]=ddd[*,0]*(jpkc/1.e4) ; from cal to J
if ia3 lt 0 or ia3 ge nm then goto, badi
ref=mater[ia3]+ref
ddy=ddd[*,ia3]                  ; k values for sub-index material
tlim=[290.,300.,1100.,1150.]
end

 12: begin & ref='NaCl_Yang81' ; NaCl of Yang 1981
ddd=[0.4,0.95,0.5,1.78,0.6,3.13,0.7,4.97,0.8,7.40,0.9,10.0,1.,14.0,2.,99.3 $
,3.,270.,4.,443.,5.,595.,6.,735.,7.,829.,8.,880.,9.,870.,10.,836.,15.,502. $
,21.,306.,25.,191.,30.,130.,40.,75.,50.,54.,75.,34.9,100.,24.3,150.,15.0 $
,200.,10.9,250.,8.24,293.,6.65,300.,6.57,400.,4.80,500.,3.67,600.,2.98 $
,700.,2.47,800.,2.08,900.,1.85,1000.,1.67] ; Tk and k
i=n_elements(ddd) 
nt=i/2                          ; number of data points
ddd=reform(ddd,2,nt)            ; separate x and y
ddx=reform(ddd[0,*]); Tk
ddy=reform(ddd[1,*]) ; k
tlim=[0.35,0.4,1000.,1100.]
end

13: begin & ref='SilicaGlass_Birch40' ; silica glass from Birch 1940a
;ddd=[0.0,1.36,50,1.44,100,1.48,150,1.53,200,1.58,250,1.64,300,1.70,350,1.78 $
;,400,1.85,450,1.94,500,2.07] ; Tc and ki=n_elements(ddd)
;i=n_elements(ddd) 
;nt=i/2                          ; number of data points
;ddd=reform(ddd,2,nt)            ; separate x and
; Cannot find above values in the original article. Do not know where I got them
; but agree with below after conversion to J  web gives 1.38: W/mK 1.46@20C
dd=[3.25,3.43,3.54,3.66,3.78,3.92,4.06,4.26,4.42,4.64,4.95] ; smoothed k 
nt=n_elements(dd)
ttt=50.*findgen(nt) ; Tc  Table 8.   k in ;1.e-3 cal / cm-sec-deg 
ddx=ttt+tzc                    ; Tk
ddy=dd * (jpkc/1.e4); k  convert to J/m-sec-deg
tlim=[0,50,500.,520]+tzc
end

14: begin & ref='_Gb64'
mater=['Silicon','Germanium']
ddd=[50,26.,5.9,60,21,4.7,70,17.,3.7,80,13.9,3.1,90,11.4,2.55 $ ; T,Si.Ge
,100,9.5,2.25,125,6.0,1.66,150,4.20,1.30,175,3.25,1.10,200,2.66,0.95 $
,250,1.95,0.73,300,1.56,0.60,400,1.05,0.44,500,0.80,0.338,600,0.64,0.269 $
,700,0.52,0.219,800,0.43,0.193,900,0.356,0.177,1000,0.310,0.171 $
,1100,0.280,0.169,1200,0.261,0.173] ;   W/cm/K
i=n_elements(ddd) 
nt=i/3                          ; number of data points
ddd=reform(ddd,3,nt)
if ia3 lt 0 or ia3 ge 2 then goto, badi
ddx=reform(ddd[0,*]); Tk
ref=mater[ia3]+ref
ddy=100.*reform(ddd[ia3+1,*])      ; convert to W/m/K
tlim=[50.,50.,1200.,1210.]
end

15: begin & ref='_Abdul06'
mater=['Sandstone','Limestone','Amphibolite','Granulite','Pyroxene-Granulite']
ddx=[273.,323,373,423]; temperatures in K
nt=n_elements(ddx)
ddd= [2.01,1.93,1.88,1.82, 1.94,1.78,1.65,1.55, 3.52,3.05,2.63,2.35 $
,2.06,1.96,1.85,1.77, 2.40,2.42,2.44,2.46] ; k in W/m-K [T,mater]
nm=n_elements(mater) 
i=n_elements(ddd) 
ddd=reform(ddd,nt,nm)
if ia3 lt 0 or ia3 ge nm then goto, badi
ddy=reform(ddd[*,ia3]); Tk
ref=mater[ia3]+ref
tlim=[123.,273.,423.,500]
end

16: begin & ref='Grossularite_Slack71'
ddd=[40,.73,50,.53,70,.32,100,.187,150,.127,200,.099,300,.072] ; T,K  W/cm-K
; debye T=770 K
i=n_elements(ddd) 
nt=i/2                          ; number of data points
ddd=reform(ddd,2,nt)
ddx=reform(ddd[0,*])
ddy=100.*reform(ddd[1,*]) ; convert to W/m-K
tlim=[35.,40.,300.,320]
end

21: begin & if num4 lt 2 then goto,bad4 ; extend lowest 2 points linearly
ddx=arg4[*,0] & ddy=arg4[*,1] ; values from prior call
b=(ddy[1]-ddy[0])/(ddx[1]-ddx[0]) ; slope
a=ddy[0]-b*ddx[0]               ;   intercept 
got=[a,b,0.,0.]                 ; return the fit parameters
out=a+b*tin                     ; analytic form
end

22: begin & if num4 lt 2 then goto,bad4 ; extend lowest 2 points using 1/(A+BT)
ddx=arg4[*,0] & ddy=arg4[*,1] ; values from prior call
b=(1./ddy[1]-1./ddy[0])/(ddx[1]-ddx[0]) ; fit to first 2  points
a=1./ddy[0]-b*ddx[0]            ;  " 
got=[a,b,0.,0.]                       ; return the fit parameters
if min([a,b]) le 0. then begin    ; physically unlikely
;    out=replicate(ddy[0],nin)    ; in case of bad form
    got[3]=-1                   ; indicate error
endif
out=1./(a+b*tin)                ; analytic form
end

24: begin & if num4 lt 3 then goto,bad4; do fit to extend points 
; will return Y for all tin, using 1/(A+BT) fit to T= arg4 and K=got
ddx=arg4[*,0] & ddy=arg4[*,1] ; values from prior call
pfit=FIT_ABX(ddx,ddy,arg3[0],frac=arg3[1]) ; fit all the data provided
print,'FIT_ABX out:',pfit,form='(a,2g12.5,f6.0,g12.7)'
if n_elements(pfit) lt 2 then begin ; fix failed or formal error
    got=[0,0,0,-1.]             ; indicate failure
    out=replicate(ddy[0],nin)
endif else begin
    got=pfit                    ; return the fit parameters
    out=1./(pfit[0]+pfit[1]*tin)
endelse 
end

else: begin & message,'Invalid kode',/con
    out=-1 & tlim=fltarr(4) & end
endcase

if kode ge 11 and kode le 16 then begin ; interpolate data to request
    arg4=reform([ddx,ddy],nt,2)  ; output data points
    y2=spl_init(ddx,ddy)      ; set up cubic spline interpolation,"natural" ends
    out=spl_interp(ddx,ddy,y2,tin) ; K at request temps.
end

if nin eq 1 then out=out[0]
if n_params() gt 5 then tlmm=tlim

done:
if !dbug then STOP
return,out

badi: Message,'Invalid sub index: max allowed='+string(arg3,nm-1),/con
out=-2 & goto,done

bad4: Message,'Too few T,k. Number input='+string(num4),/con
out=-3 & goto,done
end
