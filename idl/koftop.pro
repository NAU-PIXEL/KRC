;_Titl  KOFTOP  Specific heat, thermal conductivity. Minerals, particulate soils
;_Calls 
;  EVMONO    Evaluate monomial power series
;  LUFIT     Linear least-squares fit using LU decomposition
;  KOFT      Thermal conductivity of minerals
;  MEAN_STD  Mean and standard deviation of a vector
;  PARTCOND  Particulate conductivity
;  PP_CO2    Vapor pressure and thermal properties of  CO2
;  PRESLEY   Thermal conductivity of glass spheres
;  RNDEX     Finds floating-point index of within a monotonic array
;  SPECHEAT  Specific heat capacity of minerals
;  SP09a     Sylvain Piqueux unconsolidated particulate thermal conductivity
;  XYY5      Interface to concatonated [spectral] arrays of different length
; Utility: CLOT  GETPAN  GETPSN  KON91  LABEL_CURVE  PAUSE  SETCOLOR  ST0
;_Desc   Sections
; 2   set parameters and, T range, scaling ....
; 3   k(T)
; 4   Cp(T)
; 5   fit
; 6   plot data, plot fit
; 7   Partcond for k(T)

;  Get kgas by calling SP09A for any grain size, extract kgas(T) from return.
; loop on PARTCOND for Rg, for a set of T, for many B
;   extract results for a few B
; fit desired conditions
; Print table of selected conditions and coefficents.
; T' coefficients:       ccc      refc       = generic, either K or cp
;          chosen: @56   cck(4)   refk     @57   ccp(4)   refp
;  grain & cement: @710 park(4,2) matk(2)  @711 parp(4,2) matp(2)
;    particulates:       cpk(4)   repk          cpp(4)   repp
;_Hist 2010feb20 Hugh Kieffer  To create inputs for KRC
; 2010mar16-21 Add k(T) solids to partcond. Firmcode these.
; 2010jul09 HK Update interface comments only 
;_End

ptitl='koftop' & version ='2010mar19'

common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1

;labf=['Input DIR',' " file']
;parf=['/work1/lunar/GOES/','d02346_4600_4800.out.txt']

torr=133.3223 ; pressure conversion, Pascals 

labi=['Last @34','Material index ','Secondary index','Fit kode @51' $
 ,'bbb index, -=auto','Size of bbb.','Ylog flag @73' $
,'Cold extension: KofT action',' " Max points to use'] 
pari=[6, 2,4,3,0, 8,0,24, 4]

labc=['--- spare','Material index','Secondary index','Debye Temp. K' $
,'Reference temp. C: Waples','Reference Cp','X of left   \ Normalized' $
,'Y of first  | locations','Delta Y     | for','Line Lenght / Guide']
parc=[-7.,5,2,850.,20.,700.,  0.67,0.93,-0.024,0.08]

labg=['Fit toler  \ FIT_ABX','Frac.Scale | > Amoeba','Print Flag  /','----spare' $
,'Plot Ymax for k',' " "  for Cp' $
,'@76 X left \ Normaliz | middle Xval' $
,'Y of first | locations| offset units of char height' $
,'Delta Y    | for      | c.size LABEL_CURVE' $
,'Line Lenght/ Guide    | c.thick  ' $
,'                      |leave: 0=clear' ]
parg=[.01, .05, 0,-7, 20.,1000., 220.,0.8,1.,1.,1 ] ;

q=['ConCof','LinCof','^2 Cof','^3 Cof']
cck=[2.774009,-0.5354000,0.2042885,-0.07103046] & refk='BasicRocks:Zoth88'
labk=['Grain K '+q,'Cement K '+q]
matk0=[refk,'limestone']
park0=[cck, 3.791455,  -1.297852,  0.3453778,-0.03867132 ] ; Solids k(T)
park0=reform(park0,4,2,/over) 
park=park0

ccp=[636.1573,272.5468,-28.12828,-39.34503] & refp='clinochlore:Fe=0.89'
labp=['Grain Cp '+q,'Cement Cp '+q]
matp0=['Sphene',refp]
parp0= [581.3780,198.1888,-52.26276, 12.05196 ,ccp]  ; Solids Cp(T) 
parp0=reform(parp0,4,2,/over) 
parp=parp0

labr=['Rg: Grain radius, micron -=table','P: pressure Pascal' $
,'phi: Porosity fraction','host grain conductivity',' " "  density' $
,'Cement fraction  -=table',' "  conductitity',' "  density' $
,'Point-contact conduct.','k @25C for Sass92','@632 YplotMax' $
,'T min, Kelvin',' " max',' " delta' $
,'vvv-Beware-vvv','T scaling offset','" " multiplier']

parr=[-1.,600.,0.35,0.937, 2400.,-1.E-3,2.0, 1400.,.001,4.0,0.7 $
,120.,320.,20. ,-7.77, 220., 0.01]
 
labs=['Rock k index',' " K subindex',' " spare',' " Cp index',' " " subindex' $
,' " " value',' " density',' " fraction','Ice density','Void fraction']
pars=[2,4,-7.,5,4.9,20., 2400.,.1,1000.,.1]
   
ppp=PARTCOND(parm,[-4],xyo, labin=labm,idout=idout) ; get labels and defaults

rrset=[10.,20,50,100,200,500,1000.,2000.,5000,1.e4,2.e4,5.e4]
;rrr=[20.,100.,1000.,1.e4]       ; grain radii
;rrr=[10.,50.,200.]              ; grain radii in spreadsheet
vvset=[1.e-8,1.e-6,1.e-5,1.e-4, 1.e-3, 1.e-2, .02, .05, .10] ; vol percent cement
gc=['Grain','Cement']

prior=['1','1']                 ;impossible values, to initiate MAKE99
;^^^^^^^^^^^^^^^^^^^^^^ firm code
;===============================================================================
kons=[71,25,260,60,5,856]           ; initiation
kite=' ' & lkon=0B              ; type definitions 
kon=123 & goto,dokon            ; do initiation
ask: ;---------------------------------------- interactive parameter change
konl=kon
if lkon then begin              ; auto-sequence
    kkon=kkon+1                 ; increment to next item
    kon=kons(kkon)              ; get action from list
    print,'Doing -------------->',kon ; Notice of current action to monitor
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
; -2: return                      ;<
 0: stop                        ;- Stop

112: begin & kons=[25,5,34,64] ; Do conductivity: Group 1
pari[0]=6 & parg[4]=15.  & parc[6]=.67 & end

113: begin & kons=[34,64] ; Group 2
pari[0]=7 & end

114:  begin & kons=[35,64,-1, 64,655] ;+ data points
parg[4]=80.  & end              ; params for 2nd K plot

115: kons=[12,15,16, 25,60,41,5,51,55,61,62] ; Do one specific heat fit

116: begin & kons=[25,50,44,64]      ; Do specific heat family
parg[5]=1000. & end

117: kons=[717,74,75,76,-1,73,-1] ; k(T) solids in partcond,
; append 70,717,735 to overplot retsults with constant properties for solids

118: begin & kons=[260,60, 31,261,32,33,61,62,1182, 651,88] ; loop for big picture
ppp=[12,0, 14,0, 14,1, 11,4, 11,3, 11,0, 15,1, 15,2, 16,0] ; materials to use
nloop=n_elements(ppp)/2
ppp=reform(ppp,2,nloop)
kloop=0 ;
pari[1:2]=ppp[*,kloop] 
end

1182: if kloop lt nloop-1 then begin ;+ increment
    kloop=kloop+1               ; increment loop
    pari[1:2]=ppp[*,kloop]      ; update material
    kkon=1                      ; one before first in loop
endif

131: kons=[15,25,41,315,  5, 33,335,57]; Cp for one material
132: kons=[12,25,31,315,5,33,335,56,710] ; k for one material

138: kons=[48,33,335,56,25,5] ; Use H2O k
139: kons=[49,491,5,33,57,25,5] ; Use H2O Cp

123: begin & lkon=1b & kkon=-1  ;- Start auto-script 
lastkon=n_elements(kons)-1 & end ; last preset command 

;11: GETPSN,'File names: parf',parf,lab=labf ;- modify any of a set of strings
12: GETPAN,'Cond. Ints: pari',pari,-1,999,labs=labi ;- Integers: pari
13: GETPAN,'partcond: parm',parm,0.,0.,labs=labm ;- PARTCOND: parm
14: GETPAN,'FitPlot: parg',parg,0.,0.,labs=labg ;- Fit,plot: parg
15: GETPAN,'Cp values: parc',parc,0.,0.,labs=labc ;- Cp values: parc
16: GETPAN,'Cond. values: parr',parr,0.,0.,labs=labr ;- Cond. values: parr
166: GETPAN,'Mix values: pars',pars,0.,0.,labs=labs ;- Mix control and values
17: GETPAN,'GrainCemK: park',park,0.,0.,labs=labk ;- Partic K of T: park
177: GETPAN,'GrainCemCp: parp',parp,0.,0.,labs=labp ;- Partic Cp of T: parp
18: help,kon,ttt,yin,xxx,tex,bbb,rrr,ggg,dd4,cc4,pp4 ; Guides and help

25: begin ; Setup T
tmin=parr[11] & tmax=parr[12] & tdel=parr[13]
ntt=round((tmax-tmin)/tdel) +1
ttt=tmin+tdel*findgen(ntt)      ; Temperature vector
toff=parr[15]                   ; temperature offset for scaling
tmul=parr[16]                   ;   " multiplier " " 
q=min(abs(ttt-toff),ito)        ; get index of T closest to offset
end

260: q=XYY5('Load')             ; Empty XYY storage

261: if n_elements(arg4) gt 1 then $  ;+ Add material data to XYY
q=XYY5('Add',refc,imat,arg4)

31: begin & imat=pari[1]        ; Thermal conductivity of 1 mineral
arg3=pari[2]                    ; secondary index for several imat
if imat eq 3 then arg3=parr[9]  ; k at 25C for Sass
yin=KOFT(ttt,imat,arg3,arg4,refc,tlim)
if yin[0] lt 0 then goto,halt ; formal error
cytit='Cp  J / kg K ' & cztit='Specific heat  SI units' & cid='Con'
k=pari[4]                       ; storage index
if k lt 0 then begin            ; auto increment
    kbb=kbb+1
    k=kbb
endif
if k ge np then begin
    message,'Out of room in bbb',/con
    goto,halt                   ; out of room
endif 
bbb[*,k]=yin ; save 
sss[k]=refc
end

315: plot,ttt,yin,psym=4 $ ; Plot one material
,xtit='Temperature, Kelvin', ytit=cytit,title=refc

32: begin ; Extend data sets to lower T REQ 31
; Normally follow imat=11+ 
siz=size(arg4)                  ; needed for extension
if siz[0] eq 2 then num4=siz[1] else num4=0 ; Number of T,k pairs available
if num4 ge 2 then begin
    ddx=arg4[*,0] & ddy=arg4[*,1] ; arg4[ is   0=T, 1]=conductivity
    ii=where(ttt lt ddx[0],j1)  ; requests below lowest data point
endif else j1=0
if j1 gt 1 and imat lt 11 then begin 
    q=where(ddx ge tmin,jf)     ; points to use in fit
    if jf lt 2 then goto,halt   ; should never occur
    ja=pari[7]                  ; extension method
    if ja eq 24 and jf lt 3 then ja=22 ; 2-points or fit
    j2=(pari[8]-1)<(n_elements(ddx)-1) ; upper data point
    yout=KOFT(ttt,ja,parg[0:1],arg4[0:j2,*],refc, got=got) 
    if got[3] lt 0 then begin   ; extension may have failed
        print,'Fit warning;',got 
        print,'Pole at ',-got[0]/got[1] 
        xa=min([ttt,ddx],max=xb)
        ya=min([yin,ddy],max=yb)
        plot,ddx,ddy,psym=4,xran=[xa,xb],yran=[ya,yb]  $; data points
        ,xtit='Kelvin.  diamond=data  dash=spline  line=extension' $
          ,ytit='Thermal conductivity',title='Warning @32:'
        plots,ttt,yin,line=5    ; long dash spline extrapolation
        oplot,ttt[ii],yout[ii],color=kkc[2] ; solid line 1/(A+BT)
        stop
    endif
    yin[ii]=yout[ii]            ; replace cold request points
    bbb[*,k]=yin                ; update
endif else print,'Doing nothing'
end

33: begin ; Cubic coefficents for KRC
    if n_elements(xxx) ne n_elements(yin) then goto,halt
    ccc=poly_fit(xxx,yin,3,yfit, yband,sigma,corrm)
    ccc=reform(ccc)
    ar=MEAN_STD(yfit-yin,std=std)
    print,std,ccc,cid,refc,form='(f9.5,'' >'',4g14.7,'' <'',a,1x,a)' 
end

335: oplot,ttt,yfit ; Oplot fit

34: begin & nmat=[7,7,1,2,2,2,17] ; Test all analytic K for all their materials
k2=pari[0] < 7                 ; last material index
if k2 le 6 then k1=1 else k1=7 ; do 7 alone, as contains 17 materials
np=round(total(nmat[k1-1:k2-1]))
bbb=fltarr(ntt,np)
sss=strarr(np)
print,'T range and delta=',parr[11:13]
kbb=-1
print,'  StdDev    constant    Linear     T''^2     T''^3 ImatIndx Reference'
;     ' 0.00038 >  0.572740 -0.156726  0.046502 -0.013005 < 1 0 Dunite_Horai70b
for imat=k1,k2 do begin         ; analytic expressions
    n2=nmat[imat-1]             ; # materials   
    for i=0,n2-1 do begin 
        arg3=i                  ; material index
        arg4=parr[9]            ; conductivity at reference T
        yin=KOFT(ttt,imat,arg3,arg4,refc,tlim) ; get values
        if yin[0] lt 0 then goto,halt
        ccc=poly_fit (xxx,yin,3,yfit, yband,sigma,corrm) ; fit to T'
        ar=MEAN_STD(yfit-yin,std=std) 
     print,std,ccc,imat,i,refc,form='(f8.5,'' >'',g10.6,3f10.6,'' <'',2i2,1x,a)' 
        kbb=kbb+1
        bbb[*,kbb]=yin
        ii=where(ttt lt tlim[0] or ttt gt tlim[3],j) ; out of T range
        if j ge 1 then bbb[ii,kbb]=!values.f_nan
        sss[kbb]=refc
    endfor
endfor
if kbb lt np-1 then bbb=bbb[*,kbb]
cytit='k: W / m / K' & cztit='Thermal conductivity, SI units'
yrb=4  & end

35: begin & nmat=[5,1,1,2,5]      ; Test all data-set  materials
np=round(total(nmat))           ; total number of materials
q=XYY5('Load')                  ; empty XY storage for all data points
bbb=fltarr(ntt,np)              ; to hold cubic-fit 
sss=strarr(np)                  ; to hold material  IDs
kbb=-1                            ; storage index
print,'Pnts  StdDev      constant      Linear       T''^2       T''^3 ImatIndx Reference'
;     '  9  0.04323 >   1.4080096  -0.0108534  -0.1341701  -0.1145972 <11 0 FusedSilica_Kana68
for imat=11,10+n_elements(nmat) do begin         ; data sets
    n2=nmat[imat-11]            ; # materials   
    for i=0,n2-1 do begin       ; each material in a set
        arg3=i                  ; transfer the sub-index
        yin=KOFT(ttt,imat,arg3,arg4,refc,tlim) ; get data points
        if yin[0] lt 0 then goto,halt ; error
        ddx=arg4[*,0] & ddy=arg4[*,1] ; arg4[ is   0=T, 1]=conductivity
        if i eq 0 then begin    ; first material
            if n2 gt 1 then yran=[0.,parg[4]] else yran=[0,max([yin,ddy])]
            xa=min([ttt,ddx],max=xb)
            plot,ddx,ddy,psym=4,xran=[xa,xb],yran=yran ; data points
        endif else oplot,ddx,ddy,psym=4 ; remaining materials
        ii=where(ttt lt ddx[0],j1) ; requests below lowest data point
        if j1 gt 1 then begin 
            q=where(ddx ge tmin,jf) ; points available for fit
            if jf lt 2 then goto,halt
;  help,imat,i,yin[0],j1,jf,j2,ja & print,got 
            ja=pari[7]          ; extension method
            if ja eq 24 and jf lt 3 then ja=22 ; 2-points or fit
            j2=(pari[8]-1)<(n_elements(ddx)-1) ; upper data point
            yout=KOFT(ttt,ja,parg[0:2],arg4[0:j2,*],refc, got=got) 
            if got[3] le 0 then print,'Fit warning;',got ; extension failed
            plots,ttt[ii],yin[ii],line=5 ; long dash spline extrapolation
            yin[ii]=yout[ii]    ; replace cold request points
        endif
        oplot,ttt,yin,color=kkc[2] ; solid line for retained values
;        PAUSE,-1
        ccc=poly_fit(xxx,yin,3,yfit, yband,sigma,corrm) ; fit cubic
        ar=MEAN_STD(yfit-yin,std=std) ; RMS error
        kbb=kbb+1                   ; increment storage index
        sss[kbb]=refc
        bbb[*,kbb]=EVMONO(ccc,xxx) ; cubic for each output temperature
        oplot,ttt,bbb[*,kbb],line=2 ; short dash plot cubic fit
        ii=where(ttt gt tlim[3],j) ; above data T range
        if j ge 1 then bbb[ii,kbb]=!values.f_nan ; don't plot too-hot points 
    print,jf,std,ccc,imat,i,refc,form='(i3,f9.5,'' >'',4f12.6,'' <'',2i2,1x,a)'
        nyy=n_elements(yfit)
        j=XYY5('Add',refc,imat,arg4) ; original data points
    endfor
   PAUSE,-1
endfor
cytit='k: W / m / K' & cztit='Thermal conductivity, SI units' 
yrb=4 & end               ; Y max for plot

36: qq=SP09A(240.,100.,500.,.35, labp=labd,dbug=1) ; SP09a parameters

37: begin ; Particulates using SP09A
if parr[0] gt 0. then rrr=parr[0] else rrr=rrset ; grain radii
; call once to get labels and positions
ddd=SP09A (ttt[0],rrr[0],parr[1],parr[2],parr[8], labp=labd)
ii=where(labd eq 'K0')   & dik=ii[0] ; index for gas conductivity
ii=where(labd eq 'keff') & dig=ii[0] ; " " effective gas conductivity
ii=where(labd eq 'knet') & din=ii[0] ; " " net conductivity from spread
nr=n_elements(rrr)              ; # particle radii
xxx=(ttt-toff)*tmul             ; scale
; in SP09A, k_gas depends upon T, Rg, pressure and pore size
; can call for vector of one or more inputs.
; Choose to call for vector of: temperature 
dd4=fltarr(ntt,nr,n_elements(labd)) ; Piqueux results [T,rg, item]
ccm=fltarr(4,nr)             ; to hold fit coeffs 
for k=0,nr-1 do begin           ; each grain size
;     if k eq nr-1 then dbug=1 else dbug=0
; sp09a args:  tin,rgin,  Pascal, PoreFrac, kcontact
    ddd=SP09A (ttt,rrr[k],parr[1],parr[2],   parr[8])
    dd4[*,k,*]=ddd             ; [T,Rg,item]
endfor
; help,k,j,yyy,cc,ferr,ccm,erti,eee
end

377: begin ; Convert to use 73+ OVERRIDES 717
ztit='Un-cemented' 
zzz=(ttt-toff)*tmul             ; scaled data-point Tk
pp4=dd4[*,*,14]                 ; net conductivity [T,nip]
vvv=rrr                         ; independent variable
nip=nr                          ; number of independent points = grain radii
dip=0B                          ; flag and last index of materials
fvg=1.-parr[2]                  ; fractional volume of grain
cvol=fltarr(nip)                ; zero cement
kgrain=replicate( 0.937,ntt)    ; grain conductivity over T. Value used by SP09A
kgas=dd4[*,*,dik]               ; bulk gas conductivity [T,nip]
txt=ST0(vvv,/nojoin)
end

38: ggg=PRESLEY(rrr,parr[1]) ; Presley conductivity REQ 37

41: begin & imat=parc[1]        ; Specific heat capacity of 1 mineral
if imat eq 1 then arg3=parc[3] else $ ; Debye T
if imat eq 8 then arg3=parc[4] $ ; ref T in C
             else arg3=parc[2]  ; secondary index
arg4=parc[5]                    ; Cp at ref temp for Debye or Waples
k=pari[4]                       ; storage index
yin=SPECHEAT(ttt,imat,arg3,arg4,refc,tlim)
sizb=size(bbb)
if n_elements(yin) eq sizb[1] and k lt sizb[2] then begin 
    bbb[*,k]=yin
    sss[k]=refc
endif else print,'Not stored in bbb'
cytit='Cp  J / kg K ' & cztit='Specific heat  SI units' & cid='Sph' & end

44: begin  ; Test many SPECHEAT options
kkk=[  1 ,  1 ,3, 3,4,5,5,5,5,    6, 8, 9] ; material index
aaa=[530.,812.,0,20,0,1,2,3,4.89, 7, 0, 0] ; arg3
np=n_elements(kkk)
bbb=fltarr(ntt,np)
sss=strarr(np)
print,'Resid        constant      Linear       T''^2       T''^3 Imat  Arg3 Reference'
;     '  0.856 >     730.378     171.153    -97.3609     35.9256 <  1 530.0 Debye_530._273.15_800.
    for k=0,np-1 do begin
        imat=kkk[k]
        if imat eq 1 or imat eq 8 then arg4=800.; Cp at Ref T 
        arg3=aaa[k]
        yin=SPECHEAT(ttt,imat,arg3,arg4,refc,tlim)
        ii=where(ttt ge tlim[0] and ttt le tlim[3],j) ; within T range
        ccc=poly_fit (xxx[ii],yin[ii],3,yfit, yband,sigma,corrm)
        ar=MEAN_STD(yfit-yin[ii],std=std) 
    print,std,ccc,imat,arg3,refc,form='(f7.3,'' >'',4g12.6,'' <'',i3,f6.1,1x,a)'
        sss[k]=refc
        if imat eq 3 then plot,ttt,arg4,xtit='K',ytit='k'
        bbb[*,k]=yin
        ii=where(ttt lt tlim[0] or ttt gt tlim[3],j) ; out of T range
        if j ge 1 then bbb[ii,k]=!values.f_nan
    endfor
cytit='Cp  J / kg K ' & cztit='Specific heat  SI units' 
yrb=5 & end

48: begin & cid='Cond'           ; fit H2O k   RESETS ttt
ttt=120.+5.*findgen(32) & ttt[31]=273.15 ; 120K :0C
yin=SPECHEAT(ttt,9,6,arg4,refc,tlim) ; web conductivity
; ttt ignored; yin is k, arg4 is T, can use all points
plot,arg4,yin,psym=4,xran=[140.,275],yran=[2.1,4.5],ytit='Conductivity' $
,xtit='Temperature Kelvin',title='Fits to H2O conductivity'
;Fit the web date with cubic and plot extenstion to 140K
xxx=(arg4-toff)*tmul            ; scaled Tdata
ccd=poly_fit(xxx,yin,3,yfit, yband,sigma,corrm) ; fit the data points
xxx=(ttt-toff)*tmul 
qq=EVMONO(ccd,xxx)              ; compute fit relation over 120:273 fit 
oplot,ttt,qq,line=1             ; and plot it
abc=linfit(1./arg4, yin)        ; fit  A+B/T to all points
ylin=abc[0]+abc[1]/ttt          ; compute this relation over T
oplot,ttt,ylin,line=3           ; & plot
tx=143.15+10.*findgen(3)        ; 3 additional points at low end
;B=(yin[1]-yin[0])/(1./arg4[1]-1./arg4[0]) ; slope | fit A+B/T to 
;A=yin[0]-B/arg4[0]              ; constant        | first 2  points
;yx=a+b/tx                  ; generate the extended points
yx=abc[0]+abc[1]/tx             ; generate the extended points
oplot,tx,yx,psym=1             ; and plot this extension
ttt=[tx,arg4]                   ; data plus extrapolation
xxx=(ttt-toff)*tmul             ; T' including the 3 new points
yin=[yx,yin] 
q=0. ; dummy arg3 
qq=KOFT(ttt,8,q,arg4,ref,tlmm) ; Hobbs74 with dummy arg3 
oplot,ttt,qq,line=5
qq=KOFT(ttt,17,q,arg4,ref,tlmm) ; Slack80
oplot,arg4[*,0],arg4[*,1],psym=6 ; square for data table 
oplot,ttt,qq,line=4
print,'diamonds=web data   dotted line= cubic fit '
print,' ._._=A+B/T fit  += 3 cooler points   line= cubic fit with these included'
print,'Squares=Slack80 ...__ = cubic fit'
print,'long dash=Hobbs74' 
end

482: yfit=EVMONO(ccc,xxx); Make points on uniform T

49:  begin & cid='SpH'; Cp H2O data points
yin=SPECHEAT([tmin,tmax<275.],9,7,arg4,refc,tlim) & end
; CAUTION, points are not monotonic in T

491: ttt=arg4 ;+ Move T points into ttt

50: begin & xxx=ttt & tex=''  & end ; x=Kelvin

5:  begin & xxx=(ttt-toff)*tmul ;+ x=scaled T
qq=ST0([toff,tmul],/nojoin)
tex='Scaled by (T-'+qq[0]+')*'+qq[1] & end     

51: begin & kfit=pari[3]     ; Fit to Tk
; kfit:   Form of fit:  2=quadratic 3=cubic
;   -4=Berman:  y=c0+c1/sqrt(T) +c2/T^2 + c3/T^3;  cannot use offset in scaling
if n_elements(ttt) ne n_elements(yin) then goto,halt
if kfit gt 1 then begin         ; fit monomial
    ccc=poly_fit (ttt,yin,kfit,yfit,yband,sigma,corrm) ; IDL routine
 ;   print,'Sigma=',sigma
 ;   print,'Corrm=',corrm
endif else if kfit eq -4 then begin ; fit Berman form
    if min(ttt le 0.) then goto,halt
    i=size(yin,/type)
    if i ge 5 then one=1.D0 else one=1.
    ff=fltarr(ntt,4)            ; to hold basis vectors
    ff[*,0]=one
    ff[*,1]=one/sqrt(ttt)
    ff[*,2]=one/ttt^2
    ff[*,3]=one/ttt^3
    ccc=LUFIT(ff,yin,model=yfit); IDL LU decomposition
endif else if kfit eq -2 then begin ; 1/(A+BT)
    arg4=reform([ttt,yin],ntt,2)
    yout=KOFT(ttt,24,parg[0:1],arg4,refc, got=ccc) 
    if ccc[3] le 0 then print,'Fit warning;',ccc ; extension failed
    yfit=1./(ccc[0]+ccc[1]*ttt)
endif else begin
    print,'Invalid fit index'
    goto,halt 
endelse 
ccc=reform(ccc)
ar=MEAN_STD(yfit-yin,std=std)
print,kfit,std,ccc,refc,form='(i2,f9.5,'' T>'',4g14.7,'' <T'',2x,a)' 
end

52: begin & ttt=double(ttt) & yin=double(yin) & end ; Use DoublePrec.

55: print,kfit,std,ccc,refc,form='(i2,f9.5,'' >'',4g14.7,'' <'',2x,a)' ; Print fit results

56: begin & cck=ccc & refk=refc & end ; Save coef as: k

57: begin & ccp=ccc & refp=refc & end ; Save coef as: Cp

59: Begin   ; Print Inertia(T) of solids
rho=parr[4] ; density, use grain value
kooft=EVMONO(cck,xxx) ; conductivity of T
cpoft=EVMONO(ccp,xxx) ; specific heat of T
tioft=sqrt(rho*kooft*cpoft)
dioft=kooft/(rho*cpoft)
print,refk,' ',refp,' Dens=',rho
print,' i    T         k     Cp         I      dif*E6' 
;     ' 0  120.0    4.52 1021.85   3397.5
fmt='(i3,f7.1, f8.2,f8.2,f9.1, f10.3)'
for i=0,ntt-1 do print,i,ttt[i],kooft[i],cpoft[i],tioft[i] $
,1.e6*dioft[i],form=fmt
end

60: begin & np=pari[5]          ; Initiate bbb
kbb=-1
bbb=fltarr(ntt,np)
sss=strarr(np) & end

61:  plot,xxx,yin,psym=4,xtit='Temperature '+tex  $ ; Plot data
,ytit='Data to fit',title=refc 

62:  oplot,xxx,yfit             ;+ Plot fit

631: CHART,transpose(reform(ccm,4,nr*nv)) $ ; CHART cement k
,xtit='Cement volume * particle radius'

632: begin                      ; Plot rrr,ggg  UPDATE
plot,rrr,ggg,yran=[.001,parr[10]],/xlog,psym=-6 $
  ,xtit='Rg box=Presley',ytit='Cond at 220'
for j=0,nv-1 do begin 
    yy=reform(ccm[0,j,*],/over)
    clr=kkc[j mod nclr]
    oplot,rrr,yy ,color=clr     ; fit to PARTCOND
    LABEL_CURVE,string(vvv[j],form='(g7.2)'),rrr,yy,100.,color=clr
endfor
oplot,rrr,aaa[ito,*,0],line=5   ; Piqueux spreadsheet
oplot,rrr,aaa[ito,*,1],line=1   ; Piqueux writeup

zzz=reform(bbb[10,*,*]/bbb[0,*,*],/over) ; K for high T / low T
help,zzz & print,zzz ; [vol,rg]
end

64:  CLOT,bbb[*,0:kbb],sss,xx=ttt,yran=[0.,parg[yrb]] $ ; Plot of bbb
      ,titl=['Kelvin ',cytit,cztit], locc=parc[6:9]

65: q=XYY5('P',parc[6:9],['Kelvin',cytit,cztit],[parr[11:12],0.,parg[4]],4); Data plot

651: q=XYY5('P',[.3,.5, -.03,.08],['Kelvin',cytit,cztit],[1.,1200.,1.,3000.],4,/xlog,/ylog) ; Big picture

655:  q=XYY5('P',0,0,0,-4)      ; Overplot

; 7-- Ice mixes  AND run partcond with T-dependent solids 

70: park[1:3,*]=0.              ; Constant solid conductivity

701: qq=PARTCOND(parm,[16,-2],xyo) ; PARTCOND: Modify params only
702: qq=PARTCOND(parm,[99,-3],xyo) ;+ Full access 

71: begin & park=park0 & matk=matk0 ; Defaults for Particulates
parp=parp0 & matp=matp0 & end

710: Begin & park[*,0]=cck ; Set Grain coefs.
matk[0]=refk; +ST0(parr[11:13])
parp[*,0]=ccp & matp[0]=refp & end; specific heat

711: Begin & park[*,1]=cck ;+ Set Cement coefs
matk[1]=refk; +ST0(parr[11:13])
parp[*,1]=ccp & matp[1]=refp & end

717: begin                   ; Compute particulate k for 1 or set cement volumes
if parr[5] gt 0. then vvv=parr[5] else vvv=vvset ; independent variable
cvol=vvv ; cement volume 
zzz=(ttt-toff)*tmul    ; scaled data-point Tk
;nall=fix(parr[12]+parr[13]); 
nip=n_elements(vvv)             ; number of independent points = cement volumes
rx=fltarr(nip)                  ; to hold real interpolation location
ix=intarr(nip)                  ;  " " index part
fx=fltarr(nip)                  ;  " " fractional interval part
pp4=fltarr(ntt,nip)             ;  " " resulting particulate conductivity
q=PP_CO2(ttt,arg2,arg2,arg4,arg5,condg=condg) ; Get 4 versions of gas conductiv.
kgas=condg[*,3]                 ; CO2 gas conductivity, Piqueux09a 
kgrain=EVMONO(park[*,0],zzz)    ; grain conductivity
kcem=EVMONO(park[*,1],zzz)      ; cement conductivity
konc=[20,43,-4] ; action sequence
if !dbug then konc=[20,43,0,-4] ; stop before return
for k=0,ntt-1 do begin          ; each temperature
;    tn=zzz[k]                   ; normalized temperature
    parm[2]=ttt[k]              ; temperature
    parm[3]=kgrain[k]           ; grain conductivity
    parm[4]=kgas[k]             ; bulk gas conductivity
    parm[5]=kcem[k]             ; cement conductivity
    out=PARTCOND(parm,konc,xyo)
; xyo  out_ fltarr(n,2)  central angle and  delta Temperature
; ppp [nip,2] 0]=volume fraction cement  1]=net conductivity
    if k eq 0 then begin        ; find desired B value
        bvol=out[*,0]           ; cement volume
        for j=0,nip-1 do rx[j]=RNDEX(vvv[j],bvol,noex=2) ;  Find real location
        if min (rx) lt 0 then begin
            message,'Cement range in partcond inadequate',/con
            print,'Min b vol=', bvol[0]
            goto,halt
        endif
        ix=fix(rx)              ; truncate. May be a vector
        fx=rx-ix                ; fraction of upper value. May be a vector
    end
    partk=out[*,1]              ; k for many cemVol
    pp4[k,*]=(1.-fx)*partk[ix]+fx*partk[ix+1] ; [T,cemVol]
endfor
txt=ST0(vvv,/nojoin)
ztit='Cemented Rg='+ST0(parm[1])
fvg=1.-.476                     ; fractional volume of grain simple cubic
dip=1B                          ; flag, and last index of materials
end


721: kons=[166,722, 31,33,56, 41,33,57,710 $ ; Full mix sequence
  ,48,33,56, 49,491,5,33,57,25,711, 725]

722: begin ; transfer Mix 
pari[1:2]=fix(pars[0:1]) ; rock conductive. items
parc[1:3]=pars[3:5] ; rock Cp items
end

725: begin  ; Linear mix for grain, ice=cement and void
; because cubic coefficients are linear system, can add coeffs in proportion
frock=pars[7]                   ; fraction of rock
fvoid=pars[9]                   ; fraction of void
fcem=1.-frock-fvoid
print,'Rock and void fractions=',frock,fvoid
qq=frock*pars[6]+fcem*pars[8]   ; mix density
print,'Mix density=',qq
qq=frock*park[*,0]+fcem*park[*,1] ; conductivity
mixk=matk[0]+'+'+matk[1]        ; mix name
print,'    constant    Linear     T''^2     T''^3 Reference'
print,'Conduct. >',qq,mixk,form='(a,g10.6,3f10.6,'' < '',a)'
qq=frock*parp[*,0]+fcem*parp[*,1] ; specific Heat
mixc=matp[0]+'+'+matp[1]        ; mix name
print,'SpHeat.  >',qq,mixc,form='(a,g10.6,3f10.6,'' < '',a)'
end

73: CLOT,pp4,txt,xx=ttt,ylog=pari[6],locc=parg[6:10] $ ; CLOT partCond
,yran=[0.,max(pp4)],titl=['Kelvin','Net conductivity',ztit] 

735: CLOT,pp4,txt,xx=ttt,ylog=pari[6],locc=parg[6:10],oplot=-2  ;+ oCLOT

738: CLOT,condg,['Johnston46','TEGA/JPL','sqrt','Piqueux90a'],locc=1 ; Plot gas conds

74: begin & cpk=fltarr(4,nip); Calc and Print coefficients for particulates
print,'Tmin,Tmax, Tdel=',parr[11:13]
;      0  0.03781 > 0.2689837 0.2407015 0.0832466 0.0134025 <        10.0
for i=0,dip do print,gc[i]+' k ',park[*,i],matk[i] $
  ,form='(a10,'' >'',4f10.6,'' < '',a)'
for i=0,dip do print,gc[i]+' Cp',parp[*,i],matp[i] $
  ,form='(a10,'' >'',4f10.4,'' < '',a )'
print,'  1000|err|       c0        c1        c2        c3       Ind.Param'
eee=fltarr(ntt,nip)             ; thermal inertia
for j=0,nip-1 do begin          ; each radius or cement volume
    fvc=cvol[j]                 ; fractional volume of cement
    rhop=fvg*parr[4]+fvc*parr[7] ; particulate density
    cpp=(fvg*parp[*,0]+fvc*parp[*,1])/(fvg+fvc) ; specific heat coeffs
    pfit=EVMONO(cpp,zzz)        ; specific heat at all T'
    yyy=pp4[*,j]                ; pp4 is K[T,nip]
    ccc=poly_fit(zzz,yyy,3,yfit, yband,sigma,corrm)
    ar=MEAN_STD(yfit-yyy,std=std)
    ferr=max(abs(yyy-yfit))/ccc[0] ; max fraction residual
    print,1000.*ferr,ccc,vvv[j],form='(2x,f9.5,'' >'',4f10.6,'' <Con'',g12.3)'
    print,j,cpp,vvv[j], form='(i2,9x,'' >'',4f10.4,'' <Sph'',g12.3 )'   
    cpk[*,j]=ccc                ; [4,cemVol] fit to particulate conductivity
    eee[*,j]=sqrt(yfit*rhop*pfit) ; inertia at all T'
endfor
end

75: begin ; Print thermal inertia table
fmt='(a10,1x,99f6.1)'
fmi='(g10.3,1x,99f6.1)'
print,'T',ttt,form=fmt
print,'GasK*1000',kgas[*,0]*1000.,form=fmt
print,'Grain  k',kgrain,form=fmt
if dip then print,'Cement k',kcem,form=fmt
print,'Grain  Cp',EVMONO(parp[*,0],zzz),form=fmt
if dip then print,'Cement Cp',EVMONO(parp[*,1],zzz),form=fmt
print,'Ind.Param  ____________ Thermal inertia ______________'
for j=0,nip-1 do print,vvv[j],eee[*,j],format=fmi
txi=ST0(vvv,/nojoin) ; ,strtrim(string(vvv),2)
end

76: CLOT,eee,txi,xx=ttt,ylog=pari[6],locc=parg[6:10] $ ; CLOT inertia
,titl=['Kelvin','Thermal Inertia',ztit ] 

else: begin & KON91,ptitl,prior,hold,kon,kons,kitel ;=KON99 < needed by MAKE99
      if kon eq 99 then begin 
;          print,'11: files:  parf= '
          print,'12: Integs: pari= ',ST0(pari)
          print,'13:PARTCOND parm= ',ST0(parm)
          print,'14: FitPlt: parg= ',ST0(parg)
          print,'15: Cp    : parc= ',ST0(parc)
          print,'16: Floats: parr= ',ST0(parr)
          print,'166: Mixes: pars= ',ST0(pars)
          print,'17: GrCemK: park= ',ST0(park)
          print,'177: GrCemCp: parp= ',ST0(parp)
      endif & end
endcase
goto, ask

halt:  print,'SOME ERROR CONDITION at kon=',kon,'.  Any key to Go'
q=get_kbrd(1) & goto,sureask
end
