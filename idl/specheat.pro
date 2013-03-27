function specheat, tin, kode, arg3,arg4, ref,tlim
;_Titl  SPECHEAT   Specific heat capacity of minerals
; tin   in. fltarr(n) or scalar   Request temperatures in Kelvin
; kode  in. Integer Which material/version. See case statement below
; arg3  in. int or float   Varies with kode.   
;                  1: Debye Temperature
;                  3: 0 or neg=sample 10020  else=10046
;                  5: 0:3 =index into coef array  4.x=x fraction FE
;                  8: reference temperature in C
;           For kode=9: H2O ice, arg3 is: 
;    0=else=Giauque36 spline, 1=use linear Hobbs & arg4=k 
;    2:5= data points for: 2=Web 3=Giauque36 4=Haida74 5=Yamamuro87 & arg4=Tk
;    6= k data points from web, arg4=Tk 
;    7=Concat of 3:5 only points with Tin range & arg4=Tk
;    8=plot&stop   9=Plot k of Hobbs and Web 
; arg4  Varies  Input for: kode=1, Cp at 0C
;                          kode=8, Cp at reference temperature   
;              Output for: kode=3, thermal conductivity
;                          kode=5, mole.Wt or 2nd form
;                          kode=9, may be data Tk points, see above
; ref   out. String  Reference for this method
; tlim  out  fltarr(4) 0]=low applicable   1]=low measure 
;                      2]=high measure     3]=high applicable
;                   these are somewhat arbitrary for some materials
; func. out. fltarr(n) Heat capacity in SI units: J / kg-K 
;_Calls  EVMONO  MOLEWT  RNDEX  RTERP  ST0  
;_Hist 2007jul24 Hugh Kieffer  Start with Horai 
; 2010feb11 HK Add many, restructure
;_End

nin=n_elements(tin)  & lin=nin-1             ; number of request points
out=fltarr(nin)
tzc=273.15                      ; 0C in K
jpkc= 4.184e3                   ; joules per kilo-calorie

case kode of                    ; which material or relation

1: begin & debt=arg3  ; Debye curve
tlim=[1.,20.,1500.,1600.]
tpiv=tzc & cpiv=arg4         ; point to scale to
ref='Debye'+ST0([debt,tpiv,cpiv],nojoin='_')
for k=-1,lin do begin 
    if k lt 0 then t=tpiv else t=tin[k] ; first loop is ref
    xd=debt/t
    fint=qromo('debyefunc',0.,xd) ; Romberg integration on open interval
    cv=(t/debt)^3*fint
    if k lt 0 then yb=cpiv/cv else out[k]=yb*cv
endfor & end

3: begin & Ref='Horai70 ' ; from Horai et al 1970
tlim=[0,143.,423.,500.]
isamp=10000+[20,57, 46,65] ; thermal measurements made over -130:+150C
; Apollo sample numbers 20,57 are fine-grained vesicular xtline igneous rocks
dens=[2.99,2.88, 2.21,2.36]  ; 46,65 are breccias, Type C
;Cp in cal/[g C]    relations valid above T=273, probably OK for T>240.
; Below 240, fit Debye T^3 approximation
if arg3 le 0  then begin 
    c0=0.213 & c1=0.492e-4 & c2=-0.500e4 ; Lunar sample 10020 Type A
    debt=531.; Debye temperatures
    i=0                         ; index in above data vectors
    difu=1./(31.4+0.378*tin)    ; diffusitivy, in cm^2/sec  With 50% errors
    tmeas=[171,173,222,315,315,414,415] ; temperature  10020
    kmeas=[9.48,10.75,8.99,8.29,6.21,4.26,4.91] ; diffusivity 1.e-3 cm^2/sec
endif else begin
    c0=0.231 & c1=0.559e-4 & c2=-0.581e4 ; Lunar sample 10046 Type C
    debt=452.
    i=2
    difu=1./(54.5+0.648*tin)
endelse
ref=ref+'_'+strtrim(isamp[i],2)
tlow=240.
clow=c0+c1*tlow+c2/tlow^2 
xd=debt/tlow
fint=qromo('debyefunc',0.,xd)   ; Romberg integration on open interval
cvlow=(tlow/debt)^3*fint
for k=0,lin do begin 
    t=tin[k]
    if t ge tlow then cp=c0+c1*t+c2/t^2 else begin 
        xd=debt/t ; follow Debye relation
        fint=qromo('debyefunc',0.,xd) ; Romberg integration on open interval
        cv=(t/debt)^3*fint
        cp=clow*(cv/cvlow)
    endelse
    out[k]=cp
endfor 
out=out*4.18e3                  ; convert to J/kg C
difu=1.e-4*difu ; convert from cm^2 to m^2
arg4=dens[i]*out*difu ; thermal conductivity
end

4: begin & ref='King54_Sphene' ; Sphene CaTiSiO2 
;_Desc   hand-typed From
; Low Temperature Heat Capacity, Entropy at 298.16 K., and High Temperature 
;  Heat Content of Sphene (CaTiSiO5) E. G. King, R. L. Orr, K. R. Bonnickson
; J. Am. Chem. Soc., 1954, 76 (17), pp 4320-4321
; DOI: 10.1021/ja01646a021
tlim=[40.,52.3,298.16,305.] ; soft limits
;  mw=MOLEWT(['Ca','Ti','Si','O'],[1,1,1,5],holds,holdi,holdd)
mw=196.07 ; molecular weight

; [0 is temperature in K   entire measurement table
; [1 C_p   cal/deg-mole
ddd=[52.30,3.888,56.37,4.448,60.99,5.161,65.43,5.899,70.04,6.653  $
,74.51,7.377,79.65,8.320,82.94,8.814,93.21,10.54 $
,103.65,12.27,114.300,14.04,124.31,15.62 $
,135.9,17.37,146.05,18.84,156.20,20.18,165.86,21.41,176.14,22.67 $
,185.86,23.79,196.01,24.87,206.19,25.88,216.12,26.89,226.14,27.82,235.99 $
,28.67,245.69,29.49,255.73,30.28,265.82,31.04,278.23,31.94,287.65,32.60 $
,296.95,33.09,298.16,33.21]
i=n_elements(ddd) 
nd=i/2                          ; number of data points
ddd=reform(ddd,2,nd)            ; separate x and y
ttt=reform(ddd[0,*])            ; temperature points
yyy=reform(ddd[1,*])            ; measured values
for k=0,nin-1 do begin 
    fx=RNDEX(tin[k],ttt)        ; allows extrapolation
    out[k]=RTERP(fx,yyy,/ext)   ; do extrapolation
; out[k]=RNTERP(ttt,tin[k],yyy)   ; no extrapolation, uses extreme table  values
endfor

if !dbug then begin 
    plot,ttt,yyy,psym=-2
    for i=0,nd-1 do print,i,ttt[i],yyy[i],format='(i2,f10.2, f9.2)'
endif
out=(jpkc/mw)*out ; convert from input unit to SI=J/kg/K
end

5: begin & j=fix(arg3)                     ; sub-index 
; clinochlore  (Mg5 Al)[Si3 Al O10](OH)8  X_Fe=0.
; chamosite    (Fe5 Al)[Si3 Al O10](OH)8  X_Fe=1.
ss=['Al','Si','O','H']
nn=[1+1,  3,  10+8,8]
wc=MOLEWT(ss,nn,holds,holdi,holdd) ; mole wt of constant part
wm=MOLEWT('Mg',5,holds,holdi,holdd) ; clinochlore X=0
wf=MOLEWT('Fe',5,holds,holdi,holdd) ; Chamosite   X=1.
if j lt 0 or j gt 5 then message,'Invalid sub-index, max=3 '+ string(j)

if J le 3 then begin ; ================= Bertoldi01  =================
ref='clinochlore_Bert01_Fe='
; Measurements 143-623K  fit to Berman and Brown's (1985) form 
tlim=[135.,143.,623.,700.] ; tends to derivative=0 at 120K
;      X_Fe    con   1/sqrt(T)  1/T^2      1/T^3
Chamo=[0.000,1191.3,-10.665e3, -6.5136e6,+7.7206e8 $ ; 0  J/mole-K
      ,0.116,1200.5,-10.908E3, -5.6941e6,+7.1166e8 $ ; 1
      ,0.889,1224.3,-10.685E3, -6.4389e6,+8.0279e8 $ ; 2
      ,1.000,1248.3,-11.116e3, -5.1623e6,+7.1867e8 ] ; 3   Next 2 from 2007
;      0.,   1151.7,-8.4564e3,-13.206e6,+15.233e8  $ ; Chamo298.15-900K
;      1.,   1160.5,-9.9819e3  -5.9534e6,+3.8677e8 $ ; clinochlore  298-1000K
;     ,0.25, 610.72,  -5140.0,-5.88486e6,+9.5444e8 $ ; above 298K berthierine
;sb=['Fe','Al' ,'Si','O','H']    ; berthierine 2005 paper
;nb=[2.5,.5+.5, 1.5, 5+4,4]      ; " 
;wb=MOLEWT(sb,nb,holds,holdi,holdd)
j4=5*j                          ; start of row
x=chamo[j4] ; fraction of Fe
ref=ref+string(x,form='(f5.3)')
cc=chamo[j4+1:j4+4] ; coefficients for this fraction
for k=0,nin-1 do begin 
    t=tin[k]
    out[k]= cc[0]+cc[1]/sqrt(t)+cc[2]/t^2+cc[3]/t^3
endfor
molw=wc+x*wf+(1.-x)*wm ; molecular wt
out=out*(1000./molw) ; convert from J/Mole-K to J/kg/K
arg4=molw
endif else begin ;========== 2007 paper paper  Cp in J/mol-K =================
ref='Chlorite_Bert07_Fe='
mater=['Chamosite','Clinochlore'] ; 4,5   below from Table 2 
ddd=[5,.0523,.0552, 10,.4499,0.429,  20,4.386,3.296,  30,14.58,9.725 $
,  50,53.90,34.72,  70,109.3,73.62, 100,191.5,146.4, 110,217.6,171.7 $
, 120,243.1,196.8, 130,268.1,221.5, 140,292.6,245.6, 150,316.6,269.2 $
, 160,339.6,290.9, 170,361.0,314.0, 180,381.2,335.4, 190,400.5,355.9 $
, 200,418.9,375.6, 210,436.7,394.4, 220,454.0,412.4, 230,470.7,429.7 $
, 240,486.9,446.1, 250,502.7,461.9, 260,518.0,477.0, 270,532.8,491.5 $
, 280,547.6,505.4, 290,561.6,518.9, 300,573.2,532.4, 320,596.5,556.2 $
, 340,617.7,577.5, 400,670.2,630.2, 500,732.9,693.4] ; 0=T, 1=Cham=Fe, 1=Clin=Mg
; Cp are in J/mol-K
i=n_elements(ddd)
ddd=reform(ddd,3,i/3) & tk=reform(ddd[0,*]) ; Kelvin
x=(arg3-4.)<1.                  ; X is fraction of Fe=Chamosite
ref=ref+string(x,form='(f4.2)')
molw=wc+x*wf+(1.-x)*wm          ; molecular wt
ddy=x*ddd[1,*]+(1.-x)*ddd[2,*]  ; linear weight values at all temperatures
g2=spl_init(tk,ddy)            ; spline interpolation
out=spl_interp(tk,ddy,g2,tin) 
out=out*(1000./molw) ; from J/mol to J/kg
bbk=[1151.4,-8.4564,-13.206,15.233 $ ; Chamo Berman-Brown Coeff 
    ,1160.5,-9.9819,-5.9534,3.8677 ] ; Clino 
;     k0     k1e-3    k2e-6  k3e-8
bbk=reform(bbk,4,2)
cc=x*bbk[*,0]+(1.-x)*bbk[*,1] ; linear interpolation of coeff.
arg4=cc[0]+cc[1]*1.e3/sqrt(tin)+cc[2]*1.e6/tin^2+cc[3]*1.e8/tin^3
arg4=arg4*(1000./molw); from J/mol to J/kg
tlim=[4,5,303,600]
endelse ; =================
end

6: begin  ref='Ledlow' ;  Ledlow lunar material
; Ledlow  cal g^-1 K^-1  lunar material
tlim=[50.,100.,350.,700.]
led=[0.1812,0.1191,0.0176,0.2721,0.1869] ; up to 350K
ii=where(tin le 350.,j)
if j gt 0 then for i=0,j-1 do begin 
    k=ii[i]
    x=(Tin[k]-300.)/300.
    out[k]=EVMONO(led,x)
endfor 
ii=where(tin gt 350.,j)         ; Wechsler et al 1972 form
if j gt 0 then out[ii]=0.2029+0.0383*(1.-exp(-(Tin[ii]-350.)/100.)) 
out=out*jpkc & end

8: begin  & ref='Waples04'+ST0([arg3,arg4],nojoin='_')
tlim=[110.,273.,1200.,1200.]
; Eq 18 is normalized fit to minerals and non-porous rocks
; Their T is in C (just before Eq 18)
; arg3 is the reference temperature in C
; arg4 if the CP at that temperature
yb=(((8.95e-10*arg3) -2.13E-6)*arg3+1.72e-3)*arg3+0.716 ; Cpn1
ttt=tin-tzc ; convert to C
out=(arg4/yb)*((((8.95e-10*ttt) -2.13E-6)*ttt+1.72e-3)*ttt+0.716 )
end

9: begin & ref='H2O:Ice' ; Ice:
tlim=[20.,16.43, 267.11,tzc]
; a3=0   From pp_h2o.pro, which is Hobbs Fig 5.7 
;mw= 18.0153 ; MOLEWT(['H','O'],[2,1],holds,holdi,holdd)
yb= 3.34 + 0.12316*Tin          ; <j/(mole*deg)> fit Hobbs74 fig 5.7, 75:250K
yb = yb*1000./18.0154           ; <j/(kg*deg)> == SI  Divisor is molecular mass

; a3=1  http://www.engineeringtoolbox.com/ice-thermal-properties-d_576.html0 
; 0=C 1=density:kg/m3 2=thermal conductivity: W/mK 3=specific heat: kJ/kg.K
ddd=[0,916.2,2.22,2.050,  -5,917.5,2.25,2.027,  -10,918.9,2.30,2.000 $
  ,-15,919.4,2.34,1.972, -20,919.4,2.39,1.943,  -25,919.6,2.45,1.913 $
  ,-30,920.0,2.50,1.882, -35,920.4,2.57,1.851,  -40,920.8,2.63,1.818 $
  ,-50,921.6,2.76,1.751, -60,922.4,2.90,1.681,  -70,923.3,3.05,1.609 $
  ,-80,924.1,3.19,1.536, -90,924.9,3.34,1.463, -100,925.7,3.48,1.389 ]
i=n_elements(ddd) & nd=i/4
ddd=reform(ddd,4,nd) ; must also put in ascending order
td=reform(ddd[0,*])+tzc & td=reverse(td)
yd=reform(ddd[3,*]) & yd=1000.*reverse(yd) ; to J/ kg K

; a3=2
;From Giauque and Stout,1936, sparse below 99.57. T in K and CP in cal/deg/mole
mw=18.0156
ggg=[16.43,0.303,20.78,0.528,31.64,1.065,39.62,1.449,48.52,1.837,62.63 $
,2.418,70.61,2.723,79.98,3.070,89.20,3.389,99.57,3.814 $
,100.69,3.832,104.69,3.985,110.13,4.136,115.84,4.315,121.74,4.489 $
,127.54,4.655,133.50,4.808,139.48,4.978,145.43,5.135,151.43,5.306 $
,157.48,5.466,163.52,5.633,169.42,5.842,175.36,6.007,181.25,6.185 $
,187.20,6.359,192.96,6.530,199.11,6.710,205.32,6.935,211.56,7.119 $
,217.97,7.326,224.69,7.519,230.08,7.711,236.19,7.887,242.40,8.048 $
,249.31,8.295,256.17,8.526,262.81,8.732,267.11,8.909]
i=n_elements(ggg) & ng=i/2
ggg=reform(ggg,2,ng) & tg=reform(ggg[0,*])
yg=(jpkc/mw)*reform(ggg[1,*]) ;  to J/ kg K
; for i=0,ng-1 do print,i,ggg[*,i],form='(i3,f8.2,f8.3)'
; plot,tg,yg,psym=4

; a3=3 Haida 74 J Chem Thermodyman 6,815 every ~10 K , Series 4 only  J / K mole
hhh=[118.95,18.25,121.43,18.61,129.50,19.57,140.86,20.94,149.87,22.02 $
,158.69,23.06,170.16,24.45,178.89,25.54,190.71,27.03,201.34,28.38 $
,209.97,29.50,218.82,30.65,224.52,31.43,230.51,32.21]
i=n_elements(hhh) & nh=i/2
hhh=reform(hhh,2,nh) 
mw=18.016
th=reform(hhh[0,*]) & yh=(1000./mw)*reform(hhh[1,*])
; for i=0,nh-1 do print,i,hhh[*,i],form='(i3,f8.2,f8.2)'
; plot,th,yh,psym=4

; a3=4  From Yamamuro87, Table 1, below 121, rapidly cooled, every 10 K
; a21 and above, annealed, all points  J / K mole
yyy=[12.71,0.5825,19.88,1.911,29.42,3.973,40.01,6.072,50.12,7.913 $
,60.62,9.718,69.35,11.15,70.79,11.38,79.34,12.72,90.40,14.33 $
,100.86,15.78,109.56,16.94 $
,116.77,17.86,121.47,18.49,123.24,18.70,125.02,18.93,126.80,19.16 $
,130.38,19.60,132.19,19.83,133.99,20.07,135.80,20.31,137.62,20.56 $
,139.44,20.82,141.27,21.07,143.11,21.31,144.95,21.53,146.80,21.75 $
,148.65,21.97,150.52,22.19,152.39,22.42,154.27,22.64,156.15,22.88 $
,158.04,23.12,159.93,23.35,161.82,23.60,163.72,23.84]
i=n_elements(yyy) & ny=i/2
yyy=reform(yyy,2,ny) 
ty=reform(yyy[0,*]) & yy=(1000./mw)*reform(yyy[1,*])
; for i=0,ny-1 do print,i,yyy[*,i],form='(i3,f8.2,f8.3)'
; plot,ty,yy,psym=4
    case arg3 of
        1: begin  & out=yb & ref=ref+'_Hobbs74'     ; Hobbs Cp and k
            arg4= 0.4685 + 488.19/Tin ; <J m^-1 s^-1 K^-1> Hobbs k:  p.360  Eq 5.6
; Slack 22.6b : at 273.15  21.4E-3 W /cm K, 22.5(Ratcliff), 23.8(Dillard)
        end
;  T. Ashworth. In: Proc. Int. Cryog. Eng. Conf. 4th (1972), p. 377. not available

    2: begin & out=yd & arg4=td & ref=ref+'_Web' & end
    3: begin & out=yg & arg4=tg & ref=ref+'_Giauque36' & end
    4: begin & out=yh & arg4=th & ref=ref+'_Haida74' & end
    5: begin & out=yy & arg4=ty & ref=ref+'_Yamamuro87' & end
    6: begin & out=reverse(reform(ddd[2,*])) & arg4=td
            ref=ref+'_k_Web' & end           ; Web k
    7: begin & ref=ref+'_3sources'; concat good data points in T range
        out=[yg,yy,yh]  &  ttt=[tg,ty,th]
        xa=min(tin,max=xb)      ; request range
        ii=where(ttt ge xa and ttt le xb,j)
        if j lt 1 then message,'No data in T range'
        out=out[ii] &  arg4=ttt[ii] & end
    8: begin ; plot all data and relations
        plot,tin,yb,ytit='Cp', $ 
xtit='T:K  line=Hobbs diamond=web  square=Giaque  +=Yamamuro  Triangle=Haida' 
        oplot,td,yd,psym=4
        oplot,tg,yg,psym=6
        oplot,ty,yy,psym=1
        oplot,th,yh,psym=5
        out=yb & end         ; dummy output
    else: begin & ref=ref+' Giauque36'
        g2=spl_init(tg,yg)      ; spline interpolation of Giauque36
        out=spl_interp(tg,yg,g2,tin) & end
    endcase 
end ; kode 9

else: message,'Invalid kode',/con
endcase

if nin eq 1 then out=out[0]

if !dbug then STOP
return,out
end
