FUNCTION PP_CO2, tin, HEAT,COND,DENS,CP, ptype=ptype,condg=condg,vis=vis,txt=txt
;_Title  PP_CO2  Vapor pressure and thermal properties of  CO2
;_Args
; tin	in. 	fltarr(n) Temperature <kelvin>. May be an array (n)
; HEAT	out. 	fltarr(n) Latent heat of vaporization <joule/kg>
; COND	out. 	fltarr(n) Solid thermal conductivity  <SI = J m^-1 K^-1 > 
; DENS	out. 	fltarr(n) Solid density <kg/m^3>
; CP	out. 	fltarr(n) Solid specific heat <j/kg_k>
; condg	out_ 	fltarr(n,m) Thermal conductivity of the gas, m=4 versions
; vis	out_	fltarr(n) Viscosity of the gas
; txt   out_    String: ID of pressure type
; ptype in_	Which pressure curve to use. 
;                 0=Default=solid(cubic from), 1=clathrate
;                 2=CRC, includes liquid   3=Lange 4=MARS ClauClap
;                 5=Span96, includes liquid  6=Pollack81
;		  Set negative to execute once-only calculations
; -1=pressure   -2= HEAT CAPACITY  -3=viscosity  else=gas thermal conductiivity
; func.	out.	vapor pressure <pascal>
;_Keys  PHYSICAL_PROPERTIES  THERMAL  CO2
;_Desc
;_Lims  
; pressure equation is fit to 100:170 k
;_Bugs
;_Hist	99mar20  adopt from  pp_co2.f
; 2004jul02 HK Make COND of the solid, keyword condg of the gas
; 2004jul04 HK Put in reasonable T-dependant gas thermal conductivity
; 2006feb28 HK Move versions of gas conductivity
; 2006mar14 HK Add Lange version of vapor pressure
; 2006oct26 HK More on viscosity. Major change to Heat capacity
;              output viscosity and Restructure test code
; 2010jul07 HK Add txt keyword,. Add Span96 and Pollack81 vapor pressure
;_End

ntin=n_elements(tin)
; VAPOR  PRESSURE:  see test code below
if not keyword_set(ptype) then ptype=0

; MARS p 959.99  pressure in millibars *100 or +4.60517 to Pascal
case ptype of ; natual log of pressure in Pascal
    1:  begin & txt='clathrate'
plog = 4.60517+ 24.313 - 3081.8/tin - 1.516E-2*tin ; clathrate MARS  960.8
end
;    2: if tin lt 216.548 then plog= 27.9006 -3183.85/tin $; fit to CRC51,D-145 
    2: begin & txt='CRC'; fit to CRC51,D-145; coeff derived @ ptype=-1 below 
        plog=fltarr(ntin)
;t=(3183.85-1990.47)/(27.9006-22.3537); where gas and liquid relations have 
; the same pressure, t= 215.144
        ii=where(tin lt 215.144,j)
        if j gt 0 then plog[ii]= 27.9006 -3183.85/tin[ii] ; gas
        ii=where(tin ge 215.144,j)
        if j gt 0 then plog[ii]= 22.3537 -1990.47/tin[ii] ; " liquid 
    end 
    3:  begin & txt='Lange' ; Lange, Antoine eq.
        plog=2.30259*(9.64177-1284.07/(tin+(268.432-273.15)))+ 4.89277
    end
    4:  begin & txt='Mars959.9'
        plog= 27.9546-3182.48/tin  &  end; MARS p 959.9
    5: begin & txt='Span60' ; Span & Wagner 1996 
; used mostly data above 154K 
            ttrip=216.592 & ptrip=0.51795E6 ; triple point K and Pa
            tcrit=304.1282 & pcrit=7.3773E6 ; critical point
            plog=fltarr(ntin) 
            ii=where(tin lt ttrip,j) 
            if j gt 0 then begin ; sublimation pressure
                xx=1.-tin[ii]/ttrip ; T normalized to the triple point
                plog[ii]=alog(ptrip) $ ; eq 3.12
            +(ttrip/tin[ii])*(-14.740846*xx +2.4327015*xx^1.9 -5.3061778*xx^2.9)
            endif  
            ii=where(tin ge ttrip,j) 
            if j gt 0 then begin ; vapor pressure above liquid
                xx=1.-tin[ii]/tcrit
                yy=-7.0602087*xx+1.9391218*xx^1.5-1.6463597*xx^2-3.2995634*xx^4
                plog[ii]=alog(pcrit)+(tcrit/tin[ii])*yy
            endif  
        end
    6: begin & txt='Pollack81'  ; Pollack et al J. Atm Sci 1981 
; T=149.2+6.48 ln(0.135 P) with P in millibar.  Eq 4
; deviates <5% only within 134:141. This is not a good fit
    plog=((tin-149.2)/6.48)-alog(0.135)+alog(100.) & end
    else:  begin & txt='Mars959.99'
    plog = 4.60517+25.2194 -3311.57/tin -6.71e-3*tin & end ; MARS p 959.99
endcase
vp= exp(plog)	; vapor pressure

; LATENT  HEAT:
; fit to tabulated latent heat <cal/gm_mole> = 6861.39746 -3.90536880 *t
; (at t=140, is 6314.646)
; to convert to <joule / kg_mole>, * 4184	@t=140, is 2.642048e07
; to convert to <joule / kg*>,     * 4184/44.01	@t=140, is 6.003290e05
; clausius-clapyron eq.:  
; C1=L*m/R where m=molecular_weight,  R=UNIVERSAL_GAS_CONST.
; C1*R = 2.6460e7 <joule/kmole>    c1*r/m=6.0123e5 <j/kg>, so agreement is good.
	HEAT = 6.52308E5 -371.28*TIN

; HEAT  CAPACITY:  see test code below
	CP = 459.6 + 1.35845*TIN	
; was very wrong	CP = 349. + 4.8*TIN		;<j/kg>   1021. @  T=140

; DENSITY:
;1.  ICT 1:341 
;  crystal structure (4b,8h).  space group  Ti-6. paramorphic hemihedral
;  cell size as 5.62 angstrom; 4 molecules to a cell, 
;  this yields density 0f 1646.83 <kg/m^3>
;2.  CRC-51 lists density = 1.56 <gm/cc> at -79c
;3. mullins,kirk & ziegler list mean molar volume, 101-194 kelvin as 
;  27.40  <ML/GM mole>     liter=1000.028 cc, den=1/vol, 
;  so den=1/( 27.40*1.000028e-3 /44.01)
	DENS = 1606.		; use 3.  <kg/m^3>, average for 101:194  K

; SOLID THERMAL  CONDUCTIVITY:
; Kravchenko and Krupskii: Thermal conductivity of solid N2O and CO2
; Sov. J Low Temp. Phys 12(1) January 1988
; Fig 1 line for CO2 is thermal resistivity W=(21.2/200) T [cm K min^-1]{? wrong}
; Assume by "thermal resistivity" they mean 1/conductivity
; Table 2 lists 10.37 [mW cm ^-1 K^-1] at 90K, which is right on the curve
; to get W sec^-1 m^-1 K^-1, multiply by E-3 (to W) E2 (to m^-1)=0.1
; so k [W sec^-1 m^-1 K^-1] ~= 10./((21.2/200)T) = 94.34/T
; lam=[110.,120.,130.,140.,150.,160.,170.]  ; some of their points
; tt =[8.49,7.79,7.18,6.67,6.22,5.81,5.43]
; average of their measurments 110: 170 is 93.17
; Mellon email, based on same paper, suggests 93.4/T. I will adopt this
        cond=93.4/TIN 
  
; GAS  THERMAL  CONDUCTIVITY:  see test code below
CONDg1=exp(-12.1166487  + 1.4072660*alog(tin)) ; fit to Johnston46
CONDg2=6.e-6*tin^1.3714 ; TEGA/JPL
condg3=418.4*3.393E-5 * sqrt(tin/273.15)  ; W/m/s/K thru Lange 0C point
condg4=418.4*(4.608e-5* sqrt(tin))/(1.+6212.*10^(-10./tin)/tin); Piqueux90a,eq 14
condg=reform([condg1,condg2,condg3,condg4],ntin,4)

vis=exp(1.43262  + 0.00440360*TIN) ; gas viscosity  Pa.s

;-------------------------------------------------------

if ptype lt 0 then begin ; once-only calculations. Will stop before return
tp=130.+10.*findgen(18) ; plot T's
case ptype of

    -1:  begin                  ; vapor pressure
; conversion from torr to pascal,   * 101325/760 = 133.32237, or +4.89277 to ln
; conversion from torr to millibar, * 1013.25/760= 1.3332237, or +0.28760 to ln
; results of  FIT runs on mullins tabular data, run 90may15
;  100 to 170 k at 5 degree intervals
; form: ln p <torr> = c0 + c1 * 1000/t   fit 120:160
; results: c0=2.30617846e+01  C1=-3.18248415E+00  MAX_ERR=.01137 in ln(p)
;	PP_CO2 = EXP (27.9546 -3182.48/T)	; <pascal>
; form: with added c2*(1000/t)^2, fit to 100:170, max_err=7.47e-4 in ln(p)
;	XT = 1000./T
;	PL1 = 24.931919 + XT*(-6.7142167 -3.3115809*XT)		; ln p<torr>
;	vp= 133.32237*EXP(PL1)	; vapor pressure, constant is torr-to-Pascal
;print,t,xt,pl1,vp
; Lange 10th p1438 log_10 mmHg= A-B/(C+t_Celsius): A=9.64177 B=1284.07 C=268.432
; MARS p958: Triple point 216K,5.18E5 Pa  Critical point 304K, 7.4E6 Pa
; CRC51 D-145: Triple point -56.602+-.005C,5.18E5 Pa  Critical temp=31C

; P-T relation   Data from CRC 51th , p D-145
pmu=[0.013,.37,5.9,60.5,431]  & t11=-180. & t12=10. ; gas , P in microns of Hg
; gas, P in mm of Hg
pmm=[2.31,9.81,34.63,104.81,279.5,672.2,1486.1,3073.1] & t21=-130. & t22=10.
pm2=[3294.6,3530.2,3780.9] & t31=-59. & t32=1.
; liquid P in mm of Hg
pq1=[3987.9,4163.2,4344.3,4531.1,4723.9,4922.7] & t41=-56. & t42=1.
pq2=[5127.8,7545.,10718.,14781.,19872.,26142.,33763.,42959.,54085.] 
	t51=-50. & t52=10.
; concatonate P and T in source units P in Toor, T in celsius
n=n_elements(pmu) & pp1=0.001*pmu & tt1=t11+t22*findgen(n)
n=n_elements(pmm) & pp2=pmm & tt2=t21+t22*findgen(n)
n=n_elements(pm2) & pp3=pm2 & tt3=t31+t32*findgen(n)
n=n_elements(pq1) & pp4=pq1 & tt4=t41+t42*findgen(n)
n=n_elements(pq2) & pp5=pq2 & tt5=t51+t52*findgen(n)
ppp=133.32237*[pp1,pp2,pp3] ; gases in Pa
ttt=273.15+[tt1,tt2,tt3] ; T in K
 ; best fit ClauClap
xx=1./ttt
yy=alog(ppp)
cc=linfit(xx,yy)  ; logp= cco+cc1/T
mm=exp(cc[0]+cc[1]/ttt)
print,'Clausius Claperyon coeff=',cc
plot,ttt,mm/ppp,psym=-4, xtit='kelvin',ytit='cc model/table'
;		this is within 8% of the table for the full range 
         print,'Any key to go' & i=GET_KBRD(1) 
c3=poly_fit(xx,yy,2,yfit); within 3% of the table for the full range
c3=reform(c3,/over)
print,'Clausius Claperyon & 1/t^2=',c3
m3=exp(yfit)
plot,ttt,m3/ppp,psym=-4, xtit='kelvin',ytit='c3 model/table'
         print,'Any key to go' & i=GET_KBRD(1) 
ttq=273.15+[tt4,tt5] ; T in K
xx=1./ttq
ppq=133.32237*[pp4,pp5]
yy=alog(ppq)
cc2=linfit(xx,yy)  ; logp= cco+cc1/T
print,'Liquid Clausis Claperyon coeff=',cc2
plot,1./xx,cc2[0]+cc2[1]*xx-yy, psym=-4, xtit='kelvin',ytit='Log model-log table'
;		this is within 0.5% of the table 
end

-2:  begin ; HEAT  CAPACITY:
; ICT, 1928, vol 5, page 95. given in <joule/gram> for t in celsius
;	t<c>	cp<j/g>		%error
;	-260	.046		8	
;	-250	.153		6
;	-225	.52		4
; -200 to -75	1.660+.0048t	3
; converting the last line to <j/kg> and <kelvin>, get
; for  T<K> 73:198     CP = 349. + 4.8*TIN
thcc=reform([-260,0.046, -250,.153, -225,.52],2,3) ; above table
thcc=transpose(thcc)
thcc[*,0]=thcc[*,0]+273.15 ; convert to Kelvin
thcc[*,1]=1000.*thcc[*,1] ; convert from /g to /kg
; http://www.engineeringtoolbox.com/carbon-dioxide-d_974.html1  
; Specific Heat table: kJ/kg K
thc=[175.,0.709, 200 ,0.735, 225 ,0.763, 250 ,0.791, 275 ,0.819, 300 ,0.846 $
,325 ,0.871, 350 ,0.895, 375 ,0.918, 400 ,0.939, 450 ,0.978, 500 ,1.014]
i=n_elements(thc)
thc=reform(thc,2,i/2,/over) 
thc=transpose(thc)
thc[*,1]=1000.*thc[*,1] ;  ; convert from /g to /kg
thc=[thcc,thc]; concatonate data
plot,thc[*,0],thc[*,1],psym=4,ytit='Cp:  J kg^-1 K^-1' $
,xtit='Kelvin  __+__=a+bT to some data '
;oplot,tp, 349. + 4.8*Tp ; ICT relation ???.  Much to high
ii=[2,3,4,5,6]; indices to use in fit
; ii=indgen(i/2); use all points
n=n_elements(ii)
xx=thc[ii,0] & yy=thc[ii,1]
cc=linfit(xx,yy) 
print,'Fit H=a+bt:=',cc
oplot,xx,cc[0]+cc[1]*xx, line=-1
         print,'Any key to go' & i=GET_KBRD(1) 
plot,xx,yy,psym=4,ytit='Cp:  J kg^-1 K^-1   Fit only' $
,xtit='Kelvin   fit a+bT+c/T'
ff=fltarr(n,3) ; prepare for Least-Squares fit
ff[*,0]=1.
ff[*,1]=xx
ff[*,2]=1./xx
sig=replicate(10.,n)
for inv=0,2 do begin 
    csv=FITLSQ(ff,yy,sig=sig,inv=inv, model=sfit, dbug=1,ufit=ufit) 
    print,inv,'=inv  Fit H=a+bt+c/T:=',csv
    if inv eq 2 then print,'        Coeff uncertainties = ', ufit
    oplot,xx,sfit,line=2+inv
endfor
; Conclusion, use simple linear fit to points over 50:250K
end

    -3: begin                   ; viscosity of the gas
; Viscosity: Theory:
; 1)  nu=1/3 *density * mean velocity of molecules * mean_free_path
; 2)  nu = 5/16 * sqrt(M*R*T/pi) / N_A sigma^2
;  M=Molecular weight  R= Univ. Gas Constant  T= Temperature
; N_A = Avagadros number  sigma = collision diameter
; 2b)  nu = 5/16 * sqrt(M*k*T/pi) / sigma^2   Dushman p 28.5
;          k= Boltzman constant
;2006jan18 viscosity:  crc= CRC 50'th Ed F43  L10=Lange 10'th p.1675
; who  crc  L10   crc   crc   crc   crc   L10    crc  crc   L10 L10  crc   L10
tnu=[-97.8,-78.4,-78.2,-60.0,-40.2,-21. ,-20.7, -19.4,0.   ,0., 12.,   15.,  20.] ; T Celsius
nu=[  89.6,103.3, 97.2,106.1,115.5,129.4,129.4, 126.0,139.0,138.,143.6,145.7,160.] ; micro-poise
ip=[  4,    6,    4,     4,    4,    4,    6,     4,   4,   6,  6,     4,   6]
; nu at 0C=1.377e-4 poise  Dushman p 32.3
; AirLiquide: Viscosity (1.013 bar and 0 °C (32 °F)) : 0.0001372 Poise

; nu at 145K 7.128E-6         ?? from where
; nu at 20C=0.0148 centiPoise ?? from where
; 1 poise = 0.1 Pascal second, which is the SI unit of [dynamic] viscosity 
; Choose to use units of 1.e-6  Pascal second

tt=273.15+tnu  ; model:  nu = constant*sqrt(T)  ; const = mean(nu/sqrt(T))
nusi=0.1*nu ; value in micro Pascal second
ya=min(nusi,max=yb) & yc=.05*(yb-ya)
xa=min(tt,  max=xb) & xc=0.05*(xb-xa)
xran=[xa-xc,xb+xc]  & yran=[ya-yc,yb+yc]
xran=[140., 300.] &  yran[0]=6. ; by-hand ranges
plot,tt,nusi,xran=xran,yran=yran, /nodata $
,xtit='Kelvin. Fits:    - - =sqrt(T), ___= T,log(nu)   ...=T' $
,ytit='viscosity, micro Pascal-second'
for j=1,7 do begin ; each potential symbol
    ii=where(ip eq j,n)
    if n gt 0 then oplot,tt[ii],nusi[ii],psym=j
endfor 
; Model 1: proportional to sqrt(T)
qq=sqrt(tt)
con=MEAN_STD(nusi/qq)
print,'Fit nu=a Sqrt(T):', con
modl=con*sqrt(tp)
oplot,tp,modl,line=2  ; Model is a lousy fit
; Model 2: log nu = a+bT
lognu=alog(nusi)
cc2=linfit(tt,lognu) ; linear fit, equal weiight
print,'Fit log(nu) =a+bT',cc2
mod2=exp(cc2[0]+cc2[1]*tp)
oplot,tp,mod2  ; Model is better  fit
; Model 3: nu=a+bT to CRC only
ii=where(ip eq 4,n)
cc3=linfit(tt[ii],nusi[ii])
 print,'Fit nu=a+bT to CRC only: ',cc3
mod3=cc3[0]+cc3[1]*tp
oplot,tp,mod3,line=1
; Conclusion, use model 2 as best compromise
end

-4: begin ; gamma  
Tgam=[-75,0,15,25,100] ; T in Celcius  From Lange 10'th p1525-6, except 25C 
gamma=[1.37,1.310,1.304,1.293759,1.281]
plot,tgam,gamma
Tcp=[175.,200.,225,.250,275.] ; temperature in K
scp=[.709,735,763,791,819] ; Cp J/Kg from http://www.engineeringtoolbox.com/carbon-dioxide-d_974.html
end
else: begin ; gas thermal conductivity ==================================
; LANGE, 10th edition, p.1556; 2.546e-5 @ -78.5 C in units of 
;	 <g.cal s^-1 cm^-2 deg/cm >. Also 3.393 @ 0C.. Argon 3.88 @ 0C
;;        CONDg = 2.54E-5 * 4.184 /0.01 ; ????
; gamma == cp/cv = 1.37 ; lange p.1526, comes from  ICT?
; convert to Ws  * 4.184  convert to /m *100, yields .01065
ttl=[ -78.5,0]+273.15 & yyl=[2.546,3.393]*418.4e-5

; from the fits @kon=3 in vents.pro  2004jul04     2006oct: code lost
; CONDg1 = 4.184E-4*exp(-4.30264+ 1.39980*alog(tp))
; CONDg1 = exp(-12.0817+ 1.39980*alog(tp)) ; same values

CONDg1 =  exp(-12.1166487  + 1.4072660*alog(tp)) ; fit to Johnston46, 

CONDg2 = 6.e-6*tp^1.3714  ; from TEGA PDR Kennedy-thermal PL-8, about 10% less
; this chart also list of N2 gas as:   = 1.53E-5 * T^0.9 

; Dushman: ideal gas has conductivity proportional to sqrt(T) at high P
; when dimensions are less than mean-free-path, conduc. proportional to P 
condg3= 418.4*3.393E-5 * sqrt(TP/273.15)  ; W/m/s/K thru Dushman 0C point

condg4=418.4*(4.608e-5* sqrt(tp))/(1.+6212.*10^(-10./tp)/tp)

;        0      1       2         3        4      5      6  7
qtit=['vents','TEGA','Dushman','Hecht','Jones','Lange','Johnston','Piq14']
ql  =[   0,     2,      3,        5,       7,     7,     4, 1    ] ; line
qp  =[   0,     0,      0,        0,       6,     7,     4, 5    ] ; lpsym

 plot,tp,condg1,xtit='Kelvin  ___=1  - -=2  -.-.=3  square=JPL+' $
,ytit='Gas thermal conductivity'
for k=0,n_elements(ql)-1 do  $
  CURVEGUIDE,k,qtit[k],ql[k],ksym=qp[k],looc=[.7,.25,-.03,.08]
 oplot,tp,condg2,line=ql[1] ; Tega
 oplot,tp,condg3,line=ql[2] ; Dushman

 oplot,tp,condg4,line=ql[7] ;
 oplot,tp,condg4,psym=qp[7] ;
; Date: Tue, 26 Apr 2005 10:18:10 -0700  From: Jack.A.Jones@jpl.nasa.gov>
; For 1 bar, you can use 0.00877 w/m-K for 155K, and 0.0115 for 185K. 
;  only a decrease of about 0.2% (2 parts in 1000) for 0.006
tt=[155.,    185,   220,    250,  273.15, 300,      350.]
yy=[.00877,.0115,.010805,.012884,.01465,.016572, .02047]  ;?? where upper 5 from
;http://encyclopedia.airliquide.com/Encyclopedia.asp?GasID=26
;Thermal conductivity (1.013 bar and 0 °C (32 °F)) : 14.65 mW/(m.K

plots,tt,yy,psym=qp[4] ; square Jones
plots,ttl,yyl,psym=qp[5] ; X Lange

;2006oct27 from Michael Hecht
; Miles used a formula from N.V. T'sederberg "Thermal conductivity of gases 
; & liquids" [QC321.T882], validated over a broad range of elevated pressure. 
; T'sederberg gets: k=k0 + 1.380E-5 * w^1.26, where w is the specific weight 
; in kg/m^3 at the desired temperature and pressure, and k is in kcal/m-hr-K.
;[But that was for reactor physics]
; The kinetic theory ought to work even better at low pressure, so it's 
; probably a good bet to extrapolate down.
; For ideal gas, W proportional to 1/T, so above yield form a+b/T^1.26
;xx=1./tt^1.26 ; NOPE, tt/yy set not compatible
;ccc=linfit(xx,yy)
;print,'Fit a+b/T  =',ccc
;oplot,tp,ccc[0]+ccc[1]/tp^1.26,line=5 ; Hecht
;
; L. Johnston and E. R. Grilly, J. Chem. Phys. 14, 233 (1946). Table IV
; Units not stated, but must be 1.e-6 Cal / m K
ddd=[186.38,2.073,200.86,2.285,216.49,2.527,230.68,2.752,245.57,3.002 $
,259.60,3.245,274.22,3.505,287.75,3.754,302.38,4.031,311.24,4.199 $
,333.87,4.649,347.32,4.932,363.36,5.273,379.19,5.614]
i=n_elements(ddd) & nd=i/2
ddd=reform(ddd,2,nd)
tj=reform(ddd[0,*])
yj=reform(ddd[1,*])* 4.184e-3
oplot,tj,yj,psym=qp[6]
ltj=alog(tj)
lyj=alog(yj)
cc=linfit(ltj,lyj)
lf=cc[0]+cc[1]*ltj              ; values an fit
yf=exp(lf)                      ; convert back to true
oplot,tj,yf,line=ql[6]
err=(yf-yj)/yj                  ; fractional  residual
merr=total(abs(err))/nd         ; mean absolute residual
cc2=poly_fit(ltj,lyj,2,yfit=yfit) ; residuals look quadratic; fit one more term
er2=yfit-lyj ; error in log is error as a ratio
y3=exp(yfit)
er2=(y3-yj)/yj
mer2=total(abs(er2))/nd 
pause,-1
plot,tj,err,xtit='Kelvin',ytit='Fit - data' $
,title='log(K)=a+b*log(T) [+c(logT)^2]'  
oplot,tj,er2,line=2
print,'Fit log(K)=a+b*log(T) [+c(logT)^2]'
fmt='(a,f6.3,a,3f12.7)'                        ; almost same as cong1
print,'linear:',100.*merr,'=%err',cc,form=fmt;  -12.1166487   1.4072660
print,'quad:  ',100.*mer2,'=%err',cc2,form=fmt
end
endcase

stop
endif

return,vp
end

; Gas encyclopedia at  2006oct23
; http://encyclopedia.airliquide.com/Encyclopedia.asp?languageid=11 

; Heat capacity at constant pressure (Cp) (1.013 bar and 25 C (77 F)) : 0.037 kJ/(mol.K)
; Heat capacity at constant volume (Cv) (1.013 bar and 25 C (77 F)) : 0.028 kJ/(mol.K)
; Ratio of specific heats (Gamma:Cp/Cv) (1.013 bar and 25 C (77 F)) : 1.293759
; Viscosity (0 C (32 F) and 1.013 bar) : 0.0001372 Poise
; Thermal conductivity (0 C (32 F) and 1.013 bar) : 14.65 mW/(m.K) 
;   implimented above yields  14.57 mW
; Vapor pressure curve is .007 bar at 145K , 0.02 at 155 

; 2008sep Values for Cp from Lange p 1525+
; Specific heat at constant pressure, at 1 atm.; cal 15C per gram per deg   * 4185 =  J/kg/deg
; CO2:      at -72C 0.184,  @  0C 0.1973     0.85 kJ/(kg.K) \1.013 bar and 25 °C 
; Argon:   at -180C=0.133   @ 15C 0.1252     0.52 kJ/(kg.K) | From 
; Nitrogen: @ -181C 0.256,  @ 15C 0.248     1.042 kJ/(kg.K)/ http://encyclopedia.airliquide.com/
; q=4185.*[.184,.133,.25]*[.953,.016, .027]
;  print,total(q) =  771.003

