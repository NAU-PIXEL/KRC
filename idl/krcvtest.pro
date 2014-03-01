;_Titl KRCVTEST  Check consistency of KRC within and between versions
;_Desc. Reads  KRC type 52, 0 , -1  
;_Calls  CHART  CLOT  DEFINEKRC  GETP  GETPAN  GETPINTS  GETPSN
;  HISTFAST  HSTATS  KON91  KRCHANGE  KRCCOMLAB  LSAM  MAKEKEYVAL  
;  MEAN_STD2  PAUSE  PLOTSECT  PM180  PRINTJCOLS  PRINTRIL  READKRC1  
;  READKRC52  READKRCCOM  READTXTCOL   SETCOLOR   ST0 STRWORD1  
; Addition calls via kon91:  COLOR24BIT  DELAST0  GRAPH  MAKE99  
;    SETWIND  SUBTITLE  TOOTHB
;_Hist 2013sep03 Hugh Kieffer
 common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_End        .rnew krcvtest

ptitl='krcvtest' & prior=['q','q']
homedir=getenv('MYHOME')        ; get current home
hcpu=getenv('HOST')             ; get current CPU name
;solib=getenv('SOLIB')           ; location of shared object library
krcdat=getenv('PROJDAT')        ; KRC data files
krcsrc=getenv('PROJSRC')        ; KRC input files
idltop=getenv('IDLTOP')         ; top of current IDL run directory
;if homedir eq '/u/hkieffer/' then krcdat='/work/hkieffer/krc/test/' ; Mac

labf=['VerA=new DIR ',' " case file',' " multi-type stem',' " OnePoint [.prt]' $
,'spare','VerB=prior DIR',' " case file',' " multi-type stem' $
,' " OnePoint [.prt]','spare','DIR for IDL output'] 
parf=[krcdat,'V222test1','V222test2','V222Mone','---' $
,krcdat,'Vntest1','V211test2','V1Mone','---',idltop] 
parf0=parf

labi=['Flag: DJUL is oldstyle','@46 # seasons','@46 N hours','@522 item index' $
,'@71 rows in OnePoint']
pari=[0,20,6,1,23]

lab52=['Hour','item','Lat.','Season','Case'] ; dimensions of type52

kist=[101,103,109,115,117,119,207]; KRCCOM items in n*100+i format

clrr=[255,254,100,200,150,60] & nclr=n_elements(clrr); cases in color
labc=['Base = Case 1','Case 2','case 3','Case 4 & _h','Case 5 & GCM','Case 6']
labl=['Tsurf & 1', 'Tbolo & 2','Tatm & 3','4 & _h',   '5 & GCM',     '6=spare']
thkk=replicate(1.,6)
linn=[0,2,3,4,5,1] & nlin=n_elements(linn)

str1=string(indgen(16)+1,form='(i2)')
tcase=['AtmTconFcon','AtmTdepFcon','AtmTconFvar' $ ; standard test cases
,'noAtmTcon','noAtmTvar','noAtmTuni']

hsk=['M','S','I','X','MA','N']  ;  kodes for desired stats
hfmt=['f6.3','f5.3','f8.3','f7.3','f7.3','i6'] ;  blanks will be trimmed
id52=['Tsur','Tplan','Tatm','DownVis','DownIR']
des52=['Surface kinetic','TOA Bolometric','Atmosphere','Down-going Vis' $
,'Down-going IR'] ; labels
dj2000=2451545.D0               ; JD of epoch J2000
k24=dj2000-2440000.              ; Offset for KRC before 2013
;===============================================================================
kite=' ' & komit=0   &  paw=-1 ; type definitions
prior=['1','1']                 ;impossible values, to initiate MAKE99
text='dum_text' & ytext='dum_ytext' & text2='dum_up' ; subtitle place holders
lkon=0B
kons=[860,20,200,203,207,21,22,29,252]  ; required for definitions; read one file
kon=123 & goto,dokon           ; do them immediately

;===============================================================================
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
nkc=kcc[2] & nkl=kcc[3]         ; in case SETCOLOR was called
case kon of ;...................................................................
; DO NOT USE those defined in KON91::
; -1 -3 -9 100:3 121 122 8 80 85 87 88 801:4 808 850:860 880:899  
; 9 99 991 992 994 995

 0: stop  ;- Stop

-1: begin & print,'Any key to GO' & qi=get_kbrd(1) & end ;  Wait

110: parf=parf0 ; Reset names to default

111: kons=[200,202,207,21,22,29,252] ; Reread VerA group 2 cases

112: kons=[41,-1,411,-1,42,43,-1,44,-1,45,-1,46] ; Test cases

113: kons=[252,50,51] ; Read 3 types 

114: kons=[511,-1,52,-1,53,-1,55] ; Test between types

115: kons=[26,201,202,252] ; Save current t52 and Read VerB cases

116: kons=[61,-1,62,63] ; Compare versions

117: begin & kons=[200,202,207,21,22,29,252,26,201,207,252,62,63]
parf=parf0
parf[6]=parf[1] & parf[7]=parf[2] ; Version B is latest in distro  output
parf[8]=parf[3]
parf[0]=krcsrc ; Version A becomes new run
parf[3]='Mone'
end
;..................................................................

123: begin & lkon=1b & kkon=-1  ;- Start auto-script 
lastkon=n_elements(kons)-1 & end ; last preset command

11: GETPSN,'File names',parf,labs=labf,/align ;- Modify File names parf
14: GETPAN,'pari',pari,-1,999,labs=labi ;- Modify integers pari

18: begin & help,ifile,ttt,tth,uuu,vvv,ts ; Help, and print cases
if n_elements(cased) gt 1 then PRINTJCOLS,cased,1,len=65 & end

188: if n_elements(itemv) lt 1 then print,'Need to do @252' else begin ;+ contents
  help,ttt & print,'(hour,item,latitude,season,case)' & print,'itemt = ',itemt
  help,ddd & print,'(layer,item,latitude,season,case)' & print,'itemd = ',itemd
  help,ggg & print,'(item,latitude,season,case)' & print,'itemg = ',itemg
  help,uuu & print,'(nlat,item,case)' & print,'itemu = ',itemu
  help,vvv & print,'(season,item,case)' & print,'itemv = ',itemv
help,kcom,/struct & end

19: begin & i=7 ; Print input portion of selected KRCCOM arrays REQ 20,21
GETP,'+1=floats +2=integers +4=logicals',i,0,7
 PRINTRIL ,i,kcom1.fd,fclab, kcom1.id,iclab,kcom1.ld,lclab
end

; 222222222222222222222222222222222 set names and read files
200: Begin & iver=0 & verr='A' & end ; Set to VerA 
201: Begin & iver=1 & verr='B' & end ; Set to VerB

202: begin & igrp=1 & verg='1' & end ; Set to case group 1
203: begin & igrp=2 & verg='2' & end ; Set to case group 2

207: begin & i=5*iver  ; set input file stem
stem=parf[i]+parf[i+igrp]
sfile=verr+verg
end

20: begin                      ; Get KRCCOM structure and definitions
krcstu=DEFINEKRC('KRC',param,labkf,labki,labkl,idmin,idmax) ; full descriptions
fclab=STRWORD1(labkf) ; first word = parameter name   
iclab=STRWORD1(labki)
lclab=STRWORD1(labkl)
numkf=n_elements(labKf) & lastf=numkf-1 ; number defined for input
numki=n_elements(labKi) & lasti=numki-1
numkl=n_elements(labKl) & lastl=numkl-1
end

21: begin  ; Open file to determine locations of krccom
front=READKRCCOM(stem+'.t52',khold) 
if n_elements(front) lt 5 then goto,halt
print,'khold=',khold
kcom1=READKRCCOM(1,khold) ; get first case
end

221: GETPINTS,'KRCCOM Items',kist,100,320 ; Change KRCCOM List

22: begin & if n_elements(front) lt 5 then goto,halt ; Get KRC changes
cased=KRCHANGE(khold,/log,list=kist) ; ,kcom1=kcom1)
;print,parf[1]+' Base case= ',cased[0]
cased[0]=parf[1]+' Base' & end

23: KRCCOMLAB, pari[9],kcom.fd,kcom.id,kcom.ld,fclab,iclab,lclab ;+ Print krccom

232:  KRCCOMLAB, pari[9] $ ; Difference 2 KRCCOM's  REQ 26
,kcom.fd-kcomh.fd,kcom.id-kcomh.id,kcom.ld-kcomh.ld,fclab,iclab,lclab

252: begin & ifile=stem+'.t52' ; Open/Read/Close type 52 file
kcom=READKRC52(ifile,ttt,uuu,vvv,itemt,itemu,itemv,ddd,ggg,itemd,itemg,vern=vern)
help,ttt,uuu,vvv,ddd,ggg,vern,kcom
siz=size(kcom)
if siz[siz[0]+1] ne 8 then goto,halt ; must get a structure
sizt=size(ttt) & nhour=sizt[1] & niti=sizt[2]
nlat=sizt[3] & nseas=sizt[4] & ncase=sizt[5]
print,'Nseas, nlat, ncase=',nseas,nlat,ncase
alat=uuu[0:nlat-1,0,0]          ; latitudes for first case
slat=ST0(alat,/nojoin)          ; lats as string
djmm=vvv[*,0,0]                 ; DJUL
lsv =vvv[*,1,0]                 ; LSUBS
dfl0=(djmm-151.269) mod 686.99161 ; days from Ls=0
tsur=reform(ttt[*,0,0,*,0])       ; Tsurf [hour, season]
end

26: begin & siz=size(ttt) & if siz[0] ne 5 then goto,halt ; tth=ttt etc.
tth=ttt & uuh=uuu & kcomh=kcom & caseh=cased & sizh=sizt
ifh=ifile & vvh=vvv & lsh=lsv & ddh=ddd & ggh=ggg & verh=vern & tsh=tsur & end

266: help,ifh,ifile,lsh,lsv $ ; Help latest and hold
,tth,ttt,uuh,uuu,vvh,vvv,ddh,ddd,ggh,ggg,vern

29: q= READKRCCOM(-1,khold)     ; Close the KRC unit

; 44444444444444444444444444444444 test between cases
; 0: With atmosphere, properties constant with T
; 1: With atmosphere, properties T-dependent
; 2: With atmosphere, soil properties constant with T, frost properties variable
; 3: No atmosphere, properties constant with T
; 4: No atmosphere, properties T-dependent
; 5: No atmosphere, properties T-dependent, but constant

41: begin                       ; Test Ls
yy=reform(vvv[*,1,*]) ; Ls [season,case]
ya=MEAN_STD2(yy,std=yb) ; statistics across cases
xa=min(yb,max=xb) ; range of changes between cases
print,'Range of change of Ls between cases:',xa,xb
plot,yy[*,0],xtit='Season index [0-based]',ytit='Ls for case 0', title=ifile
end

411: begin & mjd=djmm           ; Check Ls against LSAM
if pari[1] then mjd=djmm-k24 ; adjust older version to j2000
lsam=LSAM(mjd,myn,aud)
plot,mjd,PM180(lsv-lsam),xtit='MJD  '+ifile,ytit='Ls from vvv-LSAM'
end

42: begin                       ; Confirm convergence days
ndj4=reform(ggg[0,*,*,*])
xa=min(ndj4,max=xb)
print,' NDJ4: min,max=',xa,xb
end

43: begin                       ; Plot hourly Ts near equator for 2 seasons
SETCOLOR,init=860 ; 10 colors on black
t1=reform(ttt[*,0,nlat/2,*,*]) ;[hour,season,case] Tsur at equator
q=min(abs(lsv-251.),i1) ; find index nearest perihelion, which is at Ls=251
q=min(abs(lsv-71.),i2)  ;    "    "     "      aphelion, which is at Ls=71
ii=[i1,i2] 
t1=t1[*,ii,*] ; only two seasons
t2=transpose(t1,[0,2,1]) ; [hour,case,season]
t3=reform(t2,nhour,2*ncase) ; [hour, case*season]
qq='Case '+str1[0:ncase-1]
q=[qq+(' Seas '+strtrim(ii[0],2)),qq+(' Seas '+strtrim(ii[1],2))]
CLOT,t3,q,titl=['Hour index','Ts at equator',ifile],locc=[.45,.5,-.03,.08]
Print,'Seasons and Ls=',ii, lsv[ii]
end

44: begin  ; Display central latitude seasonal behaviour
t1=reform(ttt[nhour/2,0,*,*,*]) ;[lat,season,case] Tsur at noon equator
t2=transpose(t1,[1,2,0])  ;[season,case,lat] 
t3=reform(t2,nseas*ncase,nlat)
CLOT,t3,slat,loc=[.38,.45,-.03,.08],titl=['season*case','Tsur at noon',ifile]
PLOTSECT,'Case '+str1[0:ncase-1],.21,cs=1.5
if ncase eq 6 then PLOTSECT,tcase,.16,cs=1.5
end

45: begin                        ; No atm, T:const - T:uniform
t1=ttt[*,*,*,*,5]-ttt[*,*,*,*,3] ; Prop=constant - Prop=uniform
j=n_elements(t1)
for i=0,4 do begin 
   xx=(t1[*,i,*,*])
   xa=total(abs(xx))/j
   print,itemt[i],' MAR=',xa
   if xa gt 1.E-6 then begin 
      HISTFAST,xx,xlab=itemt[i]
      PAUSE,-1
   endif
endfor
end

46: begin ; Tatm-TnoAtm
; For NoAtm cases, Tplan, Tatm, DownIR, FROST4 ,AFRO4 are meaningless
jj=[0,3]                        ; Tsur and DownVis
nsp=pari[1] & nhp=pari[2]       ; Number of seasons and hours to plot
t1=reform(ttt[*,jj,*,*,0]-ttt[*,jj,*,*,3]) ; Atm - NoAtm for  T:constant
n1=n_elements(t1)               ; [hour,item,lat,season] item =Tsur, DownVis
t2=transpose(t1,[0,3,2,1])      ; [hour,season,lat,item]
ii=(nseas/nsp)*indgen(nsp)      ; subset nsp seasons
print,'Seasons plotted:',ii
t2=t2[*,ii,*,*]
ii=(nhour/nhp)*indgen(nhp)      ; subset nhp hours
t2=t2[ii,*,*,*]
print,'Hours plotted:',ii
;t3=reform(t2,nhour*nseas*nlat,2) ; [hour*season*lat,item]
t3=reform(t2,nhp*nsp*nlat,2) ; [hour*season*lat,item]
CHART,t3,psy=3,parti=itemt[jj],dlin=2,xtit='hour * season * latitude' $
,titl='Atm. - NoAtm. '+ifile
PLOTSECT,slat,.51, cs=2.
      PAUSE,-1
for j=0,n_elements(jj)-1 do begin 
   i=jj[j]
   xx=t1[*,j,*,*]             ; Delta for an item
   xa=total(abs(xx))/n1
   print,itemt[i],' MAR=',xa
   if xa gt 1.E-6 then begin 
      HISTFAST,xx,xlab='Delta '+itemt[i]+' for atm-noAtm'
      PAUSE,-1
   endif
endfor
end

; 55555555555555555555555555555555 test between file types
; Read type 52, -1 and 0 for the same model

50:  begin & fun0=READKRC1(stem+'.t0',fcoz,icoz,lcoz, lsz $  ; Read type 0
,tsz,tpz,  ktype=0,/verb,ddd=dd0,lab=labd,desc=desc)
siz=size(fun0) & if siz[0] ne 2 then goto,halt
slat0=string(fun0[*,0],form='(f6.1)')
end

51: begin & funm=READKRC1(stem+'.tm1',fcom,icom,lcom, lsm $ ; Read type -1
,tsm,tpm,  ktype=-1,/verb)
siz=size(funm) & if siz[0] ne 2 then goto,halt
slatm=string(funm[*,0],form='(f6.1)')
end

511: begin & nread=n_elements(lsz); Compare Ls in Type 0 file with LSAM 
djul=fcoz[41-1] & deljul=fcoz[42-1] ; ASSUMED to be MJD. False before Version 2
jdisk=icoz[12-1]
mjd=djul+deljul*(findgen(nread)+(jdisk-1)) ; days from J2000
lsam=LSAM(mjd,myn,aud)
plot,mjd,PM180(lsz-lsam),xtit='MJD  '+ifile,ytit='Ls from Type0-LSAM'
end

52: begin & siz=size(dd0) ; Plot delta  of each ddd item
if siz[0] ne 3 then goto,halt
SETCOLOR,init=862 ; 17 colors
for j=0,siz[3]-1 do begin ; each item
   CLOT,transpose(dd0[*,*,j]),slat0,loc=[.35,.20,.025,.08] $
        ,titl=['season index.  ',desc[j], stem+'.t0 '+labd[j]]
   PAUSE,-1
endfor & end

522: begin & j=(pari[3]>0)<5 ; Plot one dd0 item
   CLOT,transpose(dd0[*,*,j]),slat0,loc=[.35,.20,.025,.08] $
        ,titl=['season index.  ',desc[j], stem+'.t0 '+labd[j]]
end

53: begin & help,lsv,lsm,lsz  ; Check Ls between types
plot,lsv,xtit='Season index  __=52 +=t0  diamond=t-1  & 200+100*diff' $
,ytit='Ls',title=stem
oplot,lsm,psym=4
oplot,lsz,psym=1
oplot,200.+100*(lsm-lsv),psym=4
oplot,200.+100*(lsz-lsv),psym=1
end

55: begin & fmt='(a10,4f12.5)'; Check Ts and Tp for equivalence between types
print,'   What           Mean      StdDev     Minimum     Maximum'
;     '   Ts 0--1     0.00000     0.00000     0.00000     0.00000
qt=tsz-tsm
aa=HSTATS(qt,['M','S','I','X']) & print,'Ts 0--1',aa,form=fmt
qt=tpz-tpm
aa=HSTATS(qt,['M','S','I','X']) & print,'Tp 0--1',aa,form=fmt
qt=ttt[*,0,*,*,0]-tsm ; type 52 for atm,Tconst
aa=HSTATS(qt,['M','S','I','X']) & print,'Ts 52--1',aa,form=fmt
qt=ttt[*,1,*,*,0]-tpm
aa=HSTATS(qt,['M','S','I','X']) & print,'Tp 52--1',aa,form=fmt
ii=[1,3,4] ; items in ggg
jj=[0,4,5] ; corresponding items in dd0
for i=0,2 do begin 
   qt=dd0[*,*,jj[i]]- ggg[ii[i],*,*,0]
   aa=HSTATS(qt,['M','S','I','X']) & print,itemg[ii[i]],aa ,form=fmt
endfor
end

56: begin & siz=size(tsm) & if siz[0] ne 3 then goto,halt ; Store Type 0,-1
siz=size(tsz) & if siz[0] ne 3 then goto,halt
lsmh=lsm & tsmh=tsm & tpmh=tpm ;
lszh=lsz & tszh=tsz & tpzh=tpz & dd0h=dd0 & end

57: begin & fmt='(a10,4f12.5)' ; Compare Versions for Type 0 and -1
print,'   What           Mean      StdDev     Minimum     Maximum'
qt=lsz-lszh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ls 0',aa,form=fmt
qt=tsz-tszh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ts 0',aa,form=fmt
qt=tpz-tpzh
aa=HSTATS(qt,['M','S','I','X']) & print,'Tp 0',aa,form=fmt
qt=dd0-dd0h
aa=HSTATS(qt,['M','S','I','X']) & print,'ddd 0',aa,form=fmt
qt=lsm-lsmh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ls -1',aa,form=fmt
qt=tsm-tsmh
aa=HSTATS(qt,['M','S','I','X']) & print,'Ts -1',aa,form=fmt
qt=tpm-tpmh
aa=HSTATS(qt,['M','S','I','X']) & print,'Tp -1',aa,form=fmt
end 

; 66666666666666666666666666666666 tests between versions
61: plot,djmm,PM180(lsv-lsh),xtit='MJD  '+ifile,ytit='lsv-lsh' $ ; Plot LS-LSH
,titl=ifile+' - '+ifh

62: begin & i=nhour/2 & j=nlat/2 ; Plot Tsur noon equator
xx=tth[i,0,j,*,0] ; noon equator case zero 
yy=ttt[i,0,j,*,0] ; noon equator case zero  Ver B
ya=min([xx,yy],max=yb) ; total range in T
plot,xx,yran=[ya,yb],xtit='Season index  Dashed in VerB' $
,ytit='Tsur near noon equator'
oplot,yy,line=2,color=99
oplot,280.+100*(yy-xx),psym=3
end

63: begin  & fmt='(a10,4f12.5)'; Stats on VerB-VerA
if total(abs(sizt-sizh)) ne 0 then goto,halt ; some dimension different
print,'xxx-xxh           Mean      StdDev     Minimum     Maximum'
qt=ttt-tth 
for j=0,niti-1 do begin & aa=HSTATS(qt[*,j,*,*,*],['M','S','I','X']) 
print,itemt[j],aa,form=fmt & endfor
qt=ddd-ddh 
for j=0,1 do begin & aa=HSTATS(qt[*,j,*,*,*],['M','S','I','X']) 
print,itemd[j],aa,form=fmt & endfor
qt=ggg-ggh
for j=0,5 do begin & aa=HSTATS(qt[j,*,*,*],['M','S','I','X']) 
print,itemg[j],aa,form=fmt & endfor
qt=vvv-vvh
for j=0,4 do begin & aa=HSTATS(qt[*,j,*],['M','S','I','X']) 
print,itemv[j],aa,form=fmt & endfor
qt=uuu-uuh
for j=0,1 do begin & aa=HSTATS(qt[*,j,*],['M','S','I','X']) 
print,itemu[j],aa,form=fmt & endfor

ndj4=fix(reform(ggg[0,*,*,*])) ; [lat,season,case] number of days to convergence
ndj4h=fix(reform(ggh[0,*,*,*]))
del4=ndj4-ndj4h                 ; change in number of iteration days
ii=where(del4 eq 0.,ni)         ; days the same within [lat,season,case]
i=nlat*nseas*ncase-ni ; number not the same
print,'Num lat*seas*case with NDJ4 same/diff=',ni,i
if i le 0 then print, 'All NDJ4 the same' else begin
   tr=reform(ttt,nhour,niti,nlat*nseas*ncase) ; [hour,item,lat*seas*case]
   tw=reform(tth,nhour,niti,nlat*nseas*ncase)
   qq=tr[*,*,ii]-tw[*,*,ii]     ; [hour,item, only same convg days)
   for k=0,niti-1 do begin 
      qt=qq[*,k,*]              ; one item
      aa=HSTATS(qt,['M','S','I','X']) & print,id52[k],aa,form=fmt
   endfor
endelse
end

; 77777777777777777777777777777777 Test one-point mode
71: begin & kfile=parf[0]+parf[4]+'.prt' ; Test one-point mode
mrow=pari[4]
sss=READTXTCOL(kfile,nskip=-1,ncol=12,mrow=mrow)
siz=size(sss) & if siz[0] ne 2 then goto,halt
if siz[1] lt mrow then begin
   print,'Fewer OnePoint rows than expected:',siz[1],mrow
   print,'Adjust @14 item 4'
   goto,halt
endif else print,'OnePoint VerA rows read=',siz[1]
ppa=float(sss[*,1:11]) 
kfile=parf[5]+parf[8]+'.prt'
sss=READTXTCOL(kfile,nskip=-1,ncol=12,mrow=mrow)
siz=size(sss) & if siz[0] ne 2 then goto,halt
if siz[1] ne mrow then goto,halt
print,'OnePoint VerB rows read=',siz[1]
ppb=float(sss[*,1:11]) 
qq=ppb[*,0:8]-ppa[*,0:8] ; difference
xa=min(qq,max=xb)
print,'Range of OnePoint input differences',xa,xb
if xb-xa gt 1.e-3 then goto,halt
t1=ppb[*,9:10]-ppa[*,9:10]
ya=min(qq,max=yb)
print,'Range of OnePoint output T differen',ya,yb
end

else: begin & KON91,ptitl,prior,hold,kon,kons,kitel ;=KON99 < needed by MAKE99
      if kon eq 99 then begin 
          print,'11: files: parf= ',parf
          print,'14: Integs: pari= ',ST0(pari)
      endif & end
endcase
goto, ask

halt:  print,'SOME ERROR CONDITION at kon=',kon,'.  Any key to Go'
i=get_kbrd(1) & goto,sureask
end
