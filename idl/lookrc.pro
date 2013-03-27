
function lookrc1, ggg, alat,vvv
;_Titl  LOOKRC1  Compare SUMF with integration of FROST4
; ggg   in.  from type 52
; alat  in.  fltarr(n)
; vvv   in.  from type 52
; func. out. fltarr(nseas,ncase) Global average Frost 
;_Hist 2010jan13 Hugh Kieffer
;_End
siz=size(ggg)
ns=siz[3]                       ; # seasons
nc=siz[4]                       ; # cases
out=fltarr(ns,nc)               ; to hold global integration
for k=0,nc-1 do begin           ; each case
    for j=0,ns-1 do begin       ; each season
        f4=reform(ggg[3,*,j,k]) ; get FROST4
        out[j,k]=TINT(alat,f4,www) ; global average
    endfor
endfor
sumf=reform(vvv[*,4,*])         ; SUMF
ya=min([sumf,out],max=yb)       ; range of both
plot,sumf,yran=[ya,yb],psym=2,xtit='season*case',ytit='line=TINT(ggg)  sym=vvv'
oplot,out
if !dbug then stop
return,out
end

;_Titl  LOOKRC  Read any type 5x KRC bin5 models; look at change between cases
;_Calls  DEFINEKRC  DELCASE  KRCCOMLAB   
;        READKRC52  READKRC54  READKRC56  READKRCCOM  
; Utility: CHART  GETPAN  GETPSN  KON91  LABEL_CURVE  SETCOLOR  ST0  STRWORD1
;_Desc. Initial purpose to look at differences between KRC case with/without 
;    temperature-dependent conductivity. Generalized for any type 5x file.
; Each file read followed by statements that define the order of the dimension
;  for each array in the file. Identification of the contents of each array are
;  contained in the returned arguments
; @ 40 prints a guide to arrays in the file
; @ 4 selects one array
; @ 41 ,42 will select one case or the difference between two cases,
;    regardless of dimensionality of the array; produces array yy
; @ 43 Converts arrays to y2 with dimension order: hour, season, latitude, item
;    and creates a 2-dimension ( time/lat., item) version y3
; @ 44 and 45 are intended for differences only; they attempt to locate outliers
;_Hist 2009mar07 Hugh Kieffer many revisions
;_End
ptitl='lookrc' & version ='2010feb17' 

;;common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;;SETCOLOR,/init                  ; required to define items in common.
;;SETCOLOR,init=856  & nkc=kcc[2] & nkl=kcc[3] ; initialize lines and colors

labf=['Input KRC DIR',' " file','2nd file']
parf=['/work1/krc/mars/','masterc','koft']

labi=['!dbug 1=set for all lower routines' $
,'kode for krccom print +1=flt +2=int +4=logi'$
,'Base case','Delta case','Max Number outliers to print' $
,'Index for Hour',' " Season',' " Latitude',' " Item',' " array']
pari=[0,7, 0,1,25,12,10,9,0,0]

labr=['Pause, seconds, -=wait','Toler. on outlier fraction','N sigma ' $
,'Pause @71 Not==0','DELCASE pause control']
parr=[1.,0.001, 3., -2 ,-2 ] 

paw=parr[0]                     ; defaults
mxn2=384*4                      ; default for MAXN2
mlab=['lay','hour','item','lat','seas','case'] ; dimensions in T5x file
;       0     1      2       3     4      5
dim5=mlab[1:5]                  ; dimensions of ttt for 51 and 52
msiz=intarr(6)                  ; to hold dimensions in fixed order
clrr=!binc[[13,61,111,151,190,216,235,247,255,2,20,130,40,1]]
ncl=n_elements(clrr)
prior=['1','1']                 ;impossible values, to initiate MAKE99
;^^^^^^^^^^^^^^^^^^^^^^ inputs
kite=' '   & k5=0  & narr=0 
yng=['NoneYet']  & dima=yng & ita=yng & fhold=lonarr(4); type definitions
do6=0B
sizy=[0] & siz2=[0] & siz3=[0]  & ka=0            ; insurance
kons=[252,61,6,40,4,42,43,44]           ; default auto-sequence
lkon=0B                         ; set auto-seq off
kons=[252,4,42,43,44] ; <<<< temporary
kon=1 ;  & goto,dokon             
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
-2: return                      ;<
 0: stop                        ;-

111: kons=[4,41,43,44]            ; Set to display one array
171: begin & kons=[252,61,401,402,71] ; Set to get frost amounts
parf[1]='mcap1'
pari[8]=3 ; frost at each lat
pari[9]=4 ; which array
end

123: begin & lkon=1b & kkon=-1  ;- start auto-script 
lastkon=n_elements(kons)-1 & end ; last preset command 

;;; 1x == Modify values; Guides; commitment ....................................
11: GETPSN,'File names: parf',parf,lab=labf ;- modify any of a set of strings
15: GETPAN,'Integer values: pari',pari,-1,999,labs=labi ;-

16: begin & GETPAN,'Float values: parr',parr,0.,0.,labs=labr ;-  " " , no limits
paw=parr[0] & end

18: begin  & fmta='( 12(a,1x))'  ; Guide to returned arrays
    help,ttt & print,'  Indices=',dimt,'   Items=',itemt, form=fmta
if narr ge 3 then begin
    help,uuu & print,'  Indices=',dimu,'   Items=',itemu, form=fmta
    help,vvv & print,'  Indices=',dimv,'   Items=',itemv, form=fmta
endif
if narr ge 5 then begin
    help,ddd & print,'  Indices=',dimd,'   Items=',itemd, form=fmta
    help,ggg & print,'  Indices=',dimg,'   Items=',itemg, form=fmta
endif 
help,deltit,yy,y2,y3,y4
end

188: begin ; Guide to extracted array
print,'Selected and differenced arrays'
help,fin,narr,ka,aaa,yy,y2,y3
print,'dima=',dima
print,'Items=',ita
print,'y2 dim=',yng 
; help,nhour,nitem,nlat,nseas,ncase
end

251: begin & fin=parf[0]+parf[1]+'.t51' ; Read Type: 51
lss=READKRC52(fin,ttt,uuu,vvv,itemt,itemu,itemv, /t51)
ii=size(lss) & if ii[0] ne 1 then goto,halt
k5=51 &  help,lss,ttt,uuu,vvv,itemt,itemu,itemv
narr=3; number of returned arrays
dimt=mlab[1:5]                  ; dimensions of ttt 'hour','item','lat','seas','case
dimu=['lat' ,'item','case']     ; " " uuu
dimv=['seas','item','case']     ; " " vvv
alat=uuu[*,0,0] & elev=uuu[*,1,0] ; latitudes and elevations 
msiz=intarr(6); file array sizes for layer,hour,item,lat,season,case
jj=size(ttt) & msiz[1:5]=jj[1:5]
fhold[0]=0 &     end                      

252: begin & fin=parf[0]+parf[1]+'.t52';+ 52
lss=READKRC52(fin,ttt,uuu,vvv,itemt,itemu,itemv, ddd,ggg,itemd,itemg)
ii=size(lss) & if ii[0] ne 1 then goto,halt
k5=52 &  help,lss,ttt,uuu,vvv,itemt,itemu,itemv, ddd,ggg,itemd,itemg
narr=5; number of returned arrays
dimt=mlab[1:5]                 ; dimensions of ttt 'hour','item','lat','seas','case
dimu=['lat' ,'item','case'] ; 
dimv=['seas','item','case'] ;  
dimd=['lay' ,'item','lat','seas','case'] 
dimg=['item','lat' ,'seas','case']
alat=uuu[*,0,0] & elev=uuu[*,1,0] ; latitudes and elevations
;carr=['t','u','v','d','g'] ; character in array name
jj=size(ttt) & msiz[1:5]=jj[1:5]
jj=size(ddd) & msiz[0]= jj[1]
fhold[0]=0 & end

254: begin & fin=parf[0]+parf[1]+'.t54' ;+ 54
lss=READKRC54(fin,ttt,itemt)
ii=size(lss) & if ii[0] ne 1 then goto,halt
k5=54 & help,lss,ttt,itemt ;t54 is [season, item,latitude,case]
narr=1; number of returned arrays
dimt=['seas','item','lat','case'] ; dimensions of t 
Print,'only one array, can skip @ 40,41'
ana='ttt' & aaa=ttt & ita=itemt & dima=dimt
jj=size(ttt) & msiz[[4,2,3,5]]=jj[1:4]
fhold[0]=0 &    end  ; Guides and help

255: begin & fin=parf[0]+parf[1]+'.t55';+ 55
lss=READKRC54(fin,ttt,itemt,/t55)
ii=size(lss) & if ii[0] ne 1 then goto,halt
k5=55 & help,lss,ttt,itemt ;t55 is [season, item,case]
narr=1; number of returned arrays
dimt=['seas','item','case'] ; dimensions of t
Print,'single 2D array: Equiv of @40,41,42,43 done here,'
ana='ttt' & aaa=ttt & ita=itemt & dima=dimt
jj=size(ttt) & msiz[[4,2,5]]=jj[1:3]
yy=DELCASE(ttt,pa=parr[4],hh=hh)    ;=41
sizy=size(yy)
if sizy[0] lt 2 then goto,halt
y2=yy & siz2=sizy & help,y2     ;=42 already in order
y3=yy & siz3=sizy               ;=43 only 2D, no need to reform
x3tit='Season'
fhold[0]=0 & end

256: begin & fin=parf[0]+parf[1]+'.t56';+ 56
;l56=READKRC56(fin, ts,tp,tlay,tlx,tsx,tltit,tstit)
lss=READKRC56(fin, ttt,uuu,vvv,ddd,ggg,itemd,itemg)
k5=56 &   help,lss,ttt,uuu,vvv,ddd,ggg,itemd,itemg
narr=5; number of returned arrays
dimt=['hour','lat','seas','case'] & itemt='Tsurf' ; dimensions of Ts
dimu=['hour','lat','seas','case'] & itemu='Tplan' ; dime of Tplan
dimv=['lay', 'lat','seas','case'] & itemv='Tmidn' ; dime of T_midnight_layer
dimd=['item','lat','seas','case']
dimg=['seas','item','case']
jj=size(ttt) & msiz[[1,3,4,5]]=jj[1:4]
jj=size(vvv) & msiz[0]=jj[1]    ; Beware, not all arrays
jj=size(ddd) & msiz[2]=jj[1] ; " " 
fhold[0]=0 & end


522: begin & ttth=ttt & uuuh=uuu & vvvh=vvv ; 52 arrays into hold
dddh=ddd & gggh=ggg & htit=parf[1] & k5h=k5 & hsiz=msiz & end

523: begin & if k5 ne k5h then goto,halt ; HSTATS on now-hold
print,'ArrayDelta  Mean       StdDev          MIN          MAX'     
;      ttt   -0.0750556      2.38353     -119.252      6.12738
print,'ttt',HSTATS(ttt-ttth,['M','S','I','X'])
print,'uuu',HSTATS(uuu-uuuh,['M','S','I','X'])
print,'vvv',HSTATS(vvv-vvvh,['M','S','I','X'])
print,'ddd',HSTATS(ddd-dddh,['M','S','I','X'])
print,'ggg',HSTATS(ggg-gggh,['M','S','I','X'])
end

524: sf =LOOKRC1(ggg,uuu[*,0,0],vvv) ; Recompute SUMF
525: sfh=LOOKRC1(gggh,uuuh[*,0,0],vvvh) ;+ for hold arrays

400: begin ; stats on changes between two cases
qq=DELCASE(ttt,jk=pari[2:3],pa=-2.,id='ttt',dimn=dimt,ita=itemt) ; | does
qq=DELCASE(uuu,jk=pari[2:3],pa=-2.,id='uuu',dimn=dimu,ita=itemu) ; | nothing 
qq=DELCASE(vvv,jk=pari[2:3],pa=-2.,id='vvv',dimn=dimv,ita=itemv) ; | if array
qq=DELCASE(ddd,jk=pari[2:3],pa=-2.,id='ddd',dimn=dimd,ita=itemd) ; | not 
qq=DELCASE(ggg,jk=pari[2:3],pa=-2.,id='ggg',dimn=dimg,ita=itemg) ; | proper
end


40: begin ; Guide to current file  REQ 5x
for k=0,narr-1 do begin     ; each array
    case k of
    0: begin & ana='ttt' & sizk=size(ttt) & ita=itemt & dima=dimt & end
    1: begin & ana='uuu' & sizk=size(uuu) & ita=itemu & dima=dimu & end
    2: begin & ana='vvv' & sizk=size(vvv) & ita=itemv & dima=dimv & end
    3: begin & ana='ddd' & sizk=size(ddd) & ita=itemd & dima=dimd & end
    4: begin & ana='ggg' & sizk=size(ggg) & ita=itemg & dima=dimg & end 
    endcase
    print,k,' =k   Array  '+ana+'  has:',ita,format='(i2,a,10a10)'
    Print,'Dimensions ',sizk[1:sizk[0]],format='(2x,a12,6i8)'
    print,'Contain: ',dima ,format='(2x,a12,6a8)'
    print,'' ; spacing line
endfor
end

4: begin & ka=0 & if narr lt 2 then goto,halt ; Select which array   REQ 5x
again4: read,ka,prompt='Desired array index ? > '
if ka lt 0 or ka ge narr then begin 
    print,'invalid : allowed are:',0,narr-1
    goto, again4
endif 
kon=402 & goto,dokon & end

401: ka=pari[9] ;- set array from pari[9]

402: begin & if ka lt 0 or ka ge narr then goto,halt ; aaa= one array  REQ 4 
; ana: title of array  aaa: the array  ita: names of items 
; dima: names of dimensions of aaa
; ynw: names of dimensions desired after transpose into y2
; nky: number of leading y2 dimensions to combine into y3 
case ka of
    0: begin & ana='ttt' & aaa=ttt & ita=itemt & dima=dimt 
    ynw=['hour','seas','lat','item'] & nky=2 & end
    1: begin & ana='uuu' & aaa=uuu & ita=itemu & dima=dimu 
    ynw=['lat','item'] & nky=0 & end  ; no need for transpose
    2: begin & ana='vvv' & aaa=vvv & ita=itemv & dima=dimv 
    ynw=['seas','item'] & nky=0 & end    ; no need for transpose
    3: begin & ana='ddd' & aaa=ddd & ita=itemd & dima=dimd 
    ynw=['seas','lat','lay','item'] & nky=2 & end
    4: begin & ana='ggg' & aaa=ggg & ita=itemg & dima=dimg 
    ynw=['seas','lat','item']  & nky=2 & end 
    endcase 
siza=size(aaa)
print,ka,' =k.  Array  '+ana+'  has:',ita,format='(i2,a,10a8)'
Print,'Dimensions ',siza[1:siza[0]],format='(2x,a12,6i8)'
print,'Contain: ',dima ,format='(2x,a12,6a8)'
jj=intarr(6)                    ; to hold sizes, intitially set all to zero
for j=0,5 do begin              ; each normal dimension
    i=where(dima eq mlab[j]) & i=i[0] ; did it occur?
    if i[0] ge 0 then i=siza[i+1] ; yes, gets its size
    jj[j]=i                     ; and save it
endfor
nlay=jj[0] & nhour=jj[1] & nitem=jj[2] ; | sizes in 
nlat=jj[3] & nseas=jj[4] & ncase=jj[5] ; | current array
end

41: begin & yy=DELCASE(aaa,jk=pari[2],hh=hh) ; yy = one case in aaa  REQ 402
sizy=size(yy)
if sizy[0] lt 2 then goto,halt 
deltit=ana+' Case '+strtrim(pari[2],2)
siz2=[0] & siz3=[0] & end

42: begin ; yy= Diff. two cases in aaa
yy=DELCASE(aaa,jk=pari[2:3],hh=hh,pa=parr[4])
sizy=size(yy)
if sizy[0] lt 2 then goto,halt 
deltit=string(ana,' Delta case ',pari[3],' - ',pari[2],form='(a,a,i2,a,i2)')
siz2=[0] & siz3=[0] & end       ; no y2 or y3 yet

43: begin ;  y2= Transpose yy  y3= combined y2
; y2: Transpose yy to order of ynw, typically [hour],season,lat,item  
; y3: y2 with dimensions combined for CHART of each item separately
; y4: y2 with dimensions combined so all will CHART. e.g., [hour*]season*lat,item
; nky: number of leading y2 dimensions to combine into y3
; ynw: names of dimensions of yy that preceed case
if sizy[0] lt 2 then goto,nope
if sizy[0] eq 2 or nky lt 1 then begin 
    print,'No need to reduce dimensions'
    goto, ask
    endif
nyg=n_elements(ynw)             ; number of dimensions to process
yng=strarr(nyg)                 ; to hold y order get
kyt=intarr(nyg)                 ; to become transpose control vector
k=-1                            ; index of transpose dimension 
for i1=0,nyg-1 do begin         ; each desired dimension
    i=where(dima eq ynw[i1]) & i=i[0] ; where is it now ?
    if i ge 0 then begin        ; found it
        k=k+1                   ; increment output location
        kyt[k]=i                ; source dimension 0-based index
        yng[k]=ynw[i1]          ; save name for titles
    endif else message,ynw[i1]+' Dimension not found'
endfor
y2=transpose(yy,kyt)            ; transpose
siz2=size(y2) & help,y2
j=siz2[1] & x3tit=yng[0]        ; values for first dimension of y2
if nky gt 1 then for i=1,nky-1 do begin
    j=j*siz2[i+1]               ; size of first dimension of y3
    x3tit=x3tit+' * '+yng[i]    ; X title for chart of y3
endfor
print,'y2 Dimensions are:',yng
if nky eq nyg-1 then begin 
    y3=reform(y2, j,siz2[nyg])
    siz4=0
endif else if nky eq nyg-2 then begin
    y3=reform(y2,j,siz2[nyg-1],siz2[nyg])
    y4=reform(y3,j*siz2[nyg-1],siz2[nyg])
    siz4=size(y4)
    x4tit=x3tit+' * '+yng[nyg-2]
    x3tit=x3tit+' Y='+yng[nyg-2]
    print,'y4 Dimensions are: ',x4tit+' ,'+yng[nyg-1]
endif else print,'No reform done: nky,nyg=',nky,nyg
print,'y3 Dimensions are: ',x3tit+' ,'+yng[nyg-1] & siz3=size(y3)    
end

44: if  sizy[0] eq 2 then CHART,yy,parti=ita,xtit=ynw[0],dlin=1 $ ; single CHART
else if siz2[0] eq 2 then CHART,y2,parti=ita,xtit=x3tit,dlin=1 $
else begin 
    if siz3[0] lt 2 then goto,halt ; CHART y3 items
    if      siz3[0] eq 2 then CHART,y3,parti=ita,xtit=x3tit,dlin=1  $
    else if siz4[0] eq 2 then CHART,y4,parti=ita,xtit=x4tit,dlin=1 
endelse

443: begin & if siz3[0] ne 3 then goto,nope   ; specific for delta ddd
a0=1000.*MEAN_STD2(y3[*,*,0],std=s0,/one) & s0=1000.*s0
a1=1000.*MEAN_STD2(y3[*,*,1],std=s1,/one) & s1=1000.*s1
print,'Lay MeanMin StdMin MeanMax StdMax   in milliK'
for i=0,siz3[2]-1 do print,i+2,a0[i],s0[i],a1[i],s1[i], form='(i3,4f8.1)'
end


444: begin & if siz3[0] ne 3 then goto,nope  ; Y3, each item 
j=siz3[3]-1               
if j lt 0 then goto,nope
for k=0,j do begin
    CHART,y3[*,*,k],xtit=x3tit,titl=ita[k]
    if k lt j then  PAUSE,parr[0]
endfor & end

441: begin if siz3[0] ne 3 then goto,nope;+ one item
print,ita
k=0 & read,k,prompt='Which item > '
k=(k>0)<(siz3[3]-1)
if yng[2] eq 'lat' then qq=ST0(alat,/nojoin) else qq=0
CHART,y3[*,*,k],parti=qq, xtit=xtit3,titl=ita[k] & end

48: begin & if siz3[0] ne 2 then goto,nope ; Locate outliers in y3
nk=siz3[2]                      ; number of items
ntime=siz3[1]                   ; number of times = hour*seasons*lats
sit=siz2 & sit[0]=sit[0]-1      ; reduce y2 dimensionality to one item
for k=0,nk-1 do begin           ; each item
    qq=y3[*,k]                  ; extract item
    HISTFAST,qq,i1=i1,r1=r1
    fout=(i1[1]+i1[2])/float(ntime) ; fraction outside 4 sigma plot
    print,ita[k],' fout=',fout               ; print item name
    print,'mean,sig,min,max=',r1[3:*]
    if fout gt parr[1] then begin ; excessive outliers
        ii=where(abs(qq-r1[3]) gt parr[2]*r1[4],j) ; outside N sigma
        if j gt 0 then ijk=LOC2INDEX(ii,siz=sit) else ijk=-1
        if j lt 8 then print,ijk else begin 
            PAUSE,paw
            CHART,ijk,parti=yng[0:sit[0]-1]+' Index'
        endelse
    endif
    PAUSE,paw
endfor
end

49: begin & if siz3[0] lt 2 then goto,nope ; Print outliers for one item
print,ita                       ; available items
k=0 & read,k,prompt=' Which item? Max='+strtrim(siz3[2])+' > ' 
qq=y3[*,k]                      ; one item
HISTFAST,qq,i1=i1,r1=r1; ,noplot=2 
ii=where(abs(qq-r1[3]) gt parr[2]*r1[4],j)
if j gt 0 then begin
    sit=siz2 & sit[0]=sit[0]-1  ;  reduce to dimensionality of one item
    ijk=LOC2INDEX(ii,siz=sit)
    print,'  i       value  Hour  Seas   Lat <-- indices'
    for i=0,(j-1)<pari[4] do print,i,qq[ii[i]],ijk[i,*],form='(i3,g12.4,5i6)'
endif & end

61: begin                       ; Read krccom print changes [get lats]
if fhold[0] le 0 then front=READKRCCOM(fin,fhold)
if front[0] lt 0 then goto,halt ; read failure
print,'Front=',front
kcom=READKRCCOM(1,fhold)        ; read common for first case
j=size(kcom,/type)              ; expect structure
if j ne 8 then goto,halt        ; something failed
if k5 gt 53 then begin ; get lat and elevation from krccom
    j=kcom.id[3]-1              ; last latitude
    alat=kcom.alat[0:j]         ; latitudes
    elev=kcom.elev[0:j]         ; elevations
endif
print,'Changes between cases:'
sss=KRCHANGE(fhold,/log)        ; find changes and print them
sss[0]='Basic'                  ; avoid long title for base case
i=READKRCCOM(-1,fhold)          ; close file, release the unit, set lun to 0
if do6 then begin & kon=6 & goto,dokon & endif
end

6: begin & do6=0B & jj=size(kcom); Print krccom 
if jj[2] ne 8 then begin
    print,'Must read krccom. Will do this now and continue here'
    kon=61 & & do6=1B
goto,dokon & endif
; get labels for common items
kstru=DEFINEKRC('KRC', labkf,labki,labkl,idmin,idmax,mxn2=mxn2,nwin=fhold[3]);,nword=nword)
idkf=STRWORD1(labkf,len=10)     ; extract ID and right-adjust
idki=STRWORD1(labki,len=10)     ; "
idkl=STRWORD1(labkl,len=7)      ; "
kode=pari[1]                    ; types DESIRED 
KRCCOMLAB ,kode,kcom.fd,kcom.id,kcom.ld, idkf,idki,idkl ; print commons
j=n_elements(sss)
if j gt 1 then for i=1,j-1 do print,i,sss[i],form='(i2,1x,a)' ; print them
end


641: r41=KRC41() ; read fort.25

643: KRC43,[20,2,18366] ,yid3,ii3,rr3 ; Read fort.43  30 sec
; 587712  5950584 52012512 fort.43 for every time

644: rr4=KRC44([3,2,-110],ii4,yid4) ; Read fort.44

66: begin & if siz2[0] lt 3 then goto,nope ; Plot one day of y2
; assume have T [hour,season,lat,...]
j=(pari[6]>0)<siz2[2] ; season index
k=siz2[3] ; <(pari[7]>0)           ; latitude "
if siz2[0] eq 3 then m=0 else m=siz2[4]
m=(pari[8]>0)<m ; item
qq=reform(y2[*,j,*,m],/over)
ya=min(qq,max=yb)
xx=indgen(siz2[1])
plot,qq,xran=[-.5,24.5],yran=[ya,yb],ytit=ita[m],/nodata $
,xtit='Hour.   Labeled with latitude',title=fin
for i=0,k-1 do begin 
    q=string(uuu[i,0],form='(f5.1)')
    oplot,qq[*,i]
    LABEL_CURVE,q,xx,qq[*,i]
endfor & end

71: TOTALCO2, aaa,vvv,sss,alat,kcom.fd[46],paw=parr[3] ; Frost amounts. Req 52,

72: begin  & locc=[.3, .94, -.03, .07]; Plot frost
qq=reform(ttt[*,7,*],/over) 
OPLOT2,qq,sss,['Ls','Frost kg/m^2','file='+fin],xin=lss,locc=locc,ccc=clrr[0:11]
end

73: begin ; global average pari 2,8
i2=pari[8]                       ; which item to average
i5=pari[2]                      ;  which case
xb=float(msiz[1])               ; nhour          
nk=msiz[4]                      ; nseas
nj=msiz[3]                      ; nlat
gtav=fltarr(nk)
qq=fltarr(nj)
topt=itemt[i2]+' Case='+strtrim(i5,2)
www=0.                          ; ensure new global weights
t4=total(ttt,1)/xb              ; diurnal average [item,lat,season,case]
t2=reform(t4[i2,*,*,i5])        ; [lat,season]
for k=0,nk-1 do gtav[k]=TINT(alat,t2[*,k],www)
ya=mean_STD(gtav,std=yb) & print,'mean,StDev=',ya,yb
t4=transpose(t2)                ; [season,lat]
plot,lss,gtav,xtit='L_s',ytit='Global average ',title=topt
end
74: CHART,t4,xin=lss,xtit='season',title=topt ; +Chart

else: begin & KON91,ptitl,prior,hold9,kon,kons,kitel ;=KON99 < needed by MAKE99
;  begin & ire9=KON99(ptitl,prior,hold9,m9,v9,kon,kons,kitel, dd,bbb,log,avv)
;      if ire9 eq 1 then goto,sureask & if ire9 eq 2 then goto,halt
      if kon eq 99 then begin 
          print,'11: files: parf= ',parf
          print,'15: Subset: pari= ',ST0(pari)
          print,'16: Floats: parr= ',ST0(parr)
      endif & end
endcase
goto, ask

nope: print,'Dimensions wrong for this'
goto, ask

halt:  print,'SOME ERROR CONDITION at kon=',kon,'.  Any key to Go'
i=get_kbrd(1) & goto,sureask

end
