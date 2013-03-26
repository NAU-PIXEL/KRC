;_Titl  LOOKRC  Read any type 5x KRC bin5 models; look at change between cases
;_Calls  DEFINEKRC  DELCASE  KRCCOMLAB   
;        READKRC52  READKRC54  READKRC56  READKRCCOM  
; Utility: CHART  GETPAN  GETPSN  KON91  LABEL_CURVE  SETCOLOR  ST0  STRWORD1
;_Desc. Initial purpose to look at differences between KRC case with/without 
;    temperature-dependent conductivity. Then generalized.
; Each file read followed by statements that define the order of the dimension
;  for each array in the file. Identification of the contents of each array are
;  contained in the returned arguments
; @ 40 prints a guide to arrays in the file
; @ 4 selects one array
; @ 41 ,42 will select one case or the difference between two cases,
;    regardless of dimensionality of the array; produces array yy
; @ 4 Converts arrays to y2 with dimension order: hour, season, latitude, item
;    and creates a 2-dimension ( time/lat., item) version y3
; @ 44 and 45 are intended for differences only; they attempt to locate outliers
;_Hist 2009mar07 Hugh Kieffer
;_End
ptitl='lookrc' & version ='2009mar09'

;;common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;;SETCOLOR,/init                  ; required to define items in common.
;;SETCOLOR,init=856  & nkc=kcc[2] & nkl=kcc[3] ; initialize lines and colors

labf=['Input DIR',' " file']
parf=['/work1/krc/mars/','master']

labi=['!dbug 1=set for all lower routines' $
,'kode for krccom print +1=flt +2=int +4=logi'$
,'Base case','Delta case','Max Number outliers to print' $
,'Index for Hour',' " Season',' " Latitude',' " Item',' " array']
pari=[0,7, 0,1,25,12,10,9,0,0]

labr=['Pause, seconds, -=wait','Toler. on outlier fraction','N sigma ' $
,'Pause @71 Not==0']
parr=[1.,0.001, 3., -2  ] 

jk=pari[2:3] & paw=parr[0]      ; defaults
mxn2=384*4                      ; default for MAXN2

dim5=['hour','item','lat','seas','case'] ; dimensions of ttt for 51 and 52
clrr=!binc[[13,61,111,151,190,216,235,247,255,2,20,130,40,1]]
ncl=n_elements(clrr)
prior=['1','1']                 ;impossible values, to initiate MAKE99
;^^^^^^^^^^^^^^^^^^^^^^ inputs
kite=' '   & k5=0  & narr=0 
yng=['NoneYet']  & dima=yng & ita=yng ; type definitions
do6=0B
sizy=[0] & siz2=[0] & siz3=[0]             ; insurance
kons=[52,61,6,40,4,42,43,44]           ; default auto-sequence
lkon=0B                         ; set auto-seq off
& kons=[55,61,72] ; <<<< temporary
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

111: kons=[4,41,43,44]            ; Display one array
171: begin & kons=[52,61,401,402,71] ; frost amounts
parf[1]='mcap1'
pari[8]=3 ; frost at each lat
pari[9]=4 ; which array
end

123: begin & lkon=1b & kkon=-1  ;- start auto-script 
lastkon=n_elements(kons)-1 & end ; last preset command 

;;; 1x == Modify values; Guides; commitment ....................................
11: GETPSN,'File names: parf',parf,lab=labf ;- modify any of a set of strings
15: begin & GETPAN,'Integer values: pari',pari,-1,999,labs=labi ;-
jk=pari[2:3] & end

16: begin & GETPAN,'Float values: parr',parr,0.,0.,labs=labr ;-  " " , no limits
paw=parr[0] & end

18: begin ; Guide to returned arrays
help,ttt & print,'  ',itemt,' indices are:',dimt
if narr ge 3 then begin
help,uuu & print,'  ',itemu,' indices are:',dimu
help,vvv & print,'  ',itemv,' indices are:',dimv
endif
if narr ge 5 then begin
help,ddd & print,'  ',itemd,' indices are:',dimd
help,ggg & print,'  ',itemg,' indices are:',dimg
endif & end


188: begin ; Guide to extracted array
print,'Selected and differenced arrays'
help,fin,narr,ka,aaa,yy,y2,y3
print,'dima=',dima
print,'Items=',ita
print,'y2 dim=',yng 
help,nhour,nitem,nlat,nseas,ncase
end

51: begin & fin=parf[0]+parf[1]+'.t51' ; Read Type: 51
lss=READKRC52(fin,ttt,uuu,vvv,itemt,itemu,itemv, /t51)
ii=size(lss) & if ii[0] ne 1 then goto,halt
k5=51 &  help,lss,ttt,uuu,vvv,itemt,itemu,itemv
narr=3; number of returned arrays
dimt=dim5                       ; dimensions of ttt
dimu=['lat' ,'item','case']     ; " " uuu
dimv=['seas','item','case']     ; " " vvv
alat=uuu[*,0,0] & elev=uuu[*,1,0] ; latitudes and elevations 
    end                      

52: begin & fin=parf[0]+parf[1]+'.t52';+ 52
lss=READKRC52(fin,ttt,uuu,vvv,itemt,itemu,itemv, ddd,ggg,itemd,itemg)
ii=size(lss) & if ii[0] ne 1 then goto,halt
k5=52 &  help,lss,ttt,uuu,vvv,itemt,itemu,itemv, ddd,ggg,itemd,itemg
narr=5; number of returned arrays
dimt=dim5                       ; dimensions of t
dimu=['lat' ,'item','case'] ; 
dimv=['seas','item','case'] ;  
dimd=['lay' ,'item','lat','seas','case'] 
dimg=['item','lat' ,'seas','case']
alat=uuu[*,0,0] & elev=uuu[*,1,0] ; latitudes and elevations
;carr=['t','u','v','d','g'] ; character in array name
end

54: begin & fin=parf[0]+parf[1]+'.t54' ;+ 54
lss=READKRC54(fin,ttt,itemt)
ii=size(lss) & if ii[0] ne 1 then goto,halt
k5=54 & help,lss,ttt,itemt ;t54 is [season, item,latitude,case]
narr=1; number of returned arrays
dimt=['seas','item','lat','case'] ; dimensions of t 
Print,'only one array, can skip @ 40,41'
ana='ttt' & aaa=ttt & ita=itemt & dima=dimt
    end  ; Guides and help

55: begin & fin=parf[0]+parf[1]+'.t55';+ 55
lss=READKRC54(fin,ttt,itemt,/t55)
ii=size(lss) & if ii[0] ne 1 then goto,halt
k5=55 & help,lss,ttt,itemt ;t55 is [season, item,case]
narr=1; number of returned arrays
dimt=['seas','item','case'] ; dimensions of t
Print,'single 2D array: Equiv of @40,41,42,43 done here,'
ana='ttt' & aaa=ttt & ita=itemt & dima=dimt  & ii=size(aaa)
nseas=ii[1] & nitem=ii[2] & ncase=ii[3] & nhour=-1 & nlat=-1
yy=DELCASE(ttt,pa=paw,hh=hh)    ;=41
sizy=size(yy)
if sizy[0] lt 2 then goto,halt
y2=yy & siz2=sizy & help,y2     ;=42 already in order
y3=yy & siz3=sizy               ;=43 only 2D, no need to reform
x3tit='Season'
end

56: begin & fin=parf[0]+parf[1]+'.t56';+ 56
;l56=READKRC56(fin, ts,tp,tlay,tlx,tsx,tltit,tstit)
lss=READKRC56(fin, ttt,uuu,vvv,ddd,ggg,itemd,itemg)
k5=56 &   help,lss,ttt,uuu,vvv,ddd,ggg,itemd,itemg
narr=5; number of returned arrays
dimt=['hour','lat','seas','case'] & itemt='Tsurf' ; dimensions of Ts
dimu=['hour','lat','seas','case'] & itemu='Tplan' ; dime of Tplan
dimv=['lay', 'lat','seas','case'] & itemv='Tmidn' ; dime of T_midnight_layer
dimd=['item','lat','seas','case']
dimg=['seas','item','case']
end

40: begin ; Guide to current file
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

4: begin & ka=0 & if narr lt 2 then goto,halt ; Select array 
again4: read,ka,prompt='Desired array index ? > '
if ka lt 0 or ka ge narr then begin 
    print,'invalid : allowed are:',0,narr-1
    goto, again4
endif 
kon=402 & goto,dokon & end

401: ka=pari[9] ;- set array from pari[9]

402: begin & if ka lt 0 or ka ge narr then goto,halt ; get one array
case ka of
    0: begin & ana='ttt' & aaa=ttt & ita=itemt & dima=dimt & end
    1: begin & ana='uuu' & aaa=uuu & ita=itemu & dima=dimu & end
    2: begin & ana='vvv' & aaa=vvv & ita=itemv & dima=dimv & end
    3: begin & ana='ddd' & aaa=ddd & ita=itemd & dima=dimd & end
    4: begin & ana='ggg' & aaa=ggg & ita=itemg & dima=dimg & end 
    endcase 
siza=size(aaa)
print,ka,' =k Array  '+ana+'  has:',ita,format='(i2,a,10a8)'
Print,'Dimensions ',siza[1:siza[0]],format='(2x,a12,6i8)'
print,'Contain: ',dima ,format='(2x,a12,6a8)'
jj=intarr(5) ; to hold sizes
for j=0,4 do begin
    i=where(dima eq dim5[j]) & i=i[0] 
    if i[0] gt 0 then i=siza[i+1]
    jj[j]=i
endfor
nhour=jj[0] & nitem=jj[1] & nlat=jj[2] & nhour=jj[3]  & ncase=jj[4]
end

41: begin & yy=DELCASE(aaa,jk=jk[0],hh=hh) ; Get one case
sizy=size(yy)
if sizy[0] lt 2 then goto,halt 
siz2=[0] & siz3=[0] & end

42: begin & yy=DELCASE(aaa,jk=jk,hh=hh,pa=paw); Difference two cases
sizy=size(yy)
if sizy[0] lt 2 then goto,halt
siz2=[0] & siz3=[0] & end

43: begin & if sizy[0] lt 2 then goto,halt; Transpose to hour,lat,season,item
ynw=['hour','seas','lat','item'] ; y order wanted
yng=strarr(6)                   ; to hold y order get
kyt=intarr(6)         ; to become transpose control vector, as big as ever need
k=-1                            ; index of transpose dimension 
for i1=0,3 do begin ; each desired imension
    i=where(dima eq ynw[i1]) & i=i[0] ; where is it now ?
    if i ge 0 then begin        ; found it
        k=k+1                   ; increment output location
        kyt[k]=i                ; source dimension 0-based index
        yng[k]=ynw[i1]          ; save name for titles
    endif
endfor
kyt=kyt[0:k] & yng=yng[0:k]     ; prune
y2=transpose(yy,kyt)            ; transpose
siz2=size(y2) & help,y2
j=siz2[1] & x3tit=yng[0]        ; values for first dimension of y2
if k gt 1 then for i=1,k-1 do begin
    j=j*siz2[i+1]               ; size of first dimension of y3
    x3tit=x3tit+' * '+yng[i]    ; X title for chart of y3
endfor
print,' Dimensions are:',yng
y3=reform(y2, j,siz2[k+1]) & siz3=size(y3)
end

44:  begin & if siz3[0] ne 2 then goto,halt; CHART items
CHART,y3,parti=ita,xtit=x3tit & end

45: begin if siz2[0] ne 3 then goto,halt;+ one item
print,ita
i=0 & read,i,prompt='Which item > '
i=(i>0)<(siz2[3]-1)
if yng[1] eq 'lat' then qq=alat else qq=indgen(siz[2])
CHART,y2[*,*,i],parti=ST0(qq,/nojoin), xtit=yng[0] & end

48: begin & if siz3[0] ne 2 then goto,halt ; Locate outliers in y3
nk=siz3[2] ; number of items
for k=0,nk-1 do begin 
    qq=y3[*,k]
    print,ita[k]
    HISTFAST,qq,i1=i1,r1=r1
    fout=(i1[1]+i1[2])/float(i1[0]) ; fraction outside 4 sigma plot
    if fout gt parr[1] then begin 
        ii=where(abs(qq) gt parr[2]*r1[4],j) ; within n sigma
        sit=siz2 & sit[0]=sit[0]-1 ;  reduce to dimensionality of one item
        if j gt 0 then ijk=LOC2INDEX(ii,siz=sit) else ijk=-1
        CHART,ijk,parti=yng[0:sit[0]-1]+' Index'
        PAUSE,paw
    endif
endfor
end

49: begin & if siz3[0] lt 2 then goto,halt ; Print outliers for one item
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

61: begin                       ; Read krccom for current file, get lats
jj=READKRCCOM(fin,holdk)        ; open file and compute sizes
if jj[0] lt 0 then goto,halt    ; read failure
kcom=READKRCCOM(1,holdk)        ; read one case
j=size(kcom,/type)              ; expect structure
if j ne 8 then goto,halt        ; something failed
j=kcom.id[3]-1                  ; last latitude
alat=kcom.alat[0:j]             ; latitudes
elev=kcom.elev[0:j]             ; elevations
sss=KRCHANGE(holdk)             ; find changes
free_lun,holdk[0]               ; close file and release the logical unit
print,'Basic: ',sss[0]          ; print them
sss[0]='Basic'                  ; avoid long title
if do6 then begin & kon=6 & goto,dokon & endif
end

6: begin & do6=0B & jj=size(kcom); Print krccom 
if jj[2] ne 8 then begin
    print,'Must read krccom. Will do this now and continue here'
    kon=61 & & do6=1B
goto,dokon & endif
; get labels for common items
kstru=DEFINEKRC('KRC', labkf,labki,labkl,idmin,idmax,mxn2=mxn2);,nword=nword)
idkf=STRWORD1(labkf,len=10)     ; extract ID and right-adjust
idki=STRWORD1(labki,len=10)     ; "
idkl=STRWORD1(labkl,len=7)      ; "
kode=pari[1]                    ; types DESIRED 
KRCCOMLAB ,kode,kcom.fd,kcom.id,kcom.ld, idkf,idki,idkl ; print commons
j=n_elements(sss)
if j gt 1 then for i=1,j-1 do print,i,sss[i],form='(i2,1x,a)' ; print them
end

66: begin & if siz2[0] lt 3 then goto,halt ; Plot one day of y2
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

71: TOTALCO2, aaa,vvv,sss,alat,kcom.fd[46],paw=parr[3] ; frost amounts. Req 52,

72: begin  & locc=[.3, .94, -.03, .07]; plot frost
qq=reform(ttt[*,7,*],/over) 
OPLOT2,qq,sss,['Ls','Frost kg/m^2','file='+fin],xin=lss,locc=locc,ccc=clrr[0:11]
end

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

halt:  print,'SOME ERROR CONDITION at kon=',kon,'.  Any key to Go'
i=get_kbrd(1) & goto,sureask
end
