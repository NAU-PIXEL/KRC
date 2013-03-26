function readkrc52, finame,ttt,uuu,vvv,itemt,itemu,itemv, ddd,ggg,itemd,itemg $
,t51=t51 
;_Titl  READKRC52  Read KRC type 52 or 51 bin5 file
; finame in.  String of file name
; ttt	out.	Fltarr(hour,item,latitude,season,case) Item labels are in itemt
; 			0= surface kinetic temperature
;			1= Top-of-atmosphere bolometric temperature
;		t52	2= one-layer atmosphere kinetic temperature
;		t52	3= Down-welling solar radiance 
;		t52	4= Down-welling thermal radiance
; uuu	out.	Fltarr(nlat,item,case) Item labels are in itemu
;			0= Latitude in degrees
;			1= elevation in Km.
; vvv	out.	Fltarr(season,item,case) Item labels are in itemv
;			0= Model season julian date - 2,440,000 
;			1= L-sub-S computed in KRC
;			2= Global mean pressure: PZREF
; itemt,u,v  out.   Strarr ID's for the items in ttt, uuu, and vvv
; Following 4 outputs not valid for Type 51
; ddd	out.    Fltarr(layer,item,latitude,season,case) Item labels are in itemd
;			Item:  0=Tmin    1=Tmax
; ggg	out.	Fltarr(item,latitude,season,case) Item labels are in itemg
;                Items: 'NDJ4','DTM4','TTA4','FROST4','AFRO4','HEATMM'
; itemd,g out.  Strarr ID's for the items in ddd, and ggg
; t51   in_     Flag. If set, presumes input file is type 51
; func. out.	Fltarr of L-sub-S corresponding to Jul.Day for the first case.
;                If an error, returns negative integer
;        -1: -4 are from READKRCCOM, -5 = failure here
;_Desc
; Need to first read bin5
; Then extract krccom and the size integers within it
; Expects that cases are similiar, with no change of # latitudes or seasons
; !dbug will cause debug actions
;_Calls  BIN5read  READKRCCOM
;_Desc.  Item description based on what tdisk.f saves for code 52.
;_Lims
;_Lien  uuu need be only each case.
;_Hist 2004jul21 Hugh Kieffer
; 2004Oct06 HK Revise to having up to 6 variables in the file
; 2004Oct26 HK Return -1 on file read error. Go from BIN5R to BIN5
; 2006mar25 HK Add   log   keyword
; 2006apr30 HK Remove keyword  log  and add keyword  jword
; 2008oct16-22 HK Major revision for new tdisk for type 52
; 2009feb25 HK Include option for Type 51. Reorder arguments
;_End

; help,finame,ttt,uuu,vvv,itemt,itemu,itemv,log,dbug,jword

; 51: [N24,2,Nlat,x+nseas,ncase]  
; 52: [N24,7,Nlat,x+nseas,ncase]   First x "seasons" of each case contains: 
; Initial block contains
; 4 real words  Float of NWKRC ,Dimension of xtra, Number of xtra, spare 
; followed by KRCCOM, DJU5(nseas),SUBS(nseas)
;   ,PZREF(nseas), TAUD(nseas), SUMF(nseas)
; 51: True seasons contain for every hour: TSF,TPF
; 52: True seasons contain for every hour: TSF,TPF,TAF,DOWNVIS,DOWNIR
;  and float(NDJ4)+ DTM4 + TTA4+ Tmin(Nlay-) Omitting virtual first layer
;  and FROST4+ AFRO4+ HEATMM+    Tmax(Nlay-) " " "
; the last two are as many layers as fit within the number of Hours

t52=not keyword_set(t51) 
BIN5,'R',finame,head,aaa,/verb
sizh=size(aaa)   ; [Hour or layer,7 items,latitude,1+season,[case]]
; items are [Ts,Tp,Ta,downVIS,downIR,<-- by hour   3+Tmin, 3+Tmax <-- by layer]
if sizh[0] lt 4 then begin 
    message,'Input file has wrong dimensions',/con
    return,-5
endif
if sizh[0] eq 4 then begin      ; only one case, force to 5 dimensions
    aaa=reform(aaa,sizh[1],sizh[2],sizh[3],sizh[4],1,/overwrite)
    sizh=size(aaa)
endif
front=READKRCCOM(finame,hold)   ; get the front words
if n_elements(front) lt 2 then return,front
kcom=READKRCCOM(1,hold)
mlat=n_elements(kcom.alat)      ; number of latitudes stored in KRCCOM
nlay=kcom.id[0]                 ; number of layers computed in KR
free_lun,hold[0]                ; close file
idx=front[1] & ndx=front[2]

itemt=['Tsurf','Tplan','Tatm','DownVIS','DownIR'];each hour,lat,season,case
if not t52 then itemt=itemt[0:1] ; Type 51 has only 2 items
itemu=['alat','elev']           ; each lat, case ; 
itemv=['DJU5','SUBS','PZREF','TAUD','SUMF'] ; each season,case
itemd=['Tmin','Tmax']           ; each layer,lat,season,case  t52 only
numt=n_elements(itemt)
numv=n_elements(itemv)
numu=n_elements(itemu)
if t52 then numd=n_elements(itemd) else numd=0
nhour=sizh[1]                   ; number of Hours. Expect 24 or 48
nv=sizh[2]                      ; number of variables. Expect 7[t52] or 2[t51] 
nlat=sizh[3]                    ; number of latitudes
ncase=sizh[5]                   ; each is a case
if nv ne numt+numd then message,'# items mismatch' ;
ksiz=fix(aaa[0:3,0,0,0,0])      ; get sizes
nwkrc=ksiz[0]                   ; or front[0]  ; # words in KRCCOM
nkay=(nlay-1)<(nhour-3)         ; Number of layers transfered
; nlat=ksiz[2]
ndx=front[2]                    ; number of virtual seasons containing krccom
nseas=sizh[4]-ndx               ; Number of true seasons
print,'# layers computed, transfered=',nlay,nkay
wpl=nhour*nv                    ; words per latitude
wps=wpl*nlat                    ; words per season
qq=reform(aaa[*,*,*,0:ndx-1,*],wps*ndx,ncase) ; extract header for each case
k=4+nwkrc+5*nseas               ; words in case header
krcc=reform(qq[4:3+nwkrc  ,*],nwkrc,ncase) ; extract krccom for each case
vvv =reform(qq[4+nwkrc:k-1,*],nseas,numv,ncase) ; extract season items " "
mjd=vvv[*,0]-11545.             ; Modified Julian date
lsubs=float(L_S(mjd,kode=0))    ; return is double, convert to single 
i=nwkrc-2*mlat                  ; words before start of latitude values
q=reform(krcc[i:*,*],mlat,2,ncase) ; all latitudes
uuu=q[0:nlat-1,*,*]             ; latitudes and elevations for each case
; done extraction of case header items
if !dbug then help,aaa,qq,krcc,vvv,uuu
wpc=wps*sizh[4]                 ; words per case
jword=[4,wpc,ncase]
ttt=aaa[*,0:numt-1,*,ndx:*,*]   ; hourly items

if t52 then begin 
   itemg=['NDJ4','DTM4','TTA4','FROST4','AFRO4','HEATMM'] ; each lat,season,case
    numg=n_elements(itemg)
    qq=aaa[*,5:6,*,ndx:*,*]     ; layer items
    ggg=reform(qq[0:2,*,*,*,*],6,nlat,nseas,ncase,/over) ; lat items
    ddd=qq[3:2+nkay,*,*,*,*]    ; hour, Tmin and tmax, lat, season,case
endif
if !dbug then begin 
    help,ttt,ddd,ggg,uuu,vvv,lsubs
    print,'Nhour,Nlay,Nlat,Nseas,Ncase=',Nhour,Nlay,Nlat,Nseas,Ncase
    STOP
endif
return,lsubs

end
