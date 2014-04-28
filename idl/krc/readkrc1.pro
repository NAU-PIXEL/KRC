function readkrc1, file,fcom,icom,lcom, lsubs,ts,tp,  ktype=ktype $
         , maxs=maxs,verb=verb, dates=dates ,ddd=ddd,lab=lab,desc=desc
;_Titl   READKRC1  Read KRC direct access files
; file	in.	String. file path name
; fcom	out	KRCCOM floating values, first season See KRCCOMLAB.pro
; icom	out	KRCCOM integer  values, first season
; lcom	out	KRCCOM logical  values, first season
; lsubs	out	fltarr(seasons) L_sub_s. For type -1 computed here assuming Mars
; ts	out	Surface     kinetic temperature (hour,lat,season)
; tp	out	Planetary bolometic temperature (hour,lat,season)
; ktype	in_	Integer: KRC disk file kode  K4OUT. Default is 0
;     -1: TSF+TSP  With the first record being KRCCOM+filler (called by TSEAS)
;      0: KRCCOM+LATCOM
;     +n (n<50): KRCCOM+DAYCOM  Not handled here!
; maxs	in_	Max number of seasons to read, Default = all in file.
;                if negative , will return only that 1-based season
; verb	in_	If set, will be verbose
; dates out_    Fltarr(seasons) DJUL at each season; Type -1 computed here 
;Next 3 are undefined or meaningless for ktype negative.
; ddd	out_	fltarr[ lats, seasons, 6 items]
; 	  Items are:
;	 0] DTM4   = rms temperature change on last day
;	 1] TST4   = equilibrium temperature
;	 2] TTS4   = mean surface temperature for each latitude
;	 3] TTB4   = mean bottom temperature
;	 4] FROST4 = frost amount kg/m^2.
;	 5] AFRO4  = frost albedo.
; lab	out_	short labels for the 6 items in the function return
; desc	out_	descriptions  of the 6 items
; func.	out. fltarr (nlat,2)  0]=latitude  1]=elevation used
; 	     If error occurs, will return -1.
; !dbug: ge 7 stops before return 
;_Calls None
;_Hist 99dec06 Hugh Kieffer  00jan09 HHK add fclab
; 2002mar02 HK Reorder arguments, many no longer keywords. 
;   Remove common parameter print; now in  KRCCOMLAB
;   Add ktype argument to handle two file types
; 2002jul20 HK Revise to new common size
; 2010apr16 HK Update from readkrc.pro, add !dbug
; 2010sep04 HK Allow negative maxs to return single season
; 2013aug30 HK Assume DJUL date for distinction of KRC Versions
; 2014jan29 HK Add keyword dates. Clarify when this and Ls are calculated here
; 2014apr25 HK Use ktype=-2 to indicate old krc that did not have MAXN5
;_End

dv2=5000. ; 2013-09-09 12:00   switch date between assumed Version 1 and 2
;^^^^^^^^^^^ firmcode

if not keyword_set(ktype) then ktype=0
krc1=DEFINEKRC('KRC',param,nword=nwkrc) ; define structure == krccom
; krc1=DEFINEKRC('KRC',param,nword=nwkrc,pid=pid)
; n=n_elements(pid) & q=' =param[' & q2='] ; '
; for i=0,n-1 do print,pid[i],q,i,q2,param[i],form='(a10,a,i2,a,i5)'
; definition of  KRCCOM
     MAXN1 =param[ 0] ;   30
     MAXN2 =param[ 1] ; 1536
     MAXN3 =param[ 2] ;   16
     MAXN4 =param[ 3] ;   37
     MAXN5 =param[ 4] ;  161 
if ktype eq -2 then i=1 else i=0
     MAXN6 =param[ 5-i] ;    6
     MAXNH =param[ 6-i] ;   48
    MAXBOT =param[ 7-i] ;    6
     NUMFD =param[ 8-i] ;   96
     NUMID =param[ 9-i] ;   40
     NUMLD =param[10-i] ;   20
    NUMTIT =param[11-i] ;   20
    NUMDAY =param[12-i] ;    5
     NWKRC =param[13-i] ;  ---- 255

bd=bytarr((numtit+numday)*4)
vrb=keyword_set(verb)           ; verbose flag
operr=0
openr,lun1,file,error=operr,/get_lun     ; ,/F77_UNFORMATTED
if operr ne 0 then begin
    print,' readkrc1: open ERROR. file--->',file
    print,' Error # and message=',operr,' ',strmessage(operr)
    return,-1
endif
status=fstat(lun1)
len=status.size
;--------------------------------------------------------------------
fd=fltarr(numfd) & id=lonarr(numid) & ld=lonarr(numld)
ALAT = fltarr(MAXN4)            ; latitude in degrees. set in  TCARD
ELEV = fltarr(MAXN4)            ; elevation in km. set in  TCARD
TSF = fltarr(MAXNH,MAXN4)      ; final hourly surface temperature
TPF = fltarr(MAXNH,MAXN4)      ; final hourly planetary temperature

;--------------------------------------------------------------------
if ktype lt 0 then begin        ; type -1
   nwtot=2*MAXNH*MAXN4
   ihead=1                               ; prepended records
endif else begin                         ; type 0
   NWLAT= (9+ 3*MAXN1 + 2*MAXNH) *MAXN4  ; size of this latcom in 4-byte words
   nwtot=nwkrc+nwlat
   ihead=0
endelse

frec=float(len)/(4.*float(nwtot)) ; should equal an integer
nrec=len/(4*nwtot)
if vrb then print,'File length and # records=',len,frec,nrec
nread=fix(nrec)-ihead                 ; number of season records in the file
if not keyword_set(maxs) then maxs=nread

; Read the first krccom, holds of types -1, 0 and +<50
READU,lun1,krc1
nhour=krc1.id[5] & lasth=nhour-1      ; N24
nlat=krc1.id[3]  & lastlat=nlat-1     ; N4
if vrb then print,'nhour & nlat=',nhour,nlat
lev=reform([krc1.alat[0:lastlat],krc1.elev[0:lastlat]],nlat,2) ; lat and elevation
fcom=krc1.fd                    ; return first season
icom=krc1.id                    ;  "
lcom=krc1.ld                    ; "
n5=krc1.id[5-1] & jdisk=krc1.id[12-1]
djul=krc1.fd[41-1] & deljul=krc1.fd[42-1]
nsx=n5-jdisk+1 ; number of seasons expected based on KRCCOM
Print,'nread,nsx,maxs=',nread,nsx,maxs
if nsx ne nread then message,'NumSeas disagree',/con
nread=(maxs<nsx)<nread ; number of seasons to read

; dates and lsubs will be overwritten below for Type -1
if djul gt dv2 then begin       ; assume version 1
   dates=(djul-11545.)+ deljul*(findgen(nread)+(jdisk-1)) ; days from J2000
   lsubs=float(L_S(dates))      ; compute Ls (returns dblarr)
   print,'READKRC1: Assuming -2440000 dates'
endif else begin                ; assume version 2
   dates=djul+ deljul*(findgen(nread)+(jdisk-1))
;   lsubs=LSMARS(1,dates)        ; uses MJD
;   lsubs=lsubs[*,0]             ; drop the A.U and sub-solar latitude
   lsubs=LSAM(dates,myn,aud)        ; uses MJD
endelse
ts=fltarr(nhour,nlat,nread) ; Create arrays just the right size for Tsur
tp=fltarr(nhour,nlat,nread) ;  and Tplan even if read only one

if ktype lt 0 then begin ; ==============================================
   dummy=fltarr(nwtot-nwkrc)    ; filler in first record
   READU,lun1,dummy             ; skip rest of first record.
   tsf=fltarr(MAXNH,MAXN4)  ; final hourly surface temperature 
   TPF=fltarr(MAXNH,MAXN4)  ; final hourly planetary temperature
   for k=0,nread-1 do begin     ; read the remaining records
      READU,lun1,tsf,tpf
      ts[*,*,k]=tsf[0:lasth,0:lastlat] ; surface kinetic temperature
      tp[*,*,k]=tpf[0:lasth,0:lastlat]
   endfor

endif else if ktype eq 0 then begin  ; =======================================
   plab=['DTM4','TST4','TTS4','TTB4','FROST4','AFRO4']
   mpar=n_elements(plab)        ; number of parameters to be extracted
   if arg_present(lab) then lab=plab
   if arg_present(desc) then desc=[' rms Temp. change on last day' $
       ,'equilibrium temperature' $
       ,'mean surface temperature','mean bottom temperature' $
       ,'frost amount kg/m^2','frost albedo']
   ddd=fltarr(nlat,nread,mpar)       ; to hold latcom extracts
   latc=DEFINEKRC('LAT',nword=nword) ; define structure == latcom
   for k=0,nread-1 do begin
      if k gt 0 then READU,lun1,krc1 ; read krccom
      dates[k]=krc1.fd[40]      ; 41-1 extract DJUL
      lsubs[k]=krc1.fd[44]      ; 45-1 and SUBS
      READU,lun1,latc           ; read latcom
      if arg_present(ddd) then begin       ; | extract items useful
         ddd[*,k,0]=latc.dtm4[0:lastlat] ; | where seasonal frosts
         ddd[*,k,1]=latc.tst4[0:lastlat]
         ddd[*,k,2]=latc.tts4[0:lastlat]
         ddd[*,k,3]=latc.ttb4[0:lastlat]
         ddd[*,k,4]=latc.frost4[0:lastlat]
         ddd[*,k,5]=latc.afro4[0:lastlat]
      endif
      ts[*,*,k]=latc.tsf[0:lasth,0:lastlat] ; surface kinetic temperature
      tp[*,*,k]=latc.tpf[0:lasth,0:lastlat]
   endfor
endif else begin  ; =======================================
    Message,'Not coded for positive KTYPE',/continue
    lev=-1
endelse          ; =======================================

free_lun,lun1
i= nread -nsx ; 
if vrb and i gt 0 then print,'Additional unread records in file =',i
if !dbug ge 7 then stop
return,lev
end
