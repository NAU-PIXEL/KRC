function readkrc1, file,fcom,icom,lcom, lsubs,ts,tp,  ktype=ktype $
                  , maxs=maxs,verb=verb ,ddd=ddd,lab=lab,desc=desc
;_Titl   READKRC1  Read KRC direct access files
; file	in.	String. file path name
; fcom	out	KRCCOM floating values, first season See KRCCOMLAB.pro
; icom	out	KRCCOM integer  values, first season
; lcom	out	KRCCOM logical  values, first season
; lsubs	out	fltarr(seasons) L_sub_s from krccom
; ts	out	Surface     kinetic temperature (hour,lat,season)
; tp	out	Planetary bolometic temperature (hour,lat,season)
; ktype	in_	Integer: KRC disk file kode  K4OUT
;     -1: TSF+TSP     With the first record being KRCCOM+filler (called by TSEAS)
;      0: KRCCOM+LATCOM   INCOMPLETE
;     +n (n<50): KRCCOM+DAYCOM  Not handled here!
; maxs	in_	Max number of seasons to read, Default = all in file.
;                if negative , will return only that 1-based season
; verb	in_	If set, will be verbose
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
; 	  If error occurs, will return -1.
;_Calls None
;_Hist 99dec06 Hugh Kieffer  00jan09 HHK add fclab
; 2002mar02 HK Reorder arguments, many no longer keywords. 
;   Remove common parameter print; now in  KRCCOMLAB
;   Add ktype argument to handle two file types
; 2002jul20 HK Revise to new common size
; 2010apr16 HK Update from readkrc.pro, add !dbug
; 2010sep04 HK Allow negative maxs to return single season
;_End

    krc1=DEFINEKRC('KRC',param,nword=nwkrc) 
; krc1=DEFINEKRC('KRC',param,nword=nwkrc,pid=pid)
; n=n_elements(pid) & q=' =param[' & q2='] ; '
; for i=0,n-1 do print,pid[i],q,i,q2,param[i],form='(a10,a,i2,a,i5)'
; definition of  KRCCOM
     MAXN1 =param[ 0] ;   30
     MAXN2 =param[ 1] ; 1536
     MAXN3 =param[ 2] ;   16
     MAXN4 =param[ 3] ;   37
     MAXN5 =param[ 4] ;  161
     MAXN6 =param[ 5] ;    6
     MAXNH =param[ 6] ;   48
    MAXBOT =param[ 7] ;    6
     NUMFD =param[ 8] ;   96
     NUMID =param[ 9] ;   40
     NUMLD =param[10] ;   20
    NUMTIT =param[11] ;   20
    NUMDAY =param[12] ;    5
     NWKRC =param[13] ;  255

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
if not keyword_set(ktype) then ktype=0
if ktype lt 0 then begin 
    nwtot=2*MAXNH*MAXN4
    ihead=1                     ; prepended records
endif else begin
    NWLAT= (9+ 3*MAXN1 + 2*MAXNH) *MAXN4 ; size of this latcom in 4-byte words
    nwtot=nwkrc+nwlat
    ihead=0
endelse

frec=float(len)/(4.*float(nwtot)) ; should equal an integer
nrec=len/(4*nwtot)
if vrb then print,'File length and # records=',len,frec,nrec
nread=fix(nrec)-ihead                 ; number of season records in the file
if not keyword_set(maxs) then maxs=0
if maxs ne 0 then nread= abs(maxs) <nread ; number of seasons to read

if ktype lt 0 then begin ; one KRCCOM [+ filler] , then TSF & TPF only 
    READU,lun1,krc1
    nhour=krc1.id[5] & lasth=nhour-1 ; N24
    nlat=krc1.id[3]  & lastlat=nlat-1 ; N4
    if vrb then print,'nhour & nlat=',nhour,nlat
    lev=reform([krc1.alat[0:lastlat],krc1.elev[0:lastlat]],nlat,2)
    fcom=krc1.fd                     ; return first season
    icom=krc1.id                     ;  "
    lcom=krc1.ld                     ; "
    n5=krc1.id[5-1] & jdisk=krc1.id[12-1]
    djul=krc1.fd[41-1] & deljul=krc1.fd[42-1]
    dates=(djul-11545.)+ deljul*(findgen(n5-jdisk+1)+(jdisk-1)); days from J2000
    lsubs=float(L_S(dates))        ; compute Ls (returns dblarr)
    dummy=fltarr(nwtot-nwkrc)
    READU,lun1,dummy            ; skip rest of first record.
    if maxs ge 0 then begin 
        ts=fltarr(nhour,nlat,nread)
        tp=fltarr(nhour,nlat,nread)
    endif 
    for k=0,nread-1 do begin 
        READU,lun1,tsf,tpf
        if maxs ge 0  then begin
            ts[*,*,k]=tsf[0:lasth,0:lastlat] ; surface kinetic temperature
            tp[*,*,k]=tpf[0:lasth,0:lastlat]
        endif else if k eq nread-1 then begin
            ts=tsf[0:lasth,0:lastlat] ; one season only
            tp=tpf[0:lasth,0:lastlat]
        endif
    endfor
;stop
endif else if ktype eq 0 then begin ; records of  krccom & latcom

plab=['DTM4','TST4','TTS4','TTB4','FROST4','AFRO4']
mpar=n_elements(plab)           ; number of parameters to be extracted

if arg_present(lab) then lab=plab
if arg_present(desc) then desc=[' rms Temp. change on last day' $
       ,'equilibrium temperature' $
       ,'mean surface temperature','mean bottom temperature' $
       ,'frost amount kg/m^2','frost albedo']

latcom=DEFINEKRC('LAT',nword=nword)
  message,'2010apr16 DID NOT COMPLETE update using definekrc'
    for k=0,nread-1 do begin
        READU,lun1,fd,id,ld,bd,alat,elev
        if k eq 0 then begin
            nhour=id[5] & lasth=nhour-1 ; N24
            nlat=id[3]  & lastlat=nlat-1 ; N4
            if vrb then print,'nhour & nlat=',nhour,nlat
            ddd=fltarr(nlat,nread,mpar) ; to hold daily averages
            lsubs=fltarr(nread)    ; to hold L_sub_s
            ts=fltarr(nhour,nlat,nread)
            tp=fltarr(nhour,nlat,nread)
            lev=reform([alat[0:lastlat],elev[0:lastlat]],nlat,2)
            fcom=fd             ; return first season
            icom=id             ;  "
            lcom=ld             ; "
        endif
        READU,lun1,ndj4,dtm4,tst4,tts4,ttb4,frost4,afro4,tta4,ttx4 $
          ,t4,tin,tax,tsf,tpf
        lsubs[k]=fd[44]            ; l_sub_s
        if arg_present(ddd) then begin ;   | extract items useful
            ddd[*,k,0]=  dtm4[0:lastlat] ; | where seasonal frosts
            ddd[*,k,1]=  tst4[0:lastlat]
            ddd[*,k,2]=  tts4[0:lastlat]
            ddd[*,k,3]=  ttb4[0:lastlat]
            ddd[*,k,4]=frost4[0:lastlat]
            ddd[*,k,5]= afro4[0:lastlat]
        endif
        ts[*,*,k]=tsf[0:lasth,0:lastlat] ; surface kinitic temperature
        tp[*,*,k]=tpf[0:lasth,0:lastlat]
    endfor

endif else begin
    Message,'Not coded for positive KTYPE',/continue
    lev=-1
endelse

free_lun,lun1
i= nread -( id[4]-id[11]+1)
if vrb and i gt 0 then print,'Additional records in file =',i
if !dbug then stop
return,lev
end
