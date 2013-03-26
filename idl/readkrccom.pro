function readkrccom, arg1,hold
;_Titl READKRCCOM  Read a KRCCOM structure from a bin5 file 
; arg1  in. either String or integer
;           If File name, will open KRC type 5x file and return the front values
;           If a case number, will return krccom for that case (1-based)
; hold   both.  Storage of values needed between calls
;   DO NOT MODIFY except as specifically mentioned here
;   hold[0] is lun, the  logical unit of the bin5 file
;           If arg1 is string, will open new unit; if lun negative, will close
;                  abs(that) first, and get new unit 
;           Else expects valid open lun. Caller should do the final  free_lun,

; func.	out. If arg1 is string, intarr of the "front" words of KRC file +ncase
;             [1] is 1-based index with prepend index , [2] is number of them
;             [4] is ncase
;            If arg1 is integer, Structure KRCCOM for the requested case
;          If an error, returns negative integer
; -1= Failure to open file     -2= Unexpected IDX in file 
; -3= Invalid case requested   -4= arg1 of invalid type
;_Desc
; Because KRCCOM contains items of different binary types, only way for IDL to
; access these is to read from file as a structure. 
; Read the file using a structure for KRCCOM; spacing forward as needed
; Code so that could use with more than 1 file at a time; thus, all storage
; must be in arguments, requiring recalc. on each call; time trivial.
;_Use
; Call initially, q=READKRCCOM('filename',hold) where  hold  may be undefined
; To get a case  kcom=READKRCCOM(Icase,hold)  where hold is unchanged
; When done with file: free_lun,hold[0]
;_Hist 2004jul22 Hugh Kieffer
; 2008oct22 HK Complete replacement, including meaning of arguments.
;_End

k=size(arg1,/type) 
if k eq 7 then begin ; open file ;=========================

if n_elements(hold) lt 1 then lun=0 else lun=hold[0]
if lun lt 0 then begin
    lun=-lun 
    close,lun                   ; close the file and release the logical unit
endif else if lun eq 0 then get_lun,lun

OPENR, lun,arg1,error=operr     ; open the binary file
if operr ne 0 then begin
    print,'READKRCCOM: open ERROR. file--->',arg1
    print,' Error # and message=',operr,' ',strmessage(operr)
    return,-1
endif

; get bin5 array size
hlen=512                        ; default size of bin5 header in bytes
bb=bytarr(hlen)                 ; default header
readu,lun,bb                    ; read past the default bin5 heade
; code to find header size extracted from  bin5r.pro
hh=string(bb[0:128])            ; plenty long to include IDL size definition
i1=STRPOS(hh,'<<')              ; locate the end of the dimensions
idim=STRMID(hh,0,i1-1)          ; extract the dimensions part
idi=STRCOMPRESS(idim)           ; remove extra white space
idd=long(STR_SEP(idi,' '))     ; separate into its parts, the FIRST WILL BE NULL
n=n_elements(idd)               ; # items in bin5 size specification
ndim=idd[1]                     ; number of dimensions in array
ncase=idd[1+ndim]               ; number of cases
 ; KRC files never have  >512 byte header. So this never used
if n eq ndim+5 then begin       ; header length was written in header
    hlen=idd[n-1]               ; total length of header
    if hlen gt 512 then begin   ; should read the rest of the header
        more=hlen-512                   ; amount still to be read
        cc=bytarr(more)                 ; create array to hold rest of header
        READU,lun, cc                   ; read rest of header as a byte array
    endif
endif

; get the first 4 words of array; needed by user but not needed here.
front=fltarr(4)
readu,lun,front                 ; first 4 words of file
front=fix(front)                ; convert to integer
idx=front[1]
if idx ne ndim-1 then begin 
    message,'Unexpected IDX in file '+arg1,/con
    return,-2
endif
out=[front,ncase]
; compute number of words in a case , which is always the last dimension
mmm=idd[2]                   ; size of first dimension
if ndim gt 1 then for k=2,ndim-1 do mmm=mmm*idd[k+1]; size of last dimension
hold=[lun,mmm,ncase]

endif else if k lt 4 then begin  ; case number =====================

lun=hold[0]
if arg1 lt 1 or arg1 gt hold[2] then begin 
    print,'krccom: Invalid case: requested, max = ',arg1,hold[2]
    return,-3 
endif
j=512L+(arg1-1L)*4*hold[1]+16   ; bytes before krccom of requested case
point_lun,lun,j
out=DEFINEKRC('KRC')            ; define the structure
readu,lun,out                   ; read the structure from file

endif else begin  ; =====================
    message,'arg1 of invalid type',/con
    return,-4
endelse

done: if !dbug then stop
return,out
end
