function bin5r,  name, head=head,verb=verb,quiet=quiet,date=date
;_Titl  BIN5R  Read numeric binary files with 'standard' header
; name	in. 	File name
; head	out_	String of text supplied when file written.
; verb	in_	If set, will print file name & size after reading it
;		 If =2, will also print file date
; quiet	in_ 	If set, will not report swap or missing file. 
; date	out_	String of creation date& time
; func.	out.	Binary array from the file.
;
;_USAGE  array = bin5r (file_name, head=descriptive_text_in_file)
;
;_Desc      This routine matched with  BIN5W  and  Fortran  binf5.f
; If there is an error opening file, return is a scalar of  operr.
; See  BIN5W for description of file format.
; Will print warning if non-numeric file is read. 
; This routine does byte-swapping if and only if needed.
;_Calls  None beyond RSI
;
;_Hist  97apr27 Hugh Kieffer   98mar15 revise header handling
; 98may08 HK check byte architecture, swap if necessary
; 98jul11 HK make  head  optional
; 99jan29 HK include quiet
; 99mar19 HK allow longer headers
; 99oct28 HK minor change to error message
; 99nov02 HK add verbose option
; 99dec30 HK minor fix; make ss long rather than string 
; 2000oct03 HK Add check for array type.
; 2001jan28 HK use ST0 for swap report
; 2001sep14 HK Add date argument
; 2001dec18 HK Add option of verb=2 to print filedate
; 2002feb13 HK Print swap notice only if /quiet not set and /verb is set. 
; 		Remove use of ST0
;_End

headlen = 512 			; default length of header
kwv=KEYWORD_SET(verb)
kwq=KEYWORD_SET(quiet)

GET_LUN,iod			; get a logical unit
OPENR, iod,name,error=operr	; open the binary file
if operr ne 0 then begin
    if not kwq then begin       ; Default error reporting
        print,'BIN5R: open ERROR. file--->',name
        print,' Error # and message=',operr,' ',strmessage(operr)
    endif
    FREE_LUN,iod
    return,operr
endif

bb=bytarr(headlen)	; create a dummy buffer for reading header
READU,iod, bb		; read first 512-bytes as byte array
hh=string(bb)		; convert this to a string
i1=STRPOS(hh,'<<')	; locate the end of the dimensions
idim=STRMID(hh,0,i1-1)	; extract the dimensions part
idi=STRCOMPRESS(idim)	; remove extra white space
idd=STR_SEP(idi,' ')	; separate into its parts, the first will be null
n=n_elements(idd) ;;; help,idd & for i=0,n-1 do print,i,' >',idd(i),'<'
ndim=idd[1]		; number of dimensions in array
ss=long(idd[1:ndim+3])	; SIZE of array

j=ss[ss[0]+1]                   ; array type
valid=[1,2,3,4,5,6,9,12,13,14,15] ; all the numeric types
i=WHERE(valid eq j,k)           ; check that type is numeric
if k ne 1 then Print,'Warning; invalid array type of '+string(j)


; can accomodate headers > 512 only if headlen was appended to dimensions
if n eq ndim+5 then begin	; header length was written in header
    hlen=idd[n-1]		; total length of header
    if hlen gt 512 then begin	; should read the rest of the header
	more=hlen-512			; amount still to be read
	cc=bytarr(more)			; create array to hold rest of header
	READU,iod, cc			; read rest of header as a byte array
	bb=[bb,cc]			; add to first part of header
	headlen=hlen			; update the header length
	hh=string(bb)			; convert entire header to a string
    endif
endif
;;;print,'5r headlen=',headlen
;determine if byte order in file is different than on this cpu
arch=!VERSION.ARCH		;;; &  print,'>',arch,'<cpu'
farch=STRMID(hh,headlen-10,5)	;;; & print,'>',farch,'<file'
types='sparc alpha x86   ' ; all types handled, blank assummed to be sparc
i=STRPOS(types,arch)  & if i gt 5 then here=1 else here=0
i=STRPOS(types,farch) & if i gt 5 then file=1 else file=0
;;; print,'here and file=',here,file


i2=STRPOS(hh,'>>') 		; find end of date
i1=i2-21                        ; skip past 'IDL_SIZE + headlen'
date = STRMID(hh,i1,20)  ; extract date/time, will be return if arg. present 

if ARG_PRESENT(head) then begin ; return header through the HEAD keyword
; trim off sizes and C_END
    head=STRMID(hh,i2+2,headlen-13-i2) ; extract range that may contain text
    head=STRTRIM(head)		; and trim trailing blanks
 endif

aa=MAKE_ARRAY(size=ss)		; create the array
READU,iod, aa			; read the data array
if kwv then begin               ; /Verbose
    print,'BIN5R read: '+name+'  Size & HeadLen= '+idi
    if verb ge 2 then print, '  File date = ',date
endif
FREE_LUN,iod                    ; close  and free up the logical unit
if here eq file then return,aa else  begin 
    if (not kwq) and kwv then print,'BIN5R swapping array of size= ',idi
    return,SWAP_ENDIAN(aa)
endelse
end
