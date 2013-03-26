pro bin5, code, name,head,aa, verb=verb,arc=arc,note=note,quiet=quiet $
,date=date,exc=exc
;_Titl  BIN5  Write/Read numeric binary files with 'standard' header
; code	in.	String. valid are 'Write' or 'Read', case insensitive;
;		  Only the first character required.
;                 If the second character is 'b' (case insensitive), then the
;                 standard extension .bin5' will be appended to the file name.
; name	in.	Full path file name. [optionally without the  .bin5]
; head	in/out	String text for header in the file; any length.
; aa	in/out	Numeric array, any size; NOT structures or strings.
;		For Read, if open error occurs, this will be the error number.
; verb	in_	Integer verbosity: If set, will print file name before file
; 		action. If =2, for Read, will also print file date
;		>= 3, will request confirmation. If no conf., aa is
;		unaltered.
;               =4 will stop before return
; note in_	String notification of what the array represents. Ignored if
;                verb is not set
; arc	in/out_	String of file byte architecture
;		for Read; will report the byte order in the file
;	   for Write, can override the "Native" byte order. (Not recommended)
;		 Valid are in Liens below
; exc  out_    Integer: 0= no action  1=request executed. Relevent for verb=3
; READ ONLY
; quiet	in_ 	If set, will not report swap or missing file. 
; date	out_	String of creation date& time
;
;_USAGE example
; bin5,'W', file_name, descriptive_text_for_file, numeric_array_to_write
;_bin5,'R', file_name, descriptive_text_from_file, numeric_array_from_file
;_bin5,'Rb', file_name without extension,  header, numeric_array_from_file
;_Desc 
; The header is ASCII and its length is always a multiple of 512-bytes. 
; The first part contains the dimensions of the array as an IDL  SIZE vector,
; followed by the header size, then the date and time. This required 
; section ends with  >>.  After this comes the user-supplied text. 
; The header is blank-filled up to the last 10 or 13 bytes, which are always 
; the byte-architecture used to write the file followed by 'C_END'.
;
; There is a corresponding FORTRAN program  binf5.f 
; If there is an error opening the file, return is a scalar of  operr.
; Will print warning if non-numeric file is read. 
; This routine does byte-swapping on Read if and only if needed.
; Includes warning and required response if new file name is "dangerous"
;
; With advent of 64-bit machines, the architecture word could be 6 [or more?]
; characters long, whereas the prior design of this routine accommodated
; only 5. Rather than use a 5-byte version of the these longer words,
; the space allocated to the architecture word has been increased to 8 bytes.
; For backward compatibility, the extra code has been included to check for 
; 5-byte architecture names.
;_Lims 
types='sparc alpha ppc x86 x86_64 ' ; all types handled,
;            All initial archs must be of same type
;_Calls  None beyond RSI
;_Hist  97apr27 Hugh Kieffer  Began development of bin5r and bin5w
; 2004aug03 HK Combine bin5r and bin5w
; 2007apr14 HK Add x86_64 architecture, and verb=4 will stop before return
;              and allow second character of code to control extension
; 2007nov25 HK Add return keyword exc to indicate response to verb=3 query
; 2008mar31 HK Add type: ppc.  And use 1 & 2 for types, rather than 0 & 1.
;_End

kode =strupcase(strmid(code,0,1)) ; action requested
fame=name
if strupcase(strmid(code,1,1)) eq 'B' then fame=fame+'.bin5'; autoextension

if not KEYWORD_SET(verb) then kwv=0 else kwv=verb
if not KEYWORD_SET(note) then note=''

valid=[1,2,3,4,5,6,9,12,13,14,15] ; all the numeric types
arch=!VERSION.ARCH		; get the byte architecture of this computer
postyp0= 5 ; byte position in  types  between last type 1 and first type 2  
; blank would yield position of -1, which is equivalent to type 1 
i=STRPOS(types,arch)  & if i gt postyp0 then here=2 else here=1
headlen = 512                   ; default (minimum) total length of header
barc=8  & bend=5                ; bytes reserved for architecture and 'C_END'
headend=barc+bend            ; Number of bytes reserves for architecture+'C_END'
barc1=5                  ; pre-2007apr number of bytes reserved for architecture
exc=1                           ; default is to do the action requested

if kode eq 'R' then goto, read
if kode ne 'W' then message,'Invalid code'

;=====================================WRITE=================================

siza=size(aa)
j=siza[siza[0]+1]                   ; file type code
i=WHERE(valid eq j,k)           ; check that type is numeric
if k ne 1 then message,'Invalid array type of '+string(j) ; will halt here

; Look for dangerous characters
; Note, idl (or Linux) will not allow a null file name
danger=[' ','$','//','''']      ; dangerous characters in a file name, last is '
j=0                             ; error count
for i=0,N_ELEMENTS(danger)-1 do begin
    k=STRPOS(fame,danger(i))    ; is this character in the users file name?
    if k ge 0 then begin        ;  yup
        j=j+1                   ; increment error count
        print,'DANGER: found -->'+danger(i)+'<-- in file name at location ',k
    endif
endfor
if j ne 0 then begin            ; at least one dangerous character found
    print,'Requested file name is -->'+fame+'<--'
    ss='I want it'              ; intentionally difficult string
    q=' '                       ; define type
    read,q,prompt=' Enter precisely -->'+ss+'<-- to proceed: '
    if q ne ss then begin       ; compare response
        print,'No confirmation, BIN5 quitting'
        exc=0
        return
    endif else print,'OK, you asked for it!'
endif
if kwv ge 1 then print,'Will write '+note+' file: ',fame,' Size= ',strtrim(siza,2)
if kwv ge 3 then begin ; ask for confirmation
    read,prompt='Enter 19 to confirm ',q 
    if q ne '19' then  begin
        print,'No confirmation, BIN5 quitting'
        exc=0
        return
    endif
endif

OPENW,iod,fame,error=operr,/get_lun	; open the binary output file
if operr ne 0 then begin
	print,'BIN5-W: open ERROR # and file=',operr,fame,strmessage(operr) 
        if N_ELEMENTS(iod) lt 1 then return ; iod not yet defined
        goto, done              ; release iod before return
endif

; In the rare circumstance that the additional digit in the revised header
; length pushes the required length to one more block, the header-creation 
; section will simply be executed one more time.

makehead:		; construct header
ss=[siza,headlen]            ; append the header length to array size and type
hh = ''	; construct ASCII header of SIZE and headlen
for i=0,ss[0]+3 do hh=hh+' '+strtrim(string(ss[i]),2)	 ; add dimensions 
;; hh=strtrim([siza,headlen],2) NOPE, makes an array
hh = hh+' <<IDL_SIZE + headlen '+SYSTIME(0)+' >>'+ head ; add time & input text
j=STRLEN(hh)			; get current header length
;;;print,'j,headlen=',j,headlen
if j+headend gt headlen then begin	; require larger header
	nblocks= 1+(j+9)/512	;   compute number of 512-byte blocks required
	headlen=512*nblocks	;   set revised header length
	goto, makehead		;   repeat entire header construction
    endif
for i=j+1,headlen do hh=hh+' '	; extend header to fixed length
file=here                       ; default is no-swap
if keyword_set(arc) then begin  ; change type
    i=STRPOS(types,arc)         ; look for match to valid type
    if i ge 0 then begin        ; only if user request was valid
        if i gt postyp0 then file=2 else file=1
        arch=arc                ; replace type in header
    endif
endif

STRPUT,hh,arch,headlen-headend	; write hardware architecture in fixed location
STRPUT,hh,'C_END',headlen-bend	; write standard "end" to header
bb=byte(hh)                     ;  & help,bb

if here eq file then WRITEU,iod, bb,aa  $ ; write the header & array
                else WRITEU,iod, bb,SWAP_ENDIAN(aa)
if kwv ne 0  then print,'BIN5 Wrote file ',fame
goto,done

read: ;=========================== READ===========================

kwq=KEYWORD_SET(quiet)

GET_LUN,iod			; get a logical unit
OPENR, iod,fame,error=operr	; open the binary file
if operr ne 0 then begin
    if not kwq then begin       ; Default error reporting
        print,'BIN5-R: open ERROR. file--->',fame
        print,' Error # and message=',operr,' ',strmessage(operr)
    endif
    aa=operr
    goto,done
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
siza=long(idd[1:ndim+3])	; SIZE of array

j=siza[siza[0]+1]                   ; array type
i=WHERE(valid eq j,k)           ; check that type is numeric
if k ne 1 then Print,'BIN5-R WARNING; Invalid array type of '+string(j)

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
arc=strtrim(STRMID(hh,headlen-headend,barc),2) ; extract architecture word
i=STRPOS(types,arc)             ; look for match to valid types
if i lt 0 then begin            ; was not there, try old style 5-byte
    headend=barc1+bend
    arc=STRMID(hh,headlen-headend,barc1) ; look for 5-byte architecture word
    i=STRPOS(types,arc)         ; and a match in the valid set
endif
if i lt 0 then message,'Unrecognized architecture '+arc,/con
if i gt postyp0 then file=2 else file=1
;;; print,'here and file=',here,file

i2=STRPOS(hh,'>>') 		; find end of date
i1=i2-21                        ; skip past 'IDL_SIZE + headlen'
date=STRMID(hh,i1,20)           ; extract date/time
head=STRMID(hh,i2+2,headlen-headend-3-i2) ; extract range that may contain text
head=STRTRIM(head)		; and trim trailing blanks

if kwv ge 1 then print,'Will Read '+note+' file: ',fame,' Size= ',strtrim(siza,2)
if kwv ge 2 then print, '  File date= ', date $
	  ,'   Head_length= ',strtrim(strlen(head),2)
if kwv ge 3 then begin ; ask for confirmation
    read,prompt='Enter 19 to confirm ',q 
    if q ne '19' then  begin
        print,'no confirmation, BIN5-R quitting' 
        exc=0  
        goto,done
    endif
endif

aa=MAKE_ARRAY(size=siza)		; create the array
READU,iod, aa			; read the data array
if here ne file then begin 
    if (not kwq) and kwv then print,'BIN5 R: swapping array of size= ',idi
    aa=SWAP_ENDIAN(aa)
endif

done: ;===================== DONE =====================

if kwv eq 4 then stop
FREE_LUN,iod                    ; close the file and free up the logical unit
return
end
