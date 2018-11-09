function strum, ss,xx, join=join,quiet=quiet, rem=rem, nix=nix
;_Titl  STRUM  Separate or concatonate strings into one using separator.
; ss	in.	String with items to be unjoined or Strings to be joined.
; xx	in. 	Single character used as separator.
; join	in_ 	If set, joins strings; else, separates them.
;               If == -2 then, extracts existing joined string from ss
; quiet	in_	If set, no message if separation character not found.
; rem	in_	If set, will try to remove a STRUM entry from sss
; nix	out.	Integer count of number of strings that contained an xx
;		Used only if join is set. 
; func.	out.	Strings that have been unjoined or joined.
; 		If rem was set, string cleaned of that STRUM
;		If error, returns 'ERROR'
;_Desc
; Double separation character is used at both ends of cat-string. 
; The separation character should NOT occur within any string element.
; This methodology allows blanks within string elements.
; E.g., if xx is '|' then a joined string might be:
;    '||one|this is second string|three||'
;_Calls  None beyond standard IDL
;_Hist  2001apr02 Hugh Kieffer
; 2001oct07 HK Add warning if separation character occurs within a string
; 2004aug17 HK Add error print and return
; 2006jan02 HK Add warning if separator pair occurs more than twice in input.
; 2007mar23 HK Add   quiet  option
; 2007apr03 HK Add    rem   option
; 2012nov12 HK Add option to return the joined string with no change
; 2018aug20 HK No error msg if seperator missing. Change IDL routine calls to lower-case
;_End

verb= not keyword_set(quiet)
ierr=0 ; error count
n=n_elements(ss) & if n lt 1 then goto,err1
if n_elements(xx) ne 1 then goto,err2
if strlen(xx) ne 1 then goto,err2
x2=xx+xx                         ; form end expression
if not keyword_set(join) then join=0
out='' ; in case no vector for this xx
if join eq -2 then begin        ; extract joined string as is
    I=strpos(ss,x2)             ; look for start
    if i lt 0 then goto,err4    ; this strum not in the input string
    j=strpos(ss,x2,i+2)         ; look for end
    if j lt 0 then goto,err5 
    out=strmid(ss,i,j-i+2)      ; extract existing joined items
endif else if keyword_set(rem) then begin ; look for prior STRUM to remove
    I=strpos(ss,x2)              ; look for start
    if i lt 0 then return,ss    ; this strum not in the input string
    j=strpos(ss,x2,i+2)         ; look for end
    if j lt 0 then goto,err5
    out=strmid(ss,0,i)+ strmid(ss,j+2) ; split the string
endif else if join ne 0 then begin ; join strings
    nix=0                       ; count of strings that contain the separator 
    for i=0,n-1 do if strpos(ss[i],xx) ge 0 then nix=nix+1
    out=x2+ss[0]                ; double to start
    if n gt 1 then for i=1,n-1 do out=out+xx+ss[i] ; interior
    out=out+x2                  ; add double character at end
    if nix gt 0 then goto,err3
endif else begin                ; separated strings
    I=strpos(ss,x2)             ; look for start
    if i lt 0 then goto,err4
    j=strpos(ss,x2,i+2)         ; look for end
    if j lt 0 then goto,err5
    out=str_sep(strmid(ss,i+2,j-i-2),xx) ; split the string
    k=strpos(ss,x2,j+2)
    if k ge 0 then goto,err6
endelse

done: return,out

;------------- Error section --------------------------------------------

err6:ierr=ierr+1
err5:ierr=ierr+1
err4:ierr=ierr+1
err3:ierr=ierr+1
err2:ierr=ierr+1
err1:ierr=ierr+1
errs=['null','String not defined','Separator not a single character' $
,'Separator was found in input strings','No ends found' $
,'No 2nd end found','WARNING, separator_pair appears more than twice']

if verb or ierr eq 6 then begin 
    print,'STRUM: error: ',errs[ierr]
    help,ss,xx,join,ierr
    help,/trace
    print, ' .con will return "ERROR" or null '
    stop
endif
if ierr ne 4 then out='ERROR'
goto,done

end
