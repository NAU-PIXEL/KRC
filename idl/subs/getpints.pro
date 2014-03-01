PRO getpints, desc, var,low,high
;_Titl  GETPINTS  Interactive change of integer list, with prompt and limit tests
; desc  in.	String: Description to be used as prompt
; var	both.	Integer array of parameter values. 
;		  If it is undefined upon entry, will create single entry =  1  
; low 	in.	Integer: minimum allowed value
; high	in.	Integer: max allowed value. If same as low, no limits imposed.
;_Desc
; Actions:  
;	N should be an integer within the length of the list
;	I should be a integer valid for its intended function 
;	x may be any integer number, its value is ignored
;  N I  Change value of N'th item to I
;         If N is outside current list, no change is made
; +N I  Insert I value[s] before current item N
;         I may be a set of values separated by commas, no white space!
;         If N is beyond current list, item is appended at the end
; dN x  Delete item N from the list
;         If N is beyond current list, last item is removed
; -1 x  Display current list (to be same as GETP family)
; -2 x  Exit (to be same as GETP family)
;_Lien   Does not handle insertion of more than 1 items at a tims
;_Hist 2005sep16  Hugh Kieffer  adopted from getpset.pro
; 2006jan05 HK Activate limits
; 2008oct19 HK Allow +large index to append
;_End

on_ioerror,bad
siz=size(var)
if siz[0] gt 1 then message,'Invalid 2+ D array' $
else if siz[siz[0]+2] eq 0 then var=1 ; define a set
 
buf=''                          ; define types of input items
c1=['-','+','d','D']            ; valid non-numeric first characters

blan=['',' ','  ','   ','    ','     '] ; 0 to 5 blanks
Print,'GETPINTS: Enter a set of items for: ',desc
print,' N I = replace item N with value I'
print,'+N I[,i etc] = insert one or more items before current N'
print,'dN x = delete item N'
print,'-1 x = print current list'
print,'-2 x = return to caller'
guide:
numv=n_elements(var)             ; length of current list
sind=strtrim(indgen(numv),2)     ; all the locations as string
sval=strtrim(var,2)             ; all the values as strings
for i=0,numv-1 do begin
    j=strlen(sval[i])-strlen(sind[i])
    if j gt 0 then sind[i]=blan[ j]+sind[i] else $ ; prefix blanks
    if j lt 0 then sval[i]=blan[-j]+sval[i]
endfor
print,'Item:',sind
print,'Now =',sval
get: kerr=-1                    ; no error yet
read,buf,prompt='Enter index and value  > '
b2=strtrim(buf,2)               ; remove exterior blanks
b2=strcompress(b2)              ; compress interior blanks to one
bb=str_sep(b2,' ')              ; separate the parts
if n_elements(bb) ne 2 then goto, halt1

ls=bb[0]                        ; decode action
l1=strmid(ls,0,1)               ; first character
kode=where(c1 eq l1)  & kode=kode[0] ; look for valid control character
if kode ge 0 then begin         ; had some control character
    l2=strmid(ls,1)             ; rest of first word 
    if kode eq 0 then begin     ; was a '-'
        if l2 eq '2' then return else goto,guide
    endif else begin            ; rest of first word should be location
        loc=fix(l2)             ; location in list
    endelse
endif else loc=fix(ls)
;*************************************************
; Now, kode=-1 means first word was simple index, do a replace
;            1 means insert
;            2 or more means delete
;     loc is the integer location in the list
;**********************************************
;;help,kode,loc,numv
vs=bb[1]                        ; second word is the value[s]
j=strpos(vs,',')                ; look for commas
if j lt 0 then val=fix(vs) else begin ; none, was a scalar
    vals=str_sep(vs,',')
    val=fix(vals)               ; get integer array
endelse
nv =n_elements(val)


if kode le -1 then begin        ; simple replace
    if loc ge numv then goto, halt2
    if nv gt 1 then print,'Warning: only the first value used.'
    if high gt low then $       ; test value for validity
      if val[0] lt low or val[0] gt high then goto,halt3
    var[loc]=val[0]
endif else if kode eq 1 then begin      ; insert before loc
    if high gt low then $       ; test value for validity
      if val[0] lt low or val[0] gt high then goto,halt3
    loc=loc<numv                ; if large location, append to list
    if loc gt 0 then new=[var[0:loc-1],val] else new=val ; part up to  new
    if loc lt numv then new=[new,var[loc:*]] ; part after new
    var=new
endif else begin                ; delete
    if      loc eq 0       then var=var[1:*] $ ; delete first
    else if loc ge (numv-1) then var=var[0:numv-2] $ ; delete last
    else var=[var[0:loc-1],var[loc+1:*]] ; delete interior
endelse
if kode gt 0 then goto,guide    ; redisplay the list if alignment changed
goto,get                        ; do another change

; ERROR Section
halt3: kerr=kerr+1              ; values out of bounds
halt2: kerr=kerr+1              ; index out of bounds
halt1: kerr=kerr+1              ; expect precisely two words
bad:   kerr=kerr+1
serr=['Read error occured; check and try again' $
   , 'expect precisely two words','index out of bounds','value out of bounds']
if kerr ge 0 then print,'ERROR: ',serr[kerr]
if kerr eq 3 then print,'Valid range is',low,high
goto, guide

end
