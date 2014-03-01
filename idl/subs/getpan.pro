PRO getpan, desc, var,low,high,labs=labin,ido=ido
;_Titl GETPAN  Modify any elements of numeric array, with prompt and limit tests
; desc  in.	Prompt text
; var   both.	Array (N) to modify
; low   in. 	Minimum allowed value. May be scalar or same size as var
; high  in. 	Max allowed value.     Must be same size as low
; 	    If low eq high, then no value restrictions applied.
; 	    If low gt high, then will simply print and return
; labin in_	strarr (N) labels (description) for individual items
;	    OR  Int scalar as flag to allow change of array size
;		In this case, only the first elements of low & high are used
; ido   in_ intarr(<N) of items to not demote. +100=promote. 
;               Values must be < (N-1) [+100]
;_Lien
; No test on values in ido
;_Hist  98dec04 Hugh Kieffer   Derived from getpa.pro
; 99mar02   HHK add labs option
; 2000apr17 HHK allow low & high to be vectors
; 2001jan07 HHK allow changing array size
; 2001mar25 HK revise print format
; 2002mar10 HK make explicit print formats
; 2002mar21 HK put min and max columns to the left of index
; 2003mar30 HK Add prompts when can change list length
; 2004aug17 HK Add print-only mode if low gt high. Add test on size of  var.
;_End

; Internal logic controls:
; kode  0=no labels or extension,  1=use labels, fixed number of items  
;	2= allow add/delete item
; lidx  Lower limit on valid action codes
; lim   Limits: 0=none  1=same for all items   2= individual limits
; info  Basic mode: 0=normal  -1=information only, no changes

n=N_ELEMENTS(var)	; # items in array
if n lt 1 then begin 
    print,' GETPAN: ERROR, var is not defined.  NO ACTION'
    return
end
type=SIZE(var,/type)	; word type
;   -  byte   int       long       float         double
uu=['', ' ',' __','    ___','     ____','  __________'] ; spacing for header
ff=['','I4', 'i6',    'i10',    'g12.5',       'g15.8']
u=uu[type]                  ; header spacing for current type
ON_IOERROR,bad
info=0B                         ; default is to allow changes
lidx=-1                         ; minimum idx with specific action
if not keyword_set(labin) then kode=0 $ ; no labels or extensions
  else if n_elements(labin) eq n and size(labin,/type) eq 7 then kode=1 $ ; labels, fixed dimension
  else begin & kode=2 & lidx=-4 & endelse ; allow change of dimension
labs=labin
if n_elements(ido) gt 0 then begin ; demote all but requested
    jj=where(ido ge 100,i)      ; items for emphasis
    dem='     [ '
    if i gt 0 then begin 
        ii=ido[jj] mod 100 ; items to promote
        labs[ii]='<<< '+labs[ii]
        dem='   '+dem 
    endif
    ii=replicate (1B,n)         ; all flags on
    ii[ido mod 100]=0B          ; turn off flags  of selected items
    ii=where(ii)                ; list of those not in input list
    labs[ii]=dem+labs[ii]
endif

if n_elements(low) eq 1 then begin
    if low lt high then begin   ; single fixed range for all
        print,'Allowed range=',low,high
        lim=1
    endif else lim=0            ; no range constraints
    if low gt high then info=-1B ; no changes at all
endif else lim=2                ; individual ranges
;;; print,'LIM=',lim
if lim eq 2 then idx='(2'+ff[type]+',I4,1' else idx='(I4,1' ; temp. use of idx
fmt=idx+ff[type]+',2x,a)' ; prompt format
idx=1 & test=var[0]             ; set types
if not info then $
  print,'Input item # and its new value [-1 1 for current list, -2 2 for quit].'
if lidx eq -4 then print,' -3 3 to delete last item  -4 4 to append dummy item'
top=u+'min'+u+'max idx'+u+'now' ;; column heading
if kode eq 1 then top=top+'  Description'
;;stop
show:
print,desc
;;svar=string(var)                ; convert to string
;;dvar=delast0(svar)              ; delete trailing 0's past decimal point
case kode of
0: begin                   ; no individual labels
    case lim of
        0: print,' Now=',st0(var)
        1: begin & print,'Limits=',low,high
        print,' Now=',st0(var) & end
        2: begin 
            print,top
            for i=0,n-1 do print, low[i],high[i], i,var[i],format=fmt
            end
    endcase & end
1: begin                    ; individual labels
    case lim of
        0: for i=0,n-1 do print, i,var[i],'  ',labs[i]
        1: begin 
            print,'  Limits=',low,high
            for i=0,n-1 do print, i,var[i],'  ',labs[i]
        end
        2: begin 
            print,top
            for i=0,n-1 do print, low[i],high[i], i,var[i],labs[i],format=fmt
            end
    endcase & end
2: begin          ; variable size, fixed limits 
    if lim gt 0 then print,'Limits=',low[0],high[0]
    print,' -3 3 = delete last    -4 4 = append dummy value'
    print,'Now= ',st0(var)
   end
endcase

if info then return ; in informational mode
 
get:
read,idx,test,prompt='Enter index and new value> '
if idx eq -1 then goto,show	; display current values
if idx eq -2 or idx lt lidx then return ; all done
if idx ge n then begin & print,'invalid index' & goto,get & end
if idx eq -3 then begin ; delete last item
 if n lt 2 then begin & print,'may not decrease size' & goto,get & end
     n=n-1 &  var=var[0:n-1] & goto,get & end
if idx lt -3  then begin ; append an item
    var=[var,high[0]] ; append the high limit, in case input was out of range
    idx=n & n=n+1 & end  
; valid idx as index
case lim of
    0:                          ; do nothing
    1: begin                    ; there is a single valid range
  if (test lt low[0]) or (test gt high[0]) then begin
     print,'Allowed range is',low,high
     goto, get
  endif & end
 2: begin                    ; there is a valid range for each item
     if low[idx] lt high[idx] and $
       ((test lt low[idx]) or (test gt high[idx])) then begin
     print,'Allowed range is',low[idx],high[idx]
     goto, get
  endif & end 
endcase
var[idx]=test			; update value
goto, get			; prompt for another change

bad:
print,'Read error occured; Check and try again'
goto, get
end
