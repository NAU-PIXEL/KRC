function xyy5,  action, arg2, arg3, arg4, arg5,_extra=xtr
;_Titl XYY5 Interface to concatonated [spectral] arrays of different length
; Possible actions: (first letter)                2     3        4      5
; load common with full set: code=xyy5 ('Load',[names , ints,  lastt, array])
; add an XY set to common:    num=xyy5 ('Add',  name,  int,    xyy        )
; get one XY set:           array=xyy5 ('Get',  name,  int,    item_num    )
; get all XY sets:          array=xyy5 ('Copy', names, ints,   lastt      )
; plot all XY sets          dummy=xyy5 ('Plot', Guide4,AxTit3, XYran4,[psym],[_extra=] )
; write a file               code=xyy5 ('Write',name, [titles],[text]      )
; read a file:                num=xyy5 ('Read', name,  titles              )
; an 'item' is a set of X and 1 or more Y values, along with its name.
; This routine designed to handle several spectra of different lengths 
; (and their uncertainties)  in a single file
; More details:
; load: if only one argument, will empty common in prep. for new set of items 
;          else, arg2 thru 5 correspond to common and must be of the same length
;       func.= Success(0) or error code  
; add: arg2= In. string name for this item
;      arg3= In. any integer to be associated with this item 
;      arg4= In. Fltarr[n, 2+] values for this item. All later  MUST have same
;            second dimension as the first item.
;       func.= 0-based location in the commmon arrays. Negative is an error code
; get:  arg2= Out string name for this item
;       arg3= Out. integer value associated with this item
;       arg4= In. 0-based item number to be retrieved from common
;       func.= Fltarr[n, 2+] values for this item
; copy: arg2= Out. Strarr of names for all items
;       arg3= Out. Intarr of values associated with all items
;       arg4= Out. Intarr of the last location for each item
;       func.= Fltarr[n, 2+] values for all items
; plot: arg2= fltarr(4) locations for CURVEGUIDE (<4) yields default there.
;       arg3= strarr(3) Titles for X-axis, Y-axis and top, (<3) yields default
;       arg4=fltarr(4) plot [Xmin,Xmax,Ymin,Ymax] (<4) yields data range
;       arg5: optional. Symbol for each datapoint, neg = overplot symb only 
; write: arg2= In. full-path file name to be used
;       arg3= In. Strarr(m) of description of 2nd dimension of array
;                 Default is X,Y,(m-2)*null,
;       arg4= In. Optional. Text to be written into file header.
;       func.= Success(0) or error code
; read: arg2= In. full-path file name to be used
;       arg3= Out. Strarr(m) of description of 2nd dimension of array
;       func.= Success(0) or error code

; read:
common xyy5_com, xyy5_name,xyy5_int,xyy5_lastt,xyy5_arr
; xyy5_name  strarr  Name or ID for each item
; xyy5_int   intarr  Any integer to be associated with an item
; xyy5_last  intarr  Last location for each item in the main array
; xyy5_arr   fltarr(*,2+)    0]= independant values
;                           1+]= one or more dependant values
common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_Lims  Can only handle one file at a time because of use of Common
;_Line  Plot assumes there is only one Y variable
;_Calls BIN5R STRUM
;_Hist  2003nov15 Hugh Kieffer  Adopted from LOADBANDWT 
; 2010mar03 HK Add Plot action, respond to !dbug 
;    BEWARE may have changed some arg4,arg5 meanings from older
;_End

kode=strupcase(strmid(action,0,1)) ; first letter as uppercase
func=0                          ; set to no errors
npar=n_params()                 ; number of parameters in the call
case kode of
'L': if npar lt 5 then xyy5_lastt=-1 $ ; flag common as empty
     else begin                 ; move arguments into common
    siz=size(arg5)             ; check that all arguments have proper dimensions
    if siz[0] ne 2 then goto,err1 ; wrong number of dimensions
    if siz[2] lt 2 then goto,err1
    type=siz[siz[0]+1]          ; type of this array
    if type lt 1 or type gt 5 then goto,err1 ; not numeric
    siz=size(arg2)              ; check names
    n5=siz[1]                   ; number of items
    if siz[0] ne 1 or siz[2] ne 7 then goto,err1 
    siz=size(arg3)              ; check associated integers
    if siz[0] ne 1 or siz[1] ne n5 or siz[2] gt 3  then goto,err1 
    siz=size(arg4)              ; check last position
    if siz[0] ne 1 or siz[1] ne n5 or siz[2] lt 2  $
      or siz[2] gt 3 then goto,err1 
    xyy5_name=arg2
    xyy5_int=arg3
    xyy5_lastt=arg4
    xyy5_arr=arg5
 end
'A': begin ; check that arguments are proper size and type
       siz=size(arg2)           ; check name
       if siz[0] ne 0 or siz[1] ne 7 then goto,err1 ; must be single string
       siz=size(arg3)           ; check associated integer;  must be 
       if siz[0] ne 0 or siz[1] lt 1 or siz[1] gt 3 then goto,err1 ; single int.
       siz=size(arg4)           ; check array
       if siz[0] ne 2 then goto,err1 ; wrong number of dimensions
       if siz[2] lt 2 or siz[2] gt 3 then goto,err1 ; 2nd must be 2 or 3
       type=siz[siz[0]+1]       ; type of this array
       if type lt 1 or type gt 5 then goto,err1 ; must be numeric
       n5=siz[1]                ; number of points in this item
       if xyy5_lastt[0] lt 0 then begin ; this is first item
           xyy5_name=arg2
           xyy5_int=fix(arg3)
           xyy5_lastt=n5-1      ; will be type long
           xyy5_arr=float(arg4) ; create  array
       endif else begin         ;append new item
           sizc=size(xyy5_arr)   ; check number of variables in common array
           if siz[2] ne sizc[2] then goto,err3 ; wrong dimension
           xyy5_name=[xyy5_name,arg2] ; append name
           xyy5_int=[xyy5_int,fix(arg3)] ; append number
           j=n_elements(xyy5_lastt) ; number of items already in common
           xyy5_lastt=[xyy5_lastt,xyy5_lastt[j-1]+n5] ; append last position
           xyy5_arr=[xyy5_arr,float(arg4)] ; append array
           func=j
       endelse & end
'G': begin ; retrieve one item
    n5 = n_elements(xyy5_lastt)
    j=fix(arg4)
    if j lt 0 or j ge n5 then goto,err4
    arg2=xyy5_name[j]
    arg3=xyy5_int[j]
    if j eq 0 then i1=0 else i1=xyy5_lastt[j-1]+1
    i2=xyy5_lastt[j]
    func=xyy5_arr[i1:i2,*]
    end
'C': begin ; retrieve all item
    arg2=xyy5_name
    arg3=xyy5_int
    arg4=xyy5_lastt
    func=xyy5_arr
end

'P': begin ; Plot all curves
sizc=size(xyy5_arr) 
n5 = n_elements(xyy5_lastt)
xx=xyy5_arr[*,0]
yy=xyy5_arr[*,1]
if n_elements(arg2) ge 4 then locc=arg2 else locc=0
if n_elements(arg3) ge 3 then labs=arg3 else labs=['Wave','Ref','XYY5 plot']
if n_elements(arg4) ge 4 then begin
    xran=arg4[0:1]
    yran=arg4[2:3]
endif else begin 
    xa=min(xx,max=xb)
    ya=min(yy,max=yb)
    xran=[xa,xb]
    yran=[ya,yb]
endelse
if npar ge 5 then psym=arg5 else psym=0
dop=psym ne 0                   ; overplot symbols
dol=psym ge 0                   ; plot lines
if dol then plot,xx,yy,/nodata,xran=xran,yran=yran $
  ,xtit=labs[0],ytit=labs[1],title=labs[2],_extra=xtr
psym=abs(psym)
i1=0
for j=0,n5-1 do begin 
    i2=xyy5_lastt[j]
    clr=kkc[j mod kcc[2]]
    lin=kkl[j mod kcc[3]]
    if dol then oplot,xx[i1:i2],yy[i1:i2],line=lin,color=clr
    if dop then oplot,xx[i1:i2],yy[i1:i2],psym=psym,color=clr
    if dol then CURVEGUIDE,j,xyy5_name[j],lin,locc=locc,color=clr
    i1=i2+1
endfor
end

'W': begin ; write common with to a bin5 file
    sizc=size(xyy5_arr) & mdim=sizc[2]
head='File written by XYY5 '
if npar ge 4 then head=head+arg4
if npar lt 3 then arg3=['X','Y',strarr(mdim-2)]
head=head+' Array Variables=='+STRUM(ST0(arg3,      /nojoin),'^',/join)
head=head+' Item Names=='     +STRUM(ST0(xyy5_name, /nojoin),'!',/join)
head=head+' Assoc. Integers=='+STRUM(ST0(xyy5_int,  /nojoin),'\',/join)
head=head+' Last index=='     +STRUM(ST0(xyy5_lastt,/nojoin),'|',/join)
BIN5,'W',arg2,head,xyy5_arr,/verb
     end
'R': begin ; load common from bin5 file
    BIN5,'R',arg2,head,xyy5_arr,/verb
    siz=size(xyy5_arr)
    if siz[0] ne 2 then goto,err2 ; open failed
    xyy5_name=STRUM(head,'!')   ; extract vector of names from header
            q=STRUM(head,'|')   ; extract vector of last indices from header
    xyy5_lastt=fix(q)           ; convert to integer
            q=STRUM(head,'\')   ; extract vector of associated integers
    xyy5_int=fix(q)             ; convert to 16-bit integer
    arg3=     STRUM(head,'^')   ; extract variable titles
     end
else: Message,'Called with invalid action= '+action
endcase

done: ; stop
if !dbug then STOP
return,func

err1: Begin
print, 'Some dimension or type is invalid'
help,arg2,arg3,arg4,arg5
func=-1 & goto,done & end

err2: Begin
print, 'File open failure'
func=-1 & goto,done & end

err3: Begin
print, 'array dimensions do not agree with current common'
help,arg4
func=-1 & goto,done & end

err4: Begin & print,'Itme number out of bounds:  ',+string(arg4)
func=-1 & goto,done & end


end
