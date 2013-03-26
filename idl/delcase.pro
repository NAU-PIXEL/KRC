function delcase, ttt,jk=jk,ft=ft,pa=pa, hh=hh
;_Titl  DELCASE show delta between arrays changing only last index
; ttt  in.  Array of dimensionality 2+, last index must be at least 2 for diff.
; jk   in_  intarr(2) or int. Indices to differ or get , Default is [0,1]
;            If scalar, gets only that case. Default is difference between 2
; next 3 not used if jk is scalar.
; ft   in_  float. Fraction off-plot that triggers replot, Default=.001
; pa   in_  float  Pause time between plots, seconds. Default =-1=wait 
;             -2 will skip plots
; hh   out_ fltarr(4) Statistics on differences: mean,StdDev, data_Min,_Max
; func out. Delta of array input or single case, without last dimension
;               Negative integer if formal error.
;_Hist 2009mar06 Hugh Kieffer  For testing KRC bin5 file changes
;_End

do1= n_elements(jk) eq 1 ; return one case
if not keyword_set(pa) or !dbug then pa=-1.

siz=size(ttt) & nd=siz[0]       ; dimensionality
if nd lt 2 then return,-1
nl=siz[nd]                      ; size of last dimension
if n_elements(jk) eq 0 then jk=[0,1]
j=jk[0]                         ; base index
if j lt 0 or j ge nl then return,-3
case nd of
    2: yy=ttt[*,j]
    3: yy=ttt[*,*,j]
    4: yy=ttt[*,*,*,j]
    5: yy=ttt[*,*,*,*,j]
    6: yy=ttt[*,*,*,*,*,j]
    else: message,'Not coded for nl>6'
endcase

if not do1 then begin  ;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    if nl lt 2 then return,-2
    if not keyword_set(ft) then ft=.001
    k=jk[1]
    if k lt 0 or k ge nl then return,-4
    if j eq k then return,-5
    case nd of
        2: yy=ttt[*,k]-yy
        3: yy=ttt[*,*,k]-yy
        4: yy=ttt[*,*,*,k]-yy
        5: yy=ttt[*,*,*,*,k]-yy
        6: yy=ttt[*,*,*,*,*,k]-yy
    endcase
    help,ttt,yy
    if pa eq -2 then nop=2 else nop=0 ; no plot or print
    HISTFAST,yy,i1=i1,r1=r1,noplot=nop ; h1=h1
    fout=(i1[1]+i1[2])/float(i1[0]) ; fraction outside 4 sigma plot
    if fout gt ft then begin 
        ii=where(abs(yy-r1[3]) lt 4.*r1[4]) ; within 4 sigma
        print,'will do Hist without outliers; Fraction=',fout
        if not nop then PAUSE,pa
        HISTFAST,yy[ii],i1=i2,r1=r2,noplot=nop
        print,'              Mean        sigma          min          max'
;          pass1  -0.00111441    0.0702158     -2.10176      1.85162
        print,'pass1', r1[3:6]
        print,'pass2', r2[3:6]
    endif
    hh=r1[3:*]
    if not !dbug and not nop then PAUSE,pa
endif ;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

if !dbug then stop
return,yy

end
