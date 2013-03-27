function delcase, aaa,jk=jk, id=id, dimn=dimn,ita=ita, pa=pa,ft=ft, hh=hh
;_Titl  DELCASE  Show delta between arrays changing only last index
; aaa  in.  Array of dimensionality 2+, last index must be at least 2 for diff.
; jk   in_  intarr(2) or int. Indices to differ or get , Default is [0,1]
;             If scalar, gets only that case. Default is difference between two
; id   in_ String Name of array, Default= 'DELCASE'
; dimn in_  Strarr(n) Names of each dimension  | if both present, will try
; ita  in_  Strarr( ) Names of items           | to do statistics for each item
; --- Next 3 not used if jk is scalar.
; pa   in_  float  Statistics and plot control. Default = -3 
;             le-3=no stats   -2=stats,no plots    ge-1=Pause for stats and plots
; ft   in_  float. Fraction off-plot that triggers replot, Default=.001
; hh   out_ fltarr(4) Statistics on differences: mean,StdDev, data_Min,_Max
; func out. Delta of array input or single case, without last dimension
;               Negative integer if formal error.
;_Calls  HISTFAST  HSTATS
;_Hist 2009mar06 Hugh Kieffer  For testing KRC bin5 file changes
;_End

if n_elements(jk) eq 0 then jk=[0,1] ; Default cases to use
do1= n_elements(jk) eq 1 ; return one case
if n_elements(id) lt 1 then id='DELCASE'
if not keyword_set(pa) then pa=-3.
if !dbug then pa=-1.
siz=size(aaa) & nd=siz[0]       ; dimensionality
if nd lt 2 then return,-1       ; require min. of 2 dimensions

nl=siz[nd]                      ; size of last dimension
j=jk[0]                         ; base index
if j lt 0 or j ge nl then return,-3 ; formal error, refered to non-existent case
case nd of                      ; extract first case
    2: yy=aaa[*,j]
    3: yy=aaa[*,*,j]
    4: yy=aaa[*,*,*,j]
    5: yy=aaa[*,*,*,*,j]
    6: yy=aaa[*,*,*,*,*,j]
    else: message,'Not coded for nl>6'
endcase

if not do1 then begin  ;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    if nl lt 2 then return,-2   ; require at least 2 cases
    if not keyword_set(ft) then ft=.001 ; set test level
    k=jk[1]                     ; second case
    if k lt 0 or k ge nl then return,-4  ; refered to non-existent case
    if j eq k then return,-5    ; both case indices the same
    case nd of                  ; difference of second from first case
        2: yy=aaa[*,k]-yy
        3: yy=aaa[*,*,k]-yy
        4: yy=aaa[*,*,*,k]-yy
        5: yy=aaa[*,*,*,*,k]-yy
        6: yy=aaa[*,*,*,*,*,k]-yy
    endcase
    ny=float(n_elements(yy))
    if pa gt -3 then begin
      print,id,'  Indices=',jk
      help,aaa
      print,'               Mean       sigma         min         max'
      fmt='(a8,1x,4g12.5)'
      if pa gt -2 then begin    ; some plots, use HISTFAST for stats
        HISTFAST,yy,i1=i1,r1=r1 ; h1=h1
        fout=(i1[1]+i1[2])/ny ; fraction outside 4 sigma plot
        if fout gt ft then begin 
            ii=where(abs(yy-r1[3]) lt 4.*r1[4]) ; within 4 sigma
            print,'will do Hist without outliers; Fraction=',fout
            PAUSE,pa
            HISTFAST,yy[ii],i1=i2,r1=r2
;          pass1  -0.00111441    0.0702158     -2.10176      1.85162
            print,'pass1', r1[3:6],form=fmt
            print,'pass2', r2[3:6],form=fmt 
            if not !dbug then PAUSE,pa
        endif
        hh=r1[3:*]
      endif else begin ; stats only
        hh=HSTATS(yy,['M','S','I','X'])
        print,'pass1=', hh,form=fmt 
        ii=where(abs(yy-hh[0]) lt 4.*hh[1],nout)
        if nout gt ft*ny then begin 
            r2=HSTATS(yy[ii],['M','S','I','X'])
            print,'pass2=', r2,form=fmt 
        endif
      endelse
  endif

; do stats for each item, item dimension may be anyplace, so treat worst case,
; when it is in the middle. Note last input dimension is goe from yy
if n_elements(dimn) eq nd then begin ; number of dimensions and ID's match
    ji=where(dimn eq 'item') & ji=ji[0]+1 ; look for items dimension
    if ji ge 1 and ji lt nd and n_elements(ita) eq siz[ji] then begin ; found items
        ni=siz[ji]              ; number of items
        k1=1L & k3=1L           ; artifical first and third dimension
        if ji gt 1    then for i=1,ji-1    do k1=k1*siz[i]
        if ji lt nd-1 then for i=ji+1,nd-1 do k3=k3*siz[i]  
        qq=reform(yy,k1,ni,k3)
     for i=0,siz[ji]-1 do print,ita[i],HSTATS(qq[*,i,*],['M','S','I','X']),form=fmt 
    endif
endif
 
endif ;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

if !dbug then stop
return,yy

end
