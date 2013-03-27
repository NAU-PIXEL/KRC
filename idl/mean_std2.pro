function mean_std2, xin,one=one, wei=wei,STD=std
;_Titl  MEAN_STD2  Mean and standard deviation of 2-D array
; xin	in.	Array; statistics across 2nd element
; one	in_	   if set, do statistics along first dimension
;			default is along second dimension
; wei	in_	Weight for each point, Default= uniform
; 	 if present, must be same size as xin OR 1-D of the length of a vector.
; std	in or out_  Out=Standard deviation vector; =-1. if formal error
;	If ,/std on input, then the output function includes mean and StdDev
; func. out.	Vector of means; scalar <0 if formal error
; 	   If ,/std set on input, the fltarr[*,2] where  0]=mean & 1]=StdDev
;_Calls  MEAN_STD to do the statistics
;_Hist 99feb01 Hugh Kieffer, designed for spectra. 99feb09 HHK add /one option
; and allow larger number of dimensions if they have size of 1
; this routine robust against vectors and array sizes of 1
; 2000feb13 HHK renamed older version MEAN_STD3, which can still handle some
;  3 and 1 dimensional arrays; but makes a copy of the input array.
; MEAN_STD2 presumes input array is 2-D, and now accepts weights.
; 2001jan05  HHK   will treat NAN's as missing data by calls to MEAN_STD
; 2001feb05 HHK add option to output arr[*,2]
;_End

ssx=size(xin) 			; get array size
if ssx[0] ne 2 then message,'Requires 2-D array [see MEAN_STD3]' ; formal error

if keyword_set(one) then begin
    kone=1B                     ; treat 2-nd dimension as vectors of data
    k=ssx[2]                    ; number of vectors to process
    l=ssx[1]                    ; length of each vector
endif else begin
    kone=0B
    k=ssx[1]  & l=ssx[2]   
endelse

if ssx[3] le 4 then mean=fltarr(k) else mean=dblarr(k); array to hold results

both=0B ; if true, then put both mean and std into output function
if keyword_set(std) or arg_present(std) then begin ; must do Std Dev
    lstd=-1B                    ; set true
    sdev=fltarr(k)              ; to hold Standard Deviation
    if not arg_present(std) then both=1B
endif else lstd=0B              ; set false

ww=0                            ; default in case wei is not set
eachw = 0                  ; do not look for weights separately for each vector
if keyword_set (wei) then begin
    ssw=size(wei)
    if ssw[0] eq 2 then begin   ; need 2-D weight vector same size as data
        if (ssw[1] ne ssx[1] or ssw[2] ne ssx[2])  $
          then message,'Size of wei does not agree'
        eachw=-1                ; turn on each weight is separate
    endif else begin            ; need 1-D weight vector same length as data
        if ssw[1] ne l then message,'Size of wei does not agree'
        ww=wei                  ; weight vector to use for all data vectors
    endelse
endif

for i=0L,k-1 do begin
    if kone then xx=xin[*,i] else xx=reform(xin[i,*]); vector to process
    if eachw then begin
        if kone then ww=wei[*,i] else ww=reform(wei[i,*])
    endif
    if lstd then begin
        mean[i]=MEAN_STD(xx,wei=ww,std=st, /nan) ; look for NAN's
        sdev[i]=st               ; transfer standard deviation
    endif else mean[i]=MEAN_STD(xx,wei=ww, /nan)
endfor
if lstd and not both then std=sdev; transfer Standard Deviation
if both then return,reform([mean,sdev],k,2) else return,mean

end
