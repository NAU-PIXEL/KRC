PRO krccomlab ,kode,fcom,icom,lcom, fclab,iclab,lclab
;_Titl KRCCOMLAB Print KRC common input items
; kode	in.	Integer. Print control code. +1=floats +2=integers +4=logicals
; fcom	in.	KRCCOM floating values
; icom	in.	KRCCOM integer  values
; lcom	in.	KRCCOM logical  values
; fclab	in.	Short labels for the float   items in KRCCOM
; iclab	in.	Short labels for the integer items in KRCCOM
; lclab	in.	Short labels for the logical items in KRCCOM
;_Desc
; Uses the _lab arrays for lenght, so the _com arrays must be at least this long
;_Calls None
;_Hist 2002mar02 Hugh Kieffer Mostly extracted from READKRC
; 2002mar21 HK Make _Lab input, and immmue to undefined arrays
; 2004jul24 HK. Correct error where in   n  was one too small
;_End

ko1=kode mod 2 eq 1             ; 1 was added
ko2=ishft(kode,-1) mod 2 eq 1   ; 2 was added
ko4=ishft(kode,-2) mod 2 eq 1   ; 4 was added

if ko1 then begin 
    n=n_elements(fclab)
    if n lt 1 or n_elements(fcom) lt n then  $
      Print,'KRCCOMLAB: Some Float undefined' $
    else begin
        last=n-1
        for i=0,last,8 do begin
            i2=(i+7) < last
            print,fclab[i:i2],format='(8A10)'
            print,fcom[i:i2],format='(8f10.2)'
        endfor
    endelse
endif

if ko2 then begin
    n=n_elements(iclab)
    if n lt 1 or n_elements(icom) lt n then  $
      Print,'KRCCOMLAB: Some Int. undefined' $
    else begin
        last=n-1
        for i=0,last,8 do begin
            i2=(i+7) < last
            print,iclab[i:i2],format='(8A10)'
            print,icom[i:i2],format='(8I10)'
        endfor
    endelse
endif

if ko4 then begin 
    n=n_elements(lclab)
    if n lt 1 or n_elements(lcom) lt n then  $
      Print,'KRCCOMLAB: Some Logical undefined' $
    else begin
        for i=0,last,10 do begin
            i2=(i+9) < last
            print,Lclab[i:i2],format='(10A7)'
            print,Lcom[i:i2],format='(10I7)'
        endfor
    endelse
endif

return
end
