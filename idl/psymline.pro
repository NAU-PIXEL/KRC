function psymline, n,line,noneg=noneg
;_Titl  PSYMLINE  Hughs convention for transfering PSYM and LINESTYLE in one
; n	in.	integer scaler for Callers wishes for a plot
;                -inf:-9 are no plot [line=-1 psym=0]
;		  -1:-8  are IDL -PSYM: symbol +n and solid line
;		   0:5   are IDL LINESTYLE [6:9 -->5]
;		  11:18  are PSYM n-10; eg, 14=diamond.
;             21:68= nm  are line n and symbol m  n=6 is dotted line
; line	out.	Linestyle, 0:5 ; -1 means none.
; noneg  in_	If set, negative input will result in positive symbol and line=0
; func	out.	PSYM, -/+ 1:8  ; 0 means none  - will include solid line
;_Desc  IDL conventions:
; Line: <1=solid   1=dot   2=dash   3=dash dot   4=dash 3dot   5=long dash
; Psym:   1=+   2=*   3=.  4=Diamond   5=Triangle  6=Square   7=X  
;     8=User defined, assumed to exist. (HK does so in init.pro)
; Routine designed to work with  HOPLOT
;_Hist Hugh Kieffer 2001jan15 Original version
; 2006mar04 HK Add  noneg  keyword
; 2006apr24 HK Alter so <-8 yields none and  +nm yields line n and symbol m  
;_End

j=(n > (-9)) < 68 ; bring into valid range

if j lt -8 then begin            ; no plot
    line=-1
    out=0                       ; psym
endif else if j lt 0 then begin ; same as -negative PSYM
    if keyword_set(noneg) then begin
        line=0                  ; solid line
        out=-j                  ; positive PSYM
    endif else begin
        line=-1                 ; no line keyword
        out=j                   ; -PSYM will include connecting solid line
    endelse
endif else if j lt 10 then begin
    line=j < 5                  ; LINESTYLE
    out=0                       ; no symbol
endif else if j lt 20 then begin
    line=-1                     ; no line
    out=(j-10)<8                ; PSYM
endif else begin
    line=j/10
    out=(j-10*line)<8          ; PSYM
    if line eq 6 then line=1    ; dotted
    line =line < 5              ; ensure valid
endelse

return,out
end
