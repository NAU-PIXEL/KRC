PRO graph, kodin,hard, psfile=psfile,port=port
;_Titl  GRAPH  Interface to graphics devices
; kodin	in. integer control kode:
;     80    = restart output file empty
;     8,81  = set to B&W/color hardcopy and open output file
;     87,9  = close output file, set to X-term   9= spawn to printer
;  +100 = stop before return
; hard in_ TEMPORARY for backward compatibility
; psfile in_ PostScript output file stem;	will append '.eps' if absent;
;	Default is 'idlout' & prompt: <CR> yields no change.
; port in_ Int     1=portrait  0=landscape(default); -= will ask 
;_Desc
; Required capabilitities:
; - Direct following plot output to a file: B&W (default) or color
;     allow setting to landscape  or portrait. (default)
;   If destined for B&W, then call setcolor to set all lines black
;   and reset with the file closed.
; - Direct following plot to color monitor; Yes/no plot the prior file
; 8 Direct following plot output to a file
; NORMAL USAGE:
; first call with kodin = -1 to initialize and set to plot on screen
; when ready to plot to hardcopy, call with kodin = 8 
; after plot calls are done, call with kodin=7 or 9 to send plot to printer
;_Call  SETCOLOR

;_Hist  99jun11 Hugh Kieffer complete rewrite, largely keeping default actions
; 2001apr20 HHK remove prompt for file name, rather use 98: option
; 2002apr05 HK auto-init to landscape color terminal, Delete many call options
; 2002aug25 HK Add backing-store option to use windows system 
; 2010oct25 Hugh Kieffer Major rewrite to be compatible with only:
;   color monitor, B&W printer, color printer (incomplete)
;   Delete 2nd argument 'hard' 
;   Remove all setting of colors from here.
;_End

common GRAPH_SAVE, lout,outfile,orient,bwf,devh	; local memory
; lout = flag: output file open
; outfile = output file name
; NOPE orient = orientation of hardcopy plot, 1 or 21 -> landscape, else portrait
; bwf  Flag; 1=B&W output device
; NOPE devh = defined hardcopy output device, 0=undefined
if n_elements(bwf) lt 1 then begin 
    bwf=0                       ; ensure defined
    lout=0
endif

kode=kodin mod 100

;if kode eq 8 then begin ; restart the output file


if kode eq 8 or kode eq 80 or kode eq 81 then begin ; send to print file
    if keyword_set (psfile) then outfile=psfile else outfile='idlout.eps'
    if not keyword_set(port) then port=0
    if port lt 0 then begin   ; set orientation
        read,port,prompt='orientation?: 1=portrait else=landscape > '
        if port ne 0 then port=1
    endif
    if kode eq 81 then begin    ; go to color printer
        message,'incomplete'
        devh=getenv('MYCLR')
        bwf=0                   ; unset B&W flag
        set_plot,'PS'
        if port then device,file=outfile,/color,bits=8 $
                else device,file=outfile,/color,bits=8,/landscape
    endif else begin            ; go to B&W printer
        devh=getenv('MYBW')     ; get printer name
        SETCOLOR,init=857       ; set to all black
        bwf=1                   ; set B&W flag
        set_plot,'PS'
	if port then device,file=outfile,/portrait $
	        else device,file=outfile,/landscape
    endelse
    lout=1                      ; output file is started

endif else begin ; redirect to monitor

;    if lout gt 0 then begin     ; close output file
    if !d.name ne 'X' then begin
        DEVICE,/close_file      ; may not close X device
        if kode eq 9 then begin ; Option to print it
            SPAWN,'lpr -P'+devh+' '+outfile ; print the file
            print,'Now: mv '+outfile
        endif
    endif
    SET_PLOT,'X'
    DEVICE,retain=1
    if bwf then SETCOLOR,init=860 ; reset to prior color scheme
    bwf=0
    lout=0
endelse

if kodin ge 100 then stop
return
end
