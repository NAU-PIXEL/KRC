PRO graph, kodin,hard,psfile=psfile
;_Titl  GRAPH  Interface to graphics devices
; kodin	in. integer control kode:   [ indicates options not used in new code
;     0	= Interactive: reset defaults [and clear any buffer]
;     1 = reset to color monitor
;     7	= close output file, no spawn to printer, set to X-term
;     8	= set to hardcopy and open output file
;		will request setting hardcopy device if it is not defined
;     9	= close output file,    spawn to printer, set to X-term
;  +100 = stop before return
; hard out.  current device:  1,2=x-term, 3,4,5=hardcopy device
; psfile in_ PostScript output file stem;	will append '.eps' if absent;
;	Default is 'idlout' & prompt: <CR> yields no change.
;_Desc
; The colors listed in common PENS are predefined unless color table changed
; If color chosen, loads 16-level color table
; Output device ID:
; 1 = screen, monochrome   ; 2 = X (color X11 server)  ????
; 3 = B&W ps printer   4 = PS (color postscript)  5 = HP (HPGL plotter)	
; NORMAL USAGE:
; first call with kodin = -1 to initialize and set to plot on screen
; when ready to plot to hardcopy, call with kodin = 8 
; after plot calls are done, call with kodin=7 or 9 to send plot to printer
;_Call  SETCOLOR  always calls before returning
;_var
common PENS, black,white,blue,grn,red,ltblu,mag,grey ; available to users
; positions:   1     2    3    4   5    6    7    8
;_Hist  99jun11 Hugh Kieffer complete rewrite, largely keeping default actions
; 2001apr20 HHK remove prompt for file name, rather use 98: option
; 2002apr05 HK auto-init to landscape color terminal, Delete many call options
; 2002aug25 HK Add backing-store option to use windows system 
;_End

common GRAPH_SAVE, lout,outfile,orient,devs,devh	; local memory
; lout = current output device, 1:5 are valid
; outfile = output file name
; orient = orientation of hardcopy plot, 1 or 21 -> landscape, else portrait
; devs = defined screen conditions, 0=undefined
; devh = defined hardcopy output device, 0=undefined

kode=kodin mod 100
lout=0                          ; no defined kode step done yet

if n_elements(orient) lt 1 or kode eq 1 then begin ; define or redefine  variables
  devs=2 & devh=3 & orient=21 & lout=devs & outfile='d' & end

if kode eq 0 then begin 	; revise defaults, and set to soft
	READ,devs,prompt='Monitor default: 2=color  else=B&W > '
        devs=(devs > 1) < 2
	READ,devh,prompt='Hardcopy default: 4=color else=B&W > '
        devh=(devh > 3) < 4
	READ,orient,prompt='Orientation?: 0=ask 20=portrait 21=landscape > '
        lout=devs               ; set to monitor
endif

if kode eq 8 then begin		; start new output file
        if (orient lt 20) then $
          READ,orient,prompt='Orientation?: 0=portrait 1=landscape +20 to set> '
        if keyword_set (psfile) then outfile=psfile else outfile='idlout.eps'
        lout=devh               ; set to hardcopy
endif

if kode eq 7 or kode eq 9  then begin    ;------------ close plot file
  if !d.name ne 'X' then DEVICE,/close_file ; may not close X device
  if kode eq 9 then begin ; print the file
    cpu=getenv('HOST')        ; get current cpu
    if cpu eq 'hulk' then lpp=['lpr ','lpr -Phpdj660c','lpxxx ']  $ ;B&W, color, Pen printers
                     else lpp=['lpr ','lpr -P1tek860 ','lpr -Phpgl ']
    case devh of 
	3 : SPAWN, lpp[0]+outfile ; 'lpr -P1hp8 '+outfile    ;
	4 : SPAWN, lpp[1]+outfile
	5 : SPAWN, lpp[2]+outfile
	else: Message,'Unexpected devh at file close ='+string(devh)
    endcase
  endif
  if keyword_set (psfile) then begin ; change the closed file name
      outfile=psfile 
      i=STRPOS(outfile,'.ps')  & j=STRPOS(outfile,'.eps') ; look of ps extension
      if (i lt 0 and j lt 0) then outfile=outfile+'.eps' ; if none, add one
      print,'Changeing plotfile name to: ',outfile
      SPAWN, 'mv idlout.eps '+outfile
  endif
  lout=devs ; set to soft
endif		; --------------------------------------------------


  case lout of
    1: begin
	SET_PLOT,'X'
        DEVICE,retain=1
	ctab=0 ; loadct,0   ; load color table  B-W LINEAR
	black=0 &  blue=255 & grn=255 &  red=255 & white=255 
	red=255 & ltblu=255 & mag=255 & grey=255
	end
    2: begin ; output to X
	SET_PLOT,'X'
        DEVICE,retain=1
	ctab=39 ; loadct,39   ; load color table  Rainbow + white
	black=0 & grn=45 & ltblu=80 & blue=95 & mag=125 
	red=180 & grey=210 & white=240
	!p.background = black
	end
    3: begin	; output to  B&W  PS
	SET_PLOT,'PS'
	if orient mod 20 eq 1	then DEVICE,file=outfile,/landscape $
				else DEVICE,file=outfile,/portrait
	end
   4:  begin ; output to color PS
	SET_PLOT,'PS'
       if orient mod 20 eq 1 then DEVICE,file=outfile,/color,bits=8,/landscape $
			     else DEVICE,file=outfile,/color,bits=8
	ctab=16 ; loadct,16 
	black=0 & grn=45 & ltblu=80 & blue=95 & mag=125
	red=180 & grey=210 & white=black
	!p.background = 255        ; set background color to white on erase
	end
   5:  begin ; output to HP pen plotter
	SET_PLOT,'HP'
	DEVICE, file=outfile,/landscape,/inches,xsize=30,ysize=20
	black=1 & blue=2 & grn=3 & red=4 & heavy=5
	end
   else: begin 
       Message,'Undefined lout or kode ='+string(lout,kode),/con
       stop
   end
  endcase

hard=lout
SETCOLOR,init=2		; refresh, because device may have changed

if kodin ge 100 then stop
return
end
