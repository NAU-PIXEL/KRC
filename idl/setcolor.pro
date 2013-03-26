PRO setcolor, init=init 
;_Titl  SETCOLOR  Set or modify colors, lines, plot-symbols, #plots/page
; init	in_key	Controls type of action.  See Guide at 88: near end of code
;_Vars
;common SETCOLOR_COM, nkc,nkl,nkp,kkc,kkl,kkp,bwline, $ ; before 2001mar01
;	 ctab,iklr,fkc,lctab,fixkkc,fixkkl,fixkkp       ; before 2001mar01
common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;        vvWASv before 2001mar01
; kcc[0] lctab	Last color-table loaded	                 Leave alone
; kcc[1] ctab	Color table user wants to use;-3:40      May reset before call
; kcc[2] nkc	# of color levels defined in kkc.     User may reset before call
;   after an 85x call, this will be total number of distinct lines/colors
;   but the number defined will always be at least 14
; kcc[3] nkl	# of line types defined in kkl    Reset only if you redefine kkl
; kcc[4] nkp 	# of symbols defined in kkp.      Reset only if you redefine kkp
; kcc[5] bwline Color for a Black&white line.	         Leave alone
; kcc[6] iklr	Color scheme.   OBSOLETE    User may reset to allowed values
;	          0=all B&W  1=fixed set  2=linear;  add 20 for all solid lines
; kcc[7] fkc	Low percentage of colors not used.   OBSOLETE
; kcc[8] bclr   Color for call to PLOTS for borders and titles
; kcc[9] pclr   !binc index (not value) for white line to monitor. Leave alone
; linecol intarr[1=inactive or 8] Colors for lines over B&W images.
; kkc	intarr	Array of colors.	[ These 3 are refreshed at each call  ]
; kkl	intarr	Array of linesymbols.	[ You may reset values, but not number]
; kkp	intarr	Array of psyms.		[  lasts only until next call  
;--- user generally won't need to look beyond here
; fix-kkc,-kkl,-kkp intarr Preset colors, lines, symbols  

; System Variable  !binc  lonarr  Standard rainbow sequence for current hardware
;_Desc
; User may reset the items in common to get specific color/line/symbol style
; Read the instruction for each item below.
; If using the line-colors, then should not load any other color table
; as that would wipe-out colors assigned to the top 8 levels.
; Default linestyle sequence uses dots last 
; 2009mar13. With 24-bit color hardware, IDL color tables have little use
; because plot,...,color=i does not yield the i'th of 256 colors, 
; but rather treats as 3*8 bits of intensity for RGB.
; As of 2009jul26, Hulk printer gray levels are indgen(256): 0]=black 255=white
;         Monitor white levels:  i* (1+256+256L^2 )
;_Useage hints;
;	For more than  5 or 8 colored lines,   line= kkl[(k/kcc[2]) mod kcc[3]]
;_Calls  COLOR24BIT  GETP  GETPAN  TOOTHB  ST0  
; Liens  binc not implimented for 16-bit color hardware
;_Hist  1999apr18  Hugh Kieffer  99may17, HK revise common
; 1999may24 HK add toothed color bar
; 2001feb27 HK add test to not reset same color table, replace use of !P.color
; 2001mar01 HK Add color table construction, CHANGE COMMON
; 2001oct03 HK remove resetting kcc[1] for refresh, which inhibited color change
; 2002may06 HK Specific values for 5 most distinctive colors
; 2002sep16 HK Add 85x series
; 2002nov03 HK Treat init=850 as init=0  851set to B&W set of linestyles
; 2003mar30 HK Augment 99 help
; 2003aug07 HK Set to return proper 32-bit number for equivalent of
;               8-bit color if system is set to 24-bit color
; 2004sep24 HK Move initialization higher to avoid undefined kcc
; 2004Oct09 HK Add binc into common 
; 2005jun19 HK Use !binc for all colors
; 2005jul30 HK Modify  !binc to track current color table. And, for printer,
;		larger values are increasingly black. Test with XWINCOLORS @61
; 2005oct03 HK Add custom colors, EXTEND COMMON
; 2006apr24 HK Include setting PLOT background and contrast lines = kcc[8]
;            855==852  and   853 not good for B&W
; 2006sep02 HK Replace 857 with set of 6 B&W lines
; 2006oct27 HK Add kon 860 to set color on white
; 2007mar27 HK minor cleanup
; 2007apr30 HK Fix bug, kcc[9] was initiated as !Binc[254]
; 2009mar13 HK Change some fixed values to match new COLOR24BIT -2 table
; 2009jul27 Discussion of goals
; For B&W printers: N (to >6) distinctive lines, gray-scale image
; For color monitor: same as Printer, Plus:  M distinctive colored lines 
;                colored paths acros B&W image
; Routine is getting too complex, >90% of use is for curves. Want some way to 
; handle them to Monitor or printer with no changes outside this routine.
; Plotting routines that want to be compatible with both monitor and printer
; should use the kkl and kkc arrays in common. This routine will always define
; these to be at least  minl  long, though some schemes will involve duplicates.
; Thus, plotting routines will not need to modulo the curve index with nkc or 
; nkl unless that index exceeds 23.
; After any plot to printer, must re-establish color scheme
; 2009nov29 HK Separate black versus white background from line color scheme
;_Lim
minl=24                         ; minimum number of lines always defined
;_END

;;;help ; see who called me 

defsysv,'!binc',exists=i        ; check if !binc  is defined
if i eq 0 then begin            ; no, so define it
    kok=9                       ; dummy for use in next call
;    if !p.color gt 40000 then qq=COLOR24BIT(kok,lc=-2) $ ; Rainbow+white
    device, get_visual_depth=vdp
    if vdp eq 24 then qq=COLOR24BIT(kok,lc=-2) $ ; augmented Rainbow+white
    else qq=lindgen(256)
;;    kok=9                       ; for use in next call to set read_only
;;    defsysv,'!binc',qq,kok; make it permanent for this IDL invocation 
    defsysv,'!binc',qq ; allow later changes
endif

kon=1                           ; set types
if not keyword_set(init) then inj=0 else inj=init
if inj eq 850 then inj=0 ; special to handle simple code in calling programs

if inj eq 1 or n_elements(kcc) lt 9 then begin  ; COLOR initialization.	
    !binc=COLOR24BIT(kkk,lc=-2) ; set colortable to Hugh's expanded rainbow
  ; undocumented IDL: psym -7 to -1  connects symbols with solid line
  ;   linesymbol <0 yields solid, >5 yields long dash.  -1 used by moviespec
    kcc=[-1,39,0,0,0,1,1,20,0,254] ; force color table set
    fixkkc=!binc[[23,61,111,151,190,235,247,255]] ; 8 fairly distinct colors
    kcc[2]=n_elements(fixkkc)   ; preset colors
    fixkkl=[-1,2,3,5,4,1] 	 & kcc[3]=n_elements(fixkkl) ; preset lines
    fixkkp=[6,4,5,1, 7,2,3] 	 & kcc[4]=n_elements(fixkkp) ; preset psymbols
    cblack=0 & cwhite=!binc[255] ; normal monitor values
    !p.multi = [0,0,0,0,0]	; 1/page
    !p.background=cblack        ; black background, white border
    !P.color=cwhite
    kcc[8]=cwhite               ; bclr, borders & titles
    kcc[9]=255 & kcc[5]=cwhite
endif

if inj eq -8 then begin ; line color setup provided by Jim Torson
;-------------------------------------------------------------------
  if !d.table_size LT 32 then begin ; Check if enough color cells available
    print, 'There are not enough colorcells available in the public colormap.'
    print, '  You should exit from other display applications and re-start IDL.'
    return
  endif
; Init the color LUTs for line plotting
;     red green blue                          white black
    r = [ 255,   0,   0, 255,   0, 255, 255, 127] ;255,  0]   
    g = [   0, 255,   0, 255, 255,   0, 127, 255] ;,  0]
    b = [   0,   0, 255,   0, 255, 255, 127, 127] ;,255,  0]
    i=n_elements(r)             ; number of line color defined above
    nfb = !d.table_size - i     ; these will be available for image
    linecol = nfb+indgen(i)     ; color levels for curves over images 
    TVLCT, r,g,b, nfb           ; load at high end
    LOADCT, 0, bottom=0, ncolors=nfb, /silent ; Load gray-scale color table
                                             ;     for valid pixel display
    return
;-------------------------------------------------------------------
endif

cblack=0 & cwhite=!binc[255]    ; normal monitor values
if inj eq -1 then begin         ; deactivate line color
    linecol=0                   ; define as 1 element
    return
endif
i=n_elements(linecol) & if i eq 0 then linecol=0 ; ensure defined
if i le 1 then i=0 ; short linecol is not active 
ncol=!d.table_size -1-i  ; number of available colors levels
labc=['[Last color-table loaded]','color table desired. Reset BEFORE call' $
,'# of colors defined. Reset BEFORE call' $
,'# of line types defined. MUST redefine fixkkl' $
,'# of symbols defined. " " fixkkp','[Black&white line]' $
,'Color scheme, 6 allowed values','low % to not use']
lowc =[-1, 0,  2,1,1,  0, 0, 0]
highc=[40,40,255,6,9,255,22,85]

ctop=!p.color<255 ; top color in 8-bit scheme
;      0  1  2  3  4   5   6   7   8   9  10  11
; kine =[2,13,20,40,61,111,151,190,216,235,247,255] ; 12 distinct colors
if inj gt 849 then begin 
; Set up line color/style scheme
; Basic approach is to define a number of color/style combos
; and recyle thru them if necessary
; If caller needs more than this, could either
; start repeating, or make the lines wider.
  case inj of
    851:  begin  & !p.background=cblack  ; black background, white border
        !P.color=cwhite
        kcc[8]=cwhite       ; bclr, borders & titles
    	kcc[9]=255 & kcc[5]=cwhite  & end   ; lines
    852: begin & !p.background=cwhite; white background, black border and labels
        !P.color=cblack
        kcc[8]=cblack       ; bclr, borders & titles
    	kcc[9]=0 & kcc[5]=cblack & end   ; lines
    853: begin ; printer preview
        j=155                   ; gray level, of 255
        k=long(j)+256L*j+256L^2*j ; 24-bit color word
        g=[cwhite,k]          ; two levels of gray
        i=6                     ; # of different linetypes
        kkc=lonarr(minl)
        kkl=intarr(minl)
        for j=0,minl-1 do begin ; each defined index
            kkc[j]=g[j/6 mod 2] ; grayness
            kkl[j]=fixkkl[j mod 6] ; line type cycles fastest
        endfor & end
    854: kink=[111,151,190,235]; 4 distinct colors
    855: kink=[15,60,105,150,230] ; 5 dark colors
    856: kink=[61,111,151,190,235,247] ; 6 colors
    857: kink=[61,111,151,190,235,247,255] ; 7 colors
    859: kink=[13,61,111,151,190,216,235,247,255,2,20,130,40,1] ;14 mostly bright
    else: Message,' invalid 85x call  x: 1=B+W 2=W+B 3=pre 4,6,7,9= # colors',/cont
  endcase
  if inj ge 854 then begin; fill out the common arrays
    i=n_elements(kink)>1          ; # of different colors
    kkc=lonarr(minl)
    kkl=intarr(minl)
    for j=0,minl-1 do begin          ; each defined index
        kkc[j]=!binc[kink[j mod i]] ; color cycles fastest 
        kkl[j]=fixkkl[(j/i) mod 6] ; line type cycles slower, might repeat
    endfor
  endif
if !dbug then stop
  goto,done
endif
; Normally does nothing above this for init=2

refresh:        ; set color according to device and options. kcc[2] == # colors
kok= !D.name eq 'X'             ; color ok for this device
kkp=fixkkp                      ; insure it is set
if kok then begin  ; to the monitor
    kcc[5]=cblack 
    kcc[8]=!binc[kcc[9]]        ; reset color for calls that plot border
endif else begin  ; to the printer, presume Black on white background
    g=[cblack,128]           ; two levels of gray
    i=6                         ; # of different linetypes
    kkc=lonarr(minl)
    kkl=intarr(minl)
    for j=0,minl-1 do begin     ; each defined index
        kkc[j]=g[j/6 mod 2]  ; grayness
        kkl[j]=fixkkl[j mod 6]  ; line type cycles fastest
    endfor
    kcc[5]=1                    ; PS, 1=black
    kcc[8]=0                    ; ensure color is 0 for calls that plot border
endelse
if inj gt 0 then goto,done; no prompting

ask: ;---------------------------------------- interactive parameter change
READ, kon, prompt='SETCOLOR: 0=return 3=ctab 5=scheme 7=# 88=UseGuide> '
case kon of
 -1: STOP
 0: goto,done                   ; return
1: !p.multi = [0,0,0,0,0]	; 1/page
2: !p.multi = [0,1,2,0,0]	; 2 vertical
3: begin & i=kcc[1] & GETP,'Color table',i,-5,40 & kcc[1]=i & goto,refresh & end
4: !p.multi = [0,2,2,0,0]
6: !p.multi = [0,2,3,0,0]
7: begin & GETPAN,'Constants in Common',kcc,lowc,highc,labs=labc
     goto,refresh & end
9: !p.multi = [0,3,3,0,0]
62: TOOTHB,-2,VV=VV ; modify location/size of toothbar
64: TOOTHB,-4,VV=VV ; plot
66: TOOTHB ; plot default toothed color bar on current window
88: begin
print,'Calling: SETCOLOR,init=  '
print,' -8: construct color table with 8 line colors reserved at top'
print,' -1: set to have no reserved line colors'
print,'  0: interactive prompts'
print,'  1: or /init, reinitializes; and returns without prompting'
print,'		Rainbow_&_white, 6 linestyles, 7 symbols'
print,'  2: refresh (update current plot device and arrays) return with no prompt'
print,' >2: set to this many linear-colored solid lines, no prompt'
print,'  850: same as init=0             851: white line on black'
print,'  852: Black line on white        853: printer preview'
print,'  854:7: 4:7 colors 5=dark        859: 14 colors'
print,' Minimum number of color/line set=',minl,'  modulo for larger'
print,'Code: plot,...  ,color=kkc[j mod kkc[2]],line=kkl[j mod kkc[3]]'
  end
else: begin
print,'-1=stop'
print,'1,2,4,6,9: # plots/page=12469. !p.multi=',!p.multi,format='(a,5i3)'
print,'3: set color table   =',kcc[1]
print,'7: set constants in COMMON: kcc=',ST0(kcc)
print,'66: overplot default toothed color bar  62=modify   64=plot '
print,'88=Usage Guide'
  end
endcase
goto, ask

done:
kcc[2]=n_elements(kkc)          ; always insure sizes are right
kcc[3]=n_elements(kkl)
kcc[4]=n_elements(kkp)
return
end
