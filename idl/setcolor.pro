PRO setcolor, init=init 
;_Titl  SETCOLOR  Set or modify colors, lines, plot-symbols, #plots/page
; init	in_key	Controls type of action.  See Guide at 88: near end of code
;_Use 851=black background 852=white background 853=printer preview 
; 854,6,8,9 =4:14 colors 857=all black for printer.  855=use kink in common
; 860=recover colors saved @857
; 861: 14 colors in order, 862=17 (~max that can be discriminated on black)
; 866,9: Black+5,8 colors good on white
; 880/1=DECOMPOSED=0/1: color is index/24bit  882=TOOTHB
;_Vars
;common SETCOLOR_COM, nkc,nkl,nkp,kkc,kkl,kkp,bwline, $ ; before 2001mar01
;	 ctab,iklr,fkc,lctab,fixkkc,fixkkl,fixkkp       ; before 2001mar01
common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,hink
;        vvWASv before 2001mar01
; kcc[0] lctab	Last color-table loaded	                 Leave alone
; kcc[1] ctab	Color table user wants to use;-3:40      May reset before call
; kcc[2] nkc	# of color levels defined in kkc.     User may reset before call
;   after an 85x call, this will be total number of distinct lines/colors
;   but the number defined will always be at least 14
; kcc[3] nkl	# of line types defined in kkl    Reset only if you redefine kkl
; kcc[4] nkp 	# of symbols defined in kkp.      Reset only if you redefine kkp
; kcc[5] bwline Color for a Black&white line.	         Leave alone
; kcc[6] 	Number of unique colors defined
; kcc[7] fkc	Low percentage of colors not used.   OBSOLETE
; kcc[8] bclr   Color for call to PLOTS for borders and titles
; kcc[9] pclr   !binc index (not value) for white line to monitor. Leave alone
; linecol intarr[1=inactive or 8] Colors for lines over B&W images.
; kkc	intarr	Array of colors.	[ These 3 are refreshed at each call  ]
; kkl	intarr	Array of linesymbols.	[ You may reset values, but not number]
; kkp	intarr	Array of psyms.		[  lasts only until next call  
;--- user generally won't need to look beyond here
; fix-kkc,-kkl,-kkp intarr Preset colors, lines, symbols 
; kink  intarr  Current indices into !binc
; hink  intarr  Holds prior kink when called with 857

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
; TESTING: plot,findgen(16)   then:  setcolor,init=85x  
; for i=0,15 do oplot,i+.1*findgen(9),line=kkl[i mod kcc[3]],color=kkc[i mod
; kcc[2]; for k=4,9 do begin & setcolor,init=850+k & for i=0,n_elements(kink)-1
; do xyouts,k,i,kink[i],color=kkc[i] & endfor
; 2010feb25 binc system uses 24-bit. Alternate is to use Torson method. First,
; have to force use of only 256 colors by calling DEVICE,Decomposed=0 , which
; causes the least significant 8 bits to be interpreted as a pseudo-color index.
; Then use top N values as colored lines, and region below as gray scale.
; All programs would have to use the top N 8-bit values for colored lines
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
; 2011jul25 HK Add 869 after revision of  COLOR24BIT -2 table
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
; 2010jan29 HK Modify some colors, inti=5=use kink in common
; 2010sep15 HK redefine kcc[6] , which had been obsolete. Revise init=0:2
; 2010oct30 HK Include setting DECOMPOSED
; 2011may04 HK Reset kink for new default !binc color scheme
;_Lim
minl=24                         ; minimum number of lines always defined
;_END

;;;help ; see who called me 

defsysv,'!binc',exists=i        ; check if !binc  is defined
if i eq 0 then defsysv,'!binc',lindgen(256);define to  allow later changes
                          ; set types
if not keyword_set(init) then inj=0 else inj=init
if inj ge 3 and inj le 850 then inj=0 ; special to handle simple code in calling programs

if inj eq 2 or n_elements(kcc) lt 9 then begin  ; COLOR initialization
    SET_PLOT,'X'                ; set to monitor
    device, get_visual_depth=vdp ; or, if !p.color gt 40000
    if vdp eq 24 then !binc=COLOR24BIT(kok,lc=-2) $ ; augmented Rainbow+white
                 else !binc=lindgen(256)
  ; undocumented IDL: psym -7 to -1  connects symbols with solid line
  ;   linesymbol <0 yields solid, >5 yields long dash.  -1 used by moviespec
    kcc=[-1,39,0,0,0,1,1,20,0,255] ; will force color table set
    fixkkc=!binc[[23,61,111,151,190,235,247,255]] ; 8 fairly distinct colors
    kkc=fixkkc
    fixkkl=[0,2,3,5,4,1] ; preset lines
    kkl=fixkkl
    fixkkp=[6,4,5,1, 7,2,3]; preset psymbols
    kkp=fixkkp
    cblack=0 & cwhite=!binc[255] ; normal monitor values
    !p.multi = [0,0,0,0,0]	; 1/page
    !p.background=cblack        ; black background, white border
    !P.color=cwhite
    kcc[8]=cwhite               ; bclr, borders & titles
    kcc[9]=255 & kcc[5]=cwhite
endif

;-------------------------------------------------------------------
if inj eq -8 then begin ; line color setup provided by Jim Torson
  if !d.table_size LT 32 then begin ; Check if enough color cells available
    print, 'There are not enough colorcells available in the public colormap.'
    print, '  You should exit from other display applications and re-start IDL.'
    goto,done
  endif
; Init the color LUTs for line plotting
;     red green blue                          white black
  r = [ 255,   0,   0, 255,   0, 255, 255, 127] ;255,  0]   
  g = [   0, 255,   0, 255, 255,   0, 127, 255] ;,  0]
  b = [   0,   0, 255,   0, 255, 255, 127, 127] ;,255,  0]
  i=n_elements(r)               ; number of line color defined above
  nfb = !d.table_size - i       ; these will be available for image
  linecol = nfb+indgen(i)       ; color levels for curves over images 
  TVLCT, r,g,b, nfb             ; load at high end
  LOADCT, 0, bottom=0, ncolors=nfb, /silent ; Load gray-scale color table
                                ;     for valid pixel display
  goto,done
endif
;-------------------------------------------------------------------


cblack=0 & cwhite=16777215    ; normal monitor values

i=n_elements(linecol) & if i eq 0 or inj eq -1 then linecol=0 ; ensure defined
if i le 1 then i=0 ; short linecol is not active 
ncol=!d.table_size -1-i  ; number of available colors levels
ctop=!p.color<255 ; top color in 8-bit scheme

if inj ge 850 then begin 
; Set up line color/style scheme
; Basic approach is to define a number of color/style combos
; and recyle thru them if necessary
; If caller needs more than this, could either
; start repeating, or make the lines wider.
  case inj of
    851:  begin  & !p.background=cblack  ; black background, white border
        !P.color=cwhite
;        !binc=COLOR24BIT(kok,lc=-2) ; bright
;        !binc[0]=!p.background & !binc[255]=!P.color
        kcc[8]=cwhite           ; bclr, borders & titles
    	kcc[9]=255 & kcc[5]=cwhite  & end   ; lines
    852: begin & !p.background=cwhite; white background, black border and labels
        !P.color=cblack
;        !binc=COLOR24BIT(kok,lc=-2,brf=0.5) ; dimmer colors
;        !binc[0]=!p.background & !binc[255]=!P.color
        kcc[8]=cblack           ; bclr, borders & titles
    	kcc[9]=0 & kcc[5]=cblack & end   ; lines
    853: begin ; printer preview to monitor
        j=155                   ; gray level, of 255
        k=long(j)+256L*j+256L^2*j ; 24-bit color word
        g=[cwhite,k]            ; two levels of gray
        i=6                     ; # of different linetypes
        kkc=lonarr(minl)
        kkl=intarr(minl)
        for j=0,minl-1 do begin ; each defined index
            kkc[j]=g[j/6 mod 2] ; grayness
            kkl[j]=fixkkl[j mod 6] ; line type cycles fastest
        endfor & end
;    854: kink=[111,151,190,235]; 4 distinct colors
    854: kink=[120,170,209,254]; 4 distinct colors
    855: j=1                   ; use kink in common
;    856: kink=[13,111,151,190,216,235] ; 6 colors
    856: kink=[30,40,120,170,209,254] ; 6 colors
    857: begin & hink=kink ; all black, insurance for printer
        kink=replicate(cblack,6) & end
;    858: kink=[13,111,151,190,235,247,255,61] ; 6+white+darkBlue
    858: kink=[13,111,151,190,235,247,255,90] ; 6+white+darkBlue
;    859: kink=[13,61,111,151,190,216,235,255,247,2,20,130,40,1] ; 7+white+6
    859: kink=[30,40,85,130,170,209,254,255,15,50,112,145,195,221] ; 7+white+6
    860: kink=hink              ; recover colors saved when set all black
    861: kink=[15,30,40,50,85,112,130,145,170,195,209,221,254,255] ; 14 colors
    862: kink=[15,30,32,40,50,60,85,114,130,143,170,197,209,221,232,254,255] ;17
    866: kink=[0,40,85,130,185,254] ; Black+5 good on white
    869: kink=[0,40,50,85,120,130,185,225,254] ; Black+8 good on white
    880: DEVICE, DECOMPOSED = 0 ; color is index into current table
    881: DEVICE, DECOMPOSED = 1 ; color is composite RGB value e.g., !binc
    882: TOOTHB
    else: begin & Message,' invalid 85x call',/cont
        kon=13 ; invalid, will cause print of Help
        goto,dokon
    end
endcase
  if inj ge 880 then return ; only change DECOMPOSED or do toothbar
  if inj ge 854  then begin; fill out the common arrays
    i=n_elements(kink)>1        ; # of different colors
    if inj eq 857 then i=1      ; all colors are black
    kcc[6]=i                    ; move into common
    kkc=lonarr(minl)
    kkl=intarr(minl)
    for j=0,minl-1 do begin     ; each defined index
        kkc[j]=!binc[kink[j mod i]] ; color cycles fastest 
        kkl[j]=fixkkl[(j/i) mod 6] ; line type cycles slower, might repeat
    endfor
  endif
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
    g=[cblack,128]              ; two levels of gray
    i=6                         ; # of different linetypes
    kkc=lonarr(minl)
    kkl=intarr(minl)
    for j=0,minl-1 do begin     ; each defined index
        kkc[j]=g[j/6 mod 2]     ; grayness, step every 6 lines
        kkl[j]=fixkkl[j mod 6]  ; line type cycles fastest
    endfor
    kcc[5]=1                    ; PS, 1=black
    kcc[8]=0                    ; ensure color is 0 for calls that plot border
endelse
if kcc[1] ne kcc[0] then  begin & LOADCT, kcc[1] & kcc[0]=kcc[1] & end
if inj lt 1 or inj gt 2 then goto,done; no prompting

kon=1 
ask: ;---------------------------------------- interactive parameter change
READ, kon, prompt='SETCOLOR: 0=return 3=ctab 5=scheme 7=# 99=UseGuide> '
dokon: 
case kon of
 -1: STOP
 0: goto,done                   ; return
1: !p.multi = [0,0,0,0,0]	; 1/page
2: !p.multi = [0,1,2,0,0]	; 2 vertical
3: begin & i=kcc[1] & GETP,'Color table',i,0,40 & kcc[1]=i & goto,refresh & end
4: !p.multi = [0,2,2,0,0]
6: !p.multi = [0,2,3,0,0]
7: begin & lowc =[-1, 0,  2,1,1,  0, 0, 0, 0,  0]
           highc=[40,40,255,6,9,255,22,85,50,255]
labc=['Current color-table      | Change ONLY' $
,'<< color table desired.  | those with << ' $
,'# of colors defined.     | here or before call' $
,'# of line types defined. MUST redefine fixkkl' $
,'# of symbols defined. MUST redefine fixkkp','Black&white line ' $
,'# of unique colors defined','<< Low % to not use (not used here)' $
,'Color for call to PLOTS for borders and titles' $
,'!binc index for monitor white']
     GETPAN,'Constants in Common',kcc,lowc,highc,labs=labc
     goto,refresh & end
9: !p.multi = [0,3,3,0,0]
62: TOOTHB,-2,VV=VV ; modify location/size of toothbar
64: TOOTHB,-4,VV=VV ; plot
66: TOOTHB ; plot default toothed color bar on current window
else: begin ; especially if =99
print,'Calling: SETCOLOR,init=  '
print,'-8: construct color table with 8 line colors reserved at top'
print,'-1: set to have no reserved line colors'
print,' 0: minimum initialization/refresh, no prompts'
print,' 1: or /init,  minimum reinitializes; and prompts'
print,'		Rainbow_&_white, 6 linestyles, 7 symbols'
print,' 2: full refresh (update current plot device and arrays) and prompt'
print,' 3:849 same as init=0'
print,'850: same as init=0             851: white line on black'
print,'852: Black line on white        853: printer preview'
print,'854,6,8,9 = 4,6,8,14  colors  855: use kink in common  857: all black'
print,'860= recover saved @857  861,862=14,17 colors in order'
print,'    Minimum number of color/line set=',minl,'  modulo for larger'
print,'    Code: plot,...  ,color=kkc[j mod kkc[2]],line=kkl[j mod kkc[3]]'
print,'880/1=device,DECOMPOSED=0 / 1: color is index / 24bit=!bink'
print,'882=quick TOOTHBar'
print,'     Here:'
print,'-1=stop  0=done'
print,' 1,2,4,6,9: # plots/page=12469. !p.multi=',!p.multi,format='(a,5i3)'
print,' 3: set color table   =',kcc[1]
print,' 7: set constants in COMMON: kcc=',ST0(kcc)
print,'66: overplot default toothed color bar  62=modify   64=plot '
print,'99=Usage Guide'
  end
endcase
goto, ask

done:
kcc[2]=n_elements(kkc)          ; always insure sizes are right
kcc[3]=n_elements(kkl)
kcc[4]=n_elements(kkp)
if !dbug ge 7 then stop
return
end
