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
; kcc[3] nkl	# of line types defined in kkl    Reset only if you redefine kkl
; kcc[4] nkp 	# of symbols defined in kkp.      Reset only if you redefine kkp
; kcc[5] bwline Color for a Black&white line.	         Leave alone
; kcc[6] iklr	Color scheme.		      User may reset to allowed values
;	          0=all B&W  1=fixed set  2=linear;  add 20 for all solid lines
; kcc[7] fkc	Low percentage of colors not used.       May reset before call
; kcc[8] bclr   Color for call to PLOTS for borders and titles
; kcc[9] pclr   !binc index (not value) for a Black&white line.	 Leave alone
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

kon=1	; set types
if not keyword_set(init) then inj=0 else inj=init
if inj eq 850 then inj=0 ; special to handle simple code in calling programs

if inj eq 1 or n_elements(kcc) lt 9 then begin  ; COLOR initialization.	
  ; undocumented IDL: psym -7 to -1  connects symbols with solid line
  ;   linesymbol <0 yields solid, >5 yields long dash.  -1 used by moviespec
  kcc=[-1,39,0,0,0,1,1,20,0,254]	; force color table set
;  fixkkc=[20,31,40,55, 74,80,90,100]; 8 fairly distinctive colors in %
  fixkkc=!binc[[23,61,111,151,190,235,247,255]]; 8 fairly distinct colors
  kcc[2]=n_elements(fixkkc)     ; preset colors
  fixkkl=[-1,2,3,5,4,1] 	 & kcc[3]=n_elements(fixkkl)  ; preset lines
  fixkkp=[6,4,5,1, 7,2,3] 	 & kcc[4]=n_elements(fixkkp)  ; preset psymbols
  !p.multi = [0,0,0,0,0]	; 1/page
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
  i=n_elements(r)               ; number of line color defined above
  nfb = !d.table_size - i       ; these will be available for image
  linecol = nfb+indgen(i)       ; color levels for curves over images 
  TVLCT, r,g,b, nfb             ; load at high end
  LOADCT, 0, bottom=0, ncolors=nfb, /silent ; Load gray-scale color table
                                             ;     for valid pixel display
  return
;-------------------------------------------------------------------
endif
if inj eq -1 then begin        ; deactivate line color
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
    851: begin & kcc[6]=0       ; set color/style to:  B&W
        loadct,0 & kcc[0:1]=0   ; set color to B&W
        kkl=fixkkl & nln=n_elements(kkl) ; preset lines and number of them
        kkc=replicate(!binc[255],nln) ; all lines white
    end 
    852: begin & !p.background=0 ; set to black background, white lines
        kcc[8]=!binc[255]       ; bclr, borders & titles
    	kcc[9]=255 & kcc[5]=!binc[255] & end   ; lines                
    853: begin & !p.background=!binc[255]; set to white background, dark lines
        kcc[8]=!binc[0]       ; bclr, borders & titles
    	kcc[9]=0 & kcc[5]=!binc[0] & end   ; lines
    854: begin & kink=[111,151,190,235]; 4 distinct colors
        kkc=!binc[kink]
        nln=n_elements(kkc)     ; number of different lines
        kkl=replicate(-1,nln)   ; all solid lines
    end                         ; line style 
    855: begin  & !p.background=0 ; set black background, colored lines
        kcc[8]=!binc[255]       ; bclr, borders & titles
    	kcc[9]=255 & kcc[5]=!binc[255] & end   ; lines 
    856: begin & kink=[61,111,151,190,235,247] ; 6 colors
        kkc=!binc[kink]
        nln=n_elements(kkc)     ; number of different lines
        kkl=replicate(-1,nln)   ; all solid lines
    end
    857: begin  & !p.background=0; black background, 6 white lines
        kcc[8]=!binc[255]         ; bclr, borders & titles
        kkl=[-1,5,2,3,4,1]  &   ; line style sequence
        kkc=replicate(kcc[5],6) ; all black
    	kcc[9]=0 & kcc[5]=!binc[255] 
        !P.color=!binc[255]
    end                       ; lines
    858: begin & kink=[61,111,151,190,235,247,255] ; 7 colors 6 lines, on black
        qq=!binc[kink]          ; color sequence 
        i=n_elements(kink)      ; # of different colors
        kkc=qq                  ; initial color set 
        q=[-1,5,2,3,4,1]        ; line style sequence
        kkl=replicate(q[0],i)   ; initial line style
        for k=1,n_elements(q)-1 do begin ; each color set
            kkc=[kkc,qq]        ; color sequence
            kkl=[kkl,replicate(q[k],i)] ; add next line style
        endfor
        nln=n_elements(kkc)     ; number of unique color/style combinations
    end 
    859: begin ; Customize 14 colors
        if n_elements(kink) lt 2 then $; 14 distinguishable colors, last 2 dim
          kink=[13,61,111,151,190,216,235,247,255,2,20,130,40,1] 
        GETPINTS,'Color set',kink,0,0 
        kkc=!binc[kink]         ; convert to color integers for current # bits
        nln=n_elements(kkc)     ; number of different lines
        kkl=replicate(-1,nln)   ; all solid lines
    end
    860: begin & !P.color=0 ; 7 dark colors, 6 line styles, white background
        kink=[0,1,21,31,51,151,235] ; dark colors
        qq=!binc[kink]          ; color sequence 
        i=n_elements(kink)      ; # of different colors
        kkc=qq                  ; initial color set 
        q=[-1,5,2,3,4,1]        ; line style sequence
        kkl=replicate(q[0],i)   ; initial line style
        for k=1,n_elements(q)-1 do begin ; each color set
            kkc=[kkc,qq]        ; color sequence
            kkl=[kkl,replicate(q[k],i)] ; add next line style
        endfor
        nln=n_elements(kkc)     ; number of unique color/style combinations
        !p.background=!binc[255] & end 
    else: Message,' invalid 85x call  x: 0=init 1=B&W  4,6,8= # colors',/cont
endcase
if inj ge 854 then begin ; set to IDL rainbow color table
        kcc[6]=1 
        loadct,39 & kcc[0:1]=39 ; set to color
endif
goto,done
end

if inj gt 2 then begin & kcc[6]=22 & kcc[2]=inj & end

; Normally does nothing above this for init=2

refresh:        ; set color according to device and options. kcc[2] == # colors
kok= !D.name eq 'X'			; color ok for this device
if kcc[6] gt 20 then kkl(*)=-1 else kkl=fixkkl ; -1 yields solid
kkp=fixkkp                      ; insure it is set
if kok then begin  ; to the monitor
    if kcc[1] ne kcc[0] or !binc[253] eq 2 then begin 
        qq= COLOR24BIT (lc=kcc[1]) ; will loadct for kcc ge 0
        !binc=qq
    endif
    kcc[5]=!binc[254] 
    kcc[8]=!binc[kcc[9]]        ; reset color for calls that plot border
endif else  begin  ; to the printer, presume Black on white background
    !binc=255L-lindgen(256) 
    kcc[5]=1                    ; PS, 1=black
    kcc[8]=0                    ; ensure color is 0 for calls that plot border
endelse
kcc[0]=kcc[1]             ; save current. to avoid unneeded recall
case (kcc[6] mod 20) of         ; iklr=requested color option
        0: kkc(*)=kcc[5]
        1: kkc=fixkkc
        2: begin & kcc[2]=(kcc[2]>2)<512 
            fkr=0.01*kcc[7]     ; lower fraction to not use
            kkc=fix(255*(fkr+(1.-fkr)*findgen(kcc[2])/(kcc[2]-1)))
            kkc=!binc[kkc]
           end
        5: kkc=!binc[[100,145,180,230,255]] ; 5 most distinctive colors
	else: begin & print,'invalid iklr @ 5' & goto,ask & end 
        endcase
if inj gt 0 then goto,done; no prompting

ask: ;---------------------------------------- interactive parameter change
READ, kon, prompt='SETCOLOR: 0=return 3=ctab 5=scheme 7=# 99=help 88=UseGuide> '
case kon of
 -1: STOP
 0: goto,done                   ; return
1: !p.multi = [0,0,0,0,0]	; 1/page
2: !p.multi = [0,1,2,0,0]	; 2 vertical
3: begin & i=kcc[1] & GETP,'Color table',i,-5,40 & kcc[1]=i & goto,refresh & end
4: !p.multi = [0,2,2,0,0]
5: begin & i=kcc[6] & GETP,'Color scheme. 0=B&W 1=8_fair 2=linear 5=5_best' $
		+' +20 for all lines solid',i,0,25 
           kcc[6]=i & goto,refresh & end
6: !p.multi = [0,2,3,0,0]
7: begin & GETPAN,'Constants in Common',kcc,lowc,highc,labs=labc
     goto,refresh & end
9: !p.multi = [0,3,3,0,0]
62: TOOTHB,-2,VV=VV ; modify location/size of toothbar
64: TOOTHB,-4,VV=VV ; plot
66: TOOTHB ; plot default toothed color bar on current window
99: begin
print,'-1=stop'
print,'1,2,4,6,9: # plots/page=12469. !p.multi=',!p.multi,format='(a,5i3)'
print,'3: set color table   =',kcc[1]
print,'5: set color scheme  =',kcc[6]
print,'7: set constants in COMMON: kcc=',ST0(kcc)
print,'66: overplot default toothed color bar  62=modify   64=plot '
print,'88=Usage Guide'
  end
88: begin
print,'Calling: SETCOLOR,init=  '
print,' -8: construct color table with 8 line colors reserved at top'
print,' -1: set to have no reserved line colors'
print,'  0: interactive prompts'
print,'  1: or /init, reinitializes; and returns without prompting'
print,'		Rainbow_&_white, 6 linestyles, 7 symbols'
print,'  2: refresh (update current plot device and arrays) return with no prompt'
print,' >2: set to this many linear-colored solid lines, no prompt'
print,'  850: same as init=0                   851: 6 B&W lines'
print,'  854: 4 distinct colors, solid lines   856: 6 colors ...'
print,'  858: 8 distinct colors for each of 6 line types'
print,'  859: Customize  14 colors      660: 7 colors 6 lines on white'
print,'  852: White line on black       853: Black line on white'
print,'  855; colors on black           857: Colors on white'
print,'Code: plot,...  ,color=kkc[j mod kkc[2]],line=kkl[j mod kkc[3]]'
  end
else: print,'Invalid entry'
endcase
goto, ask

done:
kcc[2]=n_elements(kkc)          ; always insure sizes are right
kcc[3]=n_elements(kkl)
kcc[4]=n_elements(kkp)
return
end
