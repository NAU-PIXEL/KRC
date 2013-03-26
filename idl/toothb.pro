PRO toothb, kp,jj, vv=vv,fmt=fmt
;_Titl  TOOTHB   Add a toothed color scale-bar to a window or TVPLEX panel.
; kp	in_	Window index  OR  Index of panel within the tvplex window. Def=0
; jj	in_	The -plex constants integer array. Default is single window.
; ----- 
; If no arguments are present, applys to the current window.
; If one argument (kp)  is present, it should be an index of a valid window
;  to which the colorbar will be added OR the 3-bit coded negative value:
;	where N:  -1 = initialize vv with all default values
;       	  -2 = invoke interactive modification
;	          -4 = do a plot before return
;	E.g.,  -7 will do all three of the above actions
; If two arguments are present, then this routine will apply to a 
; multi-panel (plex) window, and the locations are treated as normalized within 
; a single panel. In this case, locations outside of a panel, but within 
; the display, are allowed 
; Note: All parameters in terms of 8-bit color; this routine will
; convert to other systems
;-------
; vv	both	Fltarr( ) of locations and sizes. See labr below
; fmt	in_	Format for values, e.g., 'f4.1'. Ignored if no values set
;_Usage
; May call  toothb,-N,vv=vv1   to initiate/modify
; E.g., -7 will do all three of the above actions
; may also modify vv1 externally
; if vv is absent, then starts with all default value
; OR may call  toothb,-2,vv=vv1 , which will invoke modification prompts
;_Desc
; Uses a fat | to fill the area of each tooth
;_Hist 99jul15  Hugh Kieffer  Original IDL version of toothbar
; 99aug28 to 2001apr26, various edits
; 2001nov01 HK Big recode, move all numerical options into one array
; 2002oct24 HK Add value above last tick if >[25] above prior 50.
; 2003aug06 HK Make max number of color steps 256 rather than !P.color
; 2003aug16 HK Add indexes 17 and 18 to vv. 8-bit colorcode until final stage.
; 2004nov02 HK Replace all byte-dependent color with use of !binc
; 2007sep12 HK Also replace the bar label colors for X 
;_End

labr=['X-location of left end of bar, normalized','Y-location of base "' $
,'X-spacing of | ','| Character thickness.  Use 8 for LaserJet4' $
,'Height of bar between teeth in character units' $
,'Relative height of 5% teeth',' " 10% "',' " 50% " ' $
,'Number of colors (and bars)' $; for some B&W printers this would be 0
,'Max color value to use in bar' $
,'Value to print at first tick' $
,' "      "    "    last;  =above means no values' $
,' "  relative vertical position' $ ; Reasonable values are .15 or Default
,' "  Character size' $
,' "  label BincIndex if above bar','[dbug]','top-n50 for value alignment' $
,'Fraction of range for first color' $
,'NOPE Bytes in color scheme,   le 0=automatic']
par0=[0.25,0.1,.0025,3.,2., 1.25,1.5,1.8,256,255 $
,0.,0., 1.8, 1.,-1,0, 25,0.,0]

winin=!d.window                 ; save current window index
nparm = n_params()              ; number of non-keyword arguments
n=n_elements(par0)              ; size required for vv

if nparm lt 1 then kp=0         ; default action
if kp lt 0 then i=-kp else i=4  ; default is to only plot
do1 =      i     mod 2 eq 1     ; initialize
do2 =ishft(i,-1) mod 2 eq 1     ; modify
do4 =ishft(i,-2) mod 2 eq 1     ; do plot before return
if (not keyword_set(vv)) or do1 then vv=par0  ; all defaults
if n_elements(vv) ne n then MESSAGE,'vv must be size='+string(n)

if do2 then GETPAN,'Toothbar constants',vv,0.,0.,labs=labr ; interactive modify

if not do4 then return ; no plot

x0=vv[0] & y0=vv[1]
if nparm eq 2 then begin        ; apply to a TVPLEX window 
    WSET,jj[1]                  ; set to -plex window
    xwinsize=float(jj[28])      ; window size in device pixels
    ywinsize=float(jj[29])
    xfac=jj[24]/xwinsize        ; scale panel to the full display
    yfac=jj[25]/ywinsize
    kx=kp/jj[9] & ky=kp-kx*jj[9] ; panel indexes, Integer.
    xloc=jj[10]+kx*(jj[12]+jj[24]) ; starting pixel of panel
    yloc=jj[11]+ky*(jj[13]+jj[25])
    x0=xloc/xwinsize+xfac*x0    ; bar start in normalized units
    y0=yloc/ywinsize+yfac*y0
    top=jj[4]                   ; top color value used in host panel
endif else begin
    if kp gt 0 then WSET,kp     ; set to argument_1 window
    yfac=1.0
    top=(long(vv[9]) > 2)       ; max color level
;; print,'top@1=',top
endelse
top=top<255                     ; insure in 8-bit range
xs=vv[2]
siz=vv[4]

ncr=fix(vv[8]); number of colors and | bars
ncr=(ncr > 2) < 256 ; number of colors and | bars
tooth=replicate(1.,ncr) & ii=indgen(ncr) ; make height of each character
j=where(ii mod 5  eq 0) & tooth[j]=vv[5] ; a little taller for each 5
j=where(ii mod 10 eq 0) & tooth[j]=vv[6] ; taller yet for each 10
j=where(ii mod 50 eq 0,n50) & tooth[j]=vv[7] ; taller yet for each 50

bot=0>((vv[17]<0.5)*top) ; lowest color level, 0 to 127
fac=float(top-bot)/float(ncr-1)       ; color scaling
qq=round(bot+fac*findgen(ncr)) ; values within the range 0:255

for i=0,ncr-1 do XYOUTS,x0+i*xs,y0,'|',color=!binc[qq[i]],/normal $ ; bar
	,charsize=siz*tooth[i],charth=vv[3],alignment=0.5

; for X, black is large number, for PS, black is 1
if vv[10] ne vv[11] then begin  ; do values every 50
    vcol=(fix(vv[14]) >0)<255       ; label color
    if !D.name eq 'X' then begin ; set black and white colors
        black=1
        white=255
    endif else begin
        black=255
        white=1
    endelse

; y0 is the normalized location of base of colorbar
yc=!D.Y_CH_SIZE/float(!D.Y_SIZE) ; default Y charactersize, as fraction of window
;   height of colorbar between teeth is siz*yc
; vv[12]= fractional position within colorbar (between teeth) of bottom of value
    yv=y0+vv[12]*siz*yc ; y location of value, normalised to window
    if keyword_set(fmt) then form='('+fmt+')' else form='(g10.3)'
    il=-50
    for j=0,n50  do begin       ; values go on the 50's teeth, and high end
        i=(50*j) < (ncr-1)
        if i-il gt vv[16] then begin
            alig=0.5            ; internal alignment
            vci=vcol            ; attempt for contrast
            if vv[12] lt 0.9 then begin ; place values within color bar
                if j eq 0 then alig=0. else if j eq n50-1 then alig=1. 
                if j eq 0 then vci=white else vci=black
            endif
            vnow=vv[10]+(vv[11]-vv[10])*(i/float(ncr-1)) ; value here
            sv=strtrim(string(vnow,format=form),2) ; no blanks
          XYOUTS,x0+i*xs,yv,sv,/normal,alignment=alig,color=!binc[vci] $
            ,charsize=vv[13]
        endif
        il=i
    endfor
endif

if nparm ge 1 then WSET,winin   ; reset to initial window
if vv[15] then stop
return
  end
