PRO label_curve, ss,xx,yy,xloc,off=off,size=size,thick=thick,over=over $
                 ,color=color
;_Title  LABEL_CURVE  Place an oriented label on a curve
; ss	in.	Label as a string
; xx	in.	The X values for the curve, Should be monotonic.
; yy	in. 	The Y values for the curve
; xloc	in.	The X value for the middle of the label
; off	in_	An offset (toward the top of the letters) for label placement
;		  in units of character height. Default = 0.
; size	in_ 	Character Size
; Thick	in_	Character thickness
; over	in_	If set, does not first clear the area for the label.
; color	in_ 	Color index for the characters, default=B&W
;_Desc
;_Bugs still needs a little tuning for large size and offsets
;_Hist  99nov05  Hugh Kieffer, original version
; 2000jan04 HHK Use device size to get slope; increase tfac for hardcopy.
;_End

if not keyword_set(thick) then thick=1. ; IDL default for charthick
if not keyword_set(size) then size=1.   ; IDL default for charsize
if not keyword_set(off) then off=0.

if !D.name eq 'X'  then begin   ; color ok for this device
    col=!binc[255]                     ; default is white
    bg=0                        ; background is no color
    tfac=5.  ; estimate to have width of an average character, plus a little 
endif else begin                ; presume B&W printer
    col=1                       ; PS, 1=black
    bg=255                      ; white for LaserJet4
    tfac=15.
endelse

if keyword_set(color) then col=color 

;pxr=!x.crange  & delx=pxr[1]-pxr[0] ; primary x-range and scale
;pyr=!y.crange  & dely=pyr[1]-pyr[0] ; primary y-range

f=RNDEX(xloc,xx)                ; find label loc. in X array, as floating-point
yloc=RTERP1(yy,f)               ; get y location on curve
loc=fix(f)                      ; lower index
ii=[loc,loc+1]                  ;   and the next index
xy=CONVERT_COORD(xx[ii],yy[ii], /DATA, /TO_NORMAL) ;2 points in normalized units
slope= (xy(1,1)-xy(1,0))/(xy(0,1)-xy(0,0)) ;local slope
;; stop & help,ss,xx,yy,xloc,f,yloc,ii,slope
;slope= ((yy(loc+1)-yy(loc))/dely)/((xx(loc+1)-xx(loc))/delx) ;local slope
ctan=slope*!d.y_size/!d.x_size  ; factor estimated for normal landscape plots
theta=atan(ctan)                ; rotation angle
deg=!radeg*theta                ; " in degrees

up=off-0.5                      ; move to middle of label
upang=theta+!pi/2.              ; the "up" direction of characters
z=.02*size*up                   ; guess default letter height is .02 of display

xy=CONVERT_COORD(xloc,yloc, /DATA, /TO_NORMAL) ;get location in normalized units
x=xy[0] + z*cos(upang)
y=xy[1] + z*sin(upang)

if not keyword_set(over) then begin ; blank the area where label will go
    len=strlen(ss)              ; number of characters in label
    bar='H' & blank=bar
    if len gt 1 then for i=2,len do blank=blank+bar ; make a 'word" of bars
    XYOUTS,x,y,blank,alignment=0.5,charsize=size,charthick=size*thick*tfac $
       ,/normal,orientation=deg, color=bg ; blank the area for label
endif

XYOUTS,x,y,ss,alignment=0.5,charsize=size,charthick=thick $
  ,/normal,orientation=deg,color=col ; counterclock in degrees
;print,slope,theta,deg & stop
return
end
