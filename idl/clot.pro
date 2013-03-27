PRO clot,yyy,txt,xx=xx,locc=locc,xran=xran,yran=yran,titl=titl,oplot=oplot $
,bw=bw,ksym=ksym,_extra=e
;_Titl  CLOT  Color plot of related curves
; yyy   in. array(n,m=numCurves)  Data to plot
; txt   in_ strarr(m)  Labels for curves. Default is 0-based count
; xx    in_ array (n)  Abcissa positions, default is 0,1,2...
; locc  in_ fltarr(4)  Loc. for Guide in NORMAL units ala CURVEGUIDE.
;           fltarr(5)  [xloc,off,size,thick,over] for LABEL_CURVE  
;              if absent, no guide. If less then 4 use [0.15,0.92,-0.04,0.08]
; xran  in_ fltarr(2)  X plot limits:  Default is automatic. Ignored for oplot
; yran  in_ fltarr(2)  Y plot limits:  Default is automatic. Ignored for oplot
;                     if values are equal, auto-scales each curve onto 0:1
; titl  in_ strarr(3)  Lables for x,y axes and top. Ignored for oplot
; oplot in_ flag.      If set, overplots: ranges and titl ignored 
;                        Offset in SETCOLOR line index -=none
;                      Number of curves set by yyy
; oplot modes:
; -9 or less: more data; use same line set, no addition to legend
; -n: alternate values. use line type -n, show within existing legend
; +n: more curves, offset line index, add more items to legend
; bw    in_ Intarr(m)  Line type. If m>2, does in black and white
; ksym  in_ Int        Symbol to use as well as line. Will use abs-value
;                      If negative, will suppress the line
; Responds to !dbug
;_Hist 20209dec08 Hugh Kieffer
; 2010mar16 HK Add _Extra keyword
 ;2010apr09-11 HK Enable overplot, add keyword ksym.  Done only for color code
; 2010jul14 HK Allow auto scaling of all curves
; 2010dec10 HK Fix bug that could overwrite input xx
common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_End

if !dbug then stop
; help,yyy,txt,xx,locc,xran,yran,titl,oplot,bw,ksym,oplot

if keyword_set(oplot) then idop=oplot else idop=0 ; line index offset
dop = idop ne 0                 ; overplot Flag
koff=idop>0                     ; line index offsetk
kfix= (-idop)>0  & if kfix gt 5 then kfix=0 ; overplot fixed line index
kok=n_elements(locc)
if kok lt 4 then loc2=[0.15,0.92,-0.04,0.08] else loc2=locc


dol= not idop and kok ge 5 and abs(loc2[2]) gt .3  ; do curve label
dog= kok ge 1 and idop gt -5 and not dol      ; do curve Guide
if dog and dop and kfix gt 0 then begin ; new line type for existing guide
        loc2[1]=loc2[1]+0.25*loc2[2] ; Y offset
        loc2[3]=.8*loc2[3]      ; shorter line, will offset symbol if used
    endif

if not keyword_set(ksym) then ksym=0 
ksya=abs(ksym)<8                  ; ensure not beyond psym valid range
siz=size(yyy)
if siz[0] ne 2 and not dop then message,'Arg1 must be 2D, 1D allowed for oplot'
nx=siz[1]
if siz[0] eq 2 then ny=siz[2] else ny=siz[1]

np=n_params() ; 
if np lt 2 then txt=strtrim(indgen(ny),2) ; curve numbers

if n_elements(xx) lt nx then xv=indgen(nx) else xv=xx[0:nx-1]

if n_elements(xran) lt 2 then begin 
    xa=min(xv,max=xb)
    xran=[xa,xb]
end
if n_elements(yran) lt 2 then begin 
    ya=min(yyy,max=yb,/nan)
    yran=[ya,yb]
end

don=yran[0] eq yran[1] ; input nil Y range, do normalization
if don then yrn=[0,1.] else yrn=yran

if not keyword_set(titl) then titl=['Count','Constant scale','CLOT']

if not dop then plot,xv,xv,xran=xran,yran=yrn,/nodata $
,xtit=titl[0],ytit=titl[1],title=titl[2],_extra=e

nlin=n_elements(bw)
ytr=''                          ; default is no individaul range guide 
gtex=''                         ; insurance
doc = nlin lt 2                 ; do color
; linn=[0,2,3,4,5,1]
for k=0,ny-1 do begin
    k2=koff+k                ;  setcolor line/color index 
    yy=yyy[*,k]
    if idop ge 1 or not dop then gtex=txt[k] else gtex=''  
    if don then begin ; auto-scale
        ya=min(yy,max=yb)
        if ya ne yb then yy= (yy-ya)/(yb-ya) ; scale onto [0,1]
        gtex=gtex+ST0([ya,yb]) ; range as text
    endif


    if doc then begin ; color
        clr=kkc[k2 mod kcc[2]]
        lin=kkl[k2 mod kcc[3]]
        if kfix gt 0 then lin=kfix
        j=lin ; for use by  CURVEGUIDE
        if ksym ge 0 then oplot,xv,yy,line=lin,color=clr ; plot data line
        if ksya ne 0 then  oplot,xv,yy,psym=ksya,color=clr ; add symbol
        if dog then CURVEGUIDE,k2,gtex,lin,locc=loc2,color=clr,ksym=ksya 
        if dol then LABEL_CURVE, gtex,xv,yy,loc2[0],off=loc2[1],size=loc2[2] $
                       ,thick=loc2[3],over=loc2[4],color=clr
    endif else begin ; monochrome
        lin=((bw[k2 mod nlin])>0) mod 6 ; ensure valid line
        oplot,xv,yy,line=lin
        if ksya ne 0 then  oplot,xv,yy,psym=ksya ; add symbol
        if dog then CURVEGUIDE,k2,gtex,lin,locc=loc2,ksym=ksya
        if dol then LABEL_CURVE,gtex,xv,yy,loc2[0],off=loc2[1],size=loc2[2] $
                       ,thick=loc2[3],over=loc2[4]
    endelse
endfor
if !dbug then stop
; help,yyy,txt,xv,locc,xran,yran,titl,oplot,bw,ksym,oplot
; help, np,idop,dop,koff,kfix,dog,loc2,ksy,k2,lin,j

return
end
