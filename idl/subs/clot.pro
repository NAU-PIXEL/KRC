PRO clot,yyy,txt,xx=xx,locc=locc,xran=xran,yran=yran,titl=titl,oplot=oplot $
,bw=bw,ksym=ksym,tsiz=tsiz,abso=abso,_extra=e
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
; abso  in  flag  If set, plots absolute value, with symbol where negative
; oplot in_ flag.      If set, overplots: ranges and titl ignored 
;                        Offset in SETCOLOR line index -=none
;                      Number of curves set by yyy
;       oplot modes:
;        -9 or less: more data; use same line set, no addition to legend
;        -n: alternate values. use line type -n, show within existing legend
;        +n: more curves, offset line index, add more items to legend
;       100+n: n=line style and  new set of labels
; bw    in_ Intarr(m)  Line type. If m>2, does in black and white
; ksym  in_ Int        Symbol to use as well as line. Will use abs-value
;                      If negative, will suppress the line
; tsiz  in_ Float  Character-size for CURVEGUIDE. Default=IDL default

; Responds to !dbug : ge 8: stop at entry.    ge 7: storp before return
;_Hist 20209dec08 Hugh Kieffer
; 2010mar16 HK Add _Extra keyword
 ;2010apr09-11 HK Enable overplot, add keyword ksym.  Done only for color code
; 2010jul14 HK Allow auto scaling of all curves
; 2010dec10 HK Fix bug that could overwrite input xx
; 2011dec31 HK Add oplot>100 mode
; 2012jan18 HK Add keyword  tsiz
; 2012feb06 HK Add keyword abso
; 2013apr01 HK Make default X value Long so will handle >32767 properly
; 2013may12 HK Include _extra in  all data plot commands
common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1
;_End

if !dbug ge 8 then stop
; help,yyy,txt,xx,locc,xran,yran,titl,oplot,bw,ksym,oplot

kok=n_elements(locc)
if kok lt 4 then loc2=[0.15,0.92,-0.04,0.08] else loc2=locc

if keyword_set(oplot) then idop=oplot else idop=0 ; line index offset
dos = idop lt 100               ; use line style from commmon
dop = idop ne 0                 ; overplot Flag
doa = keyword_set(abso)         ; plot absolute value
if dos then begin
    koff=idop>0                 ; line index offset
    kfix= (-idop)>0  & if kfix gt 5 then kfix=0 ; overplot fixed line index
endif else begin                ; fixed line style
    koff=0                      ; no index offset
    kfix=idop-100               ; line style
endelse

if not keyword_set(tsiz) then tsiz=0

dol= not idop and kok ge 5 and abs(loc2[2]) gt .3  ; put label on each curve
dog= kok ge 1 and idop gt -5 and not dol      ; do curve Guide
if dog and dop and not dos and kfix gt 0 then begin ; new line type for existing guide
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

if n_elements(xx) lt nx then xv=lindgen(nx) else xv=xx[0:nx-1]

if n_elements(xran) lt 2 then begin 
    xa=min(xv,max=xb)
    xran=[xa,xb]
end
if n_elements(yran) lt 2 then begin ; need to set Y range
    ya=min(yyy,max=yb,/nan)
    if ya gt 0 then doa=0B       ; ther are no negative values
    if doa then ya=min(abs(yyy),max=yb,/nan)
    yran=[ya,yb]
end

don=yran[0] eq yran[1] ; input nil Y range, do normalization
if don then yrn=[0,1.] else yrn=yran

if not keyword_set(titl) then titl=['Count','Constant scale','CLOT']

if not dop then plot,xv,xv,xran=xran,yran=yrn,/nodata $
,xtit=titl[0],ytit=titl[1],title=titl[2],_extra=e

if doa then begin 
    plots,.82,.02,psym=8,/norm,_extra=e
    xyouts,.83,.015,' indicates was negative',/norm
endif

jn=0                            ; default is no symbol for negative data
nlin=n_elements(bw)
ytr=''                          ; default is no individaul range guide 
gtex=''                         ; insurance
doc = nlin lt 2                 ; do color
; linn=[0,2,3,4,5,1]
for k=0,ny-1 do begin
    k2=koff+k                ;  setcolor line/color index 
    yy=yyy[*,k]
    if doa then begin 
        ii=where(yy lt 0.,jn)   ; all negative points
        yy=abs(yy)
    endif
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
        if ksym ge 0 then oplot,xv,yy,line=lin,color=clr,_extra=e ; plot data line
        if ksya ne 0 then  oplot,xv,yy,psym=ksya,color=clr,_extra=e ; add symbol
        if jn gt 0 then for i=0,jn-1 do plots,xv[ii],yy[ii],color=clr,psym=8 ,_extra=e
  if dog then CURVEGUIDE,k2,gtex,lin,locc=loc2,color=clr,ksym=ksya,charsize=tsiz
        if dol then LABEL_CURVE, gtex,xv,yy,loc2[0],off=loc2[1],size=loc2[2] $
                       ,thick=loc2[3],over=loc2[4],color=clr
    endif else begin ; monochrome
        lin=((bw[k2 mod nlin])>0) mod 6 ; ensure valid line
        oplot,xv,yy,line=lin
        if ksya ne 0 then  oplot,xv,yy,psym=ksya,_extra=e ; add symbol 
        if jn gt 0 then for i=0,jn-1 do plots,xv[ii],yy[ii],psym=8,_extra=e 
        if dog then CURVEGUIDE,k2,gtex,lin,locc=loc2,ksym=ksya,charsize=tsiz
        if dol then LABEL_CURVE,gtex,xv,yy,loc2[0],off=loc2[1],size=loc2[2] $
                       ,thick=loc2[3],over=loc2[4]
    endelse
endfor
if !dbug ge 7 then stop
; help,yyy,txt,xv,locc,xran,yran,titl,oplot,bw,ksym,oplot
; help, np,idop,dop,koff,kfix,dog,loc2,ksy,k2,lin,j

return
end
