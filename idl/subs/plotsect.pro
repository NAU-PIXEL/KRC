PRO plotsect, lab, yf,  xv=xv, cs=cs
;_Titl  PLOTSECT Plot section lines and titles when several things on one abcissa
; lab in. strarr(N) of section labels
; yf  in_ float fraction fo way up for labels
; xv  in_ fltarr(N-1) of X values of the separations. Default is uniform
; c   in_ float. Charactersize of labels. Default is 1.5

nlab=n_elements(lab)

pxr=!x.crange                   ; primary x-range
pyr=!y.crange         

if n_elements(xv) lt (nlab-1) then begin 
  xx=pxr[0]+(findgen(nlab+1))*((pxr[1]-pxr[0])/float(nlab))
endif else xx=[pxr[0],xv[0:nlab-1],pxr[1]]

xc=(xx+shift(xx,-1))/2.        ; center of each section

if not keyword_set(cf) then cf=1.5
cf=(cf>.5)<6.                   ; reasonable limits
yf=(yf>0.) < 1.                 ; reasonable limits

for j=1,nlab-1 do plots,xx[j],pyr,line=1

ya=pyr[0]+yf*(pyr[1]-pyr[0])
for j=0,nlab-1 do xyouts,xc[j],ya,lab[j],align=.5,charsiz=cs

return
end


