PRO plotsect, lab, yf,  xv=xv, cs=cs 
;_Titl  PLOTSECT Plot section lines and titles when several things on one abcissa
; lab in. strarr(N)    Section labels
; yf  in. float        Fraction of way up for labels
; xv  in_ fltarr(N-1)  X values of the separations. Default is uniform
; cs  in_ float        Charactersize of labels. Default is 1.5
;_Hist  2013sep01 Hugh Kieffer  When can't find any prior version
; 2014may08 HK accomodate Y -log axis
;_End

nlab=n_elements(lab)          ; number of section

pxr=!x.crange                   ; primary x-range
pyr=!y.crange  
ya=pyr[0]+yf*(pyr[1]-pyr[0]) ; Y values for lables       
if !y.type eq 1 then begin 
   pyr=10.^pyr
   ya=10.^ya
endif
if n_elements(xv) lt (nlab-1) then begin 
  xx=pxr[0]+(findgen(nlab+1))*((pxr[1]-pxr[0])/float(nlab))
endif else xx=[pxr[0],xv[0:nlab-1],pxr[1]]

xc=(xx+shift(xx,-1))/2.        ; center of each section

if not keyword_set(cs) then cs=1.5
cs=(cs>.5)<6.                   ; reasonable limits
yf=(yf>0.) < 1.                 ; reasonable limits

for j=1,nlab-1 do plots,xx[j],pyr,line=1 ; vertical dots separating sections

for j=0,nlab-1 do xyouts,xc[j],ya,lab[j],align=.5,charsiz=cs ; each lable

return
end


