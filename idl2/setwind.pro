PRO setwind, kon 
;_Titl  SETWIND  Create windows 0,2,3 of preset sizes
; kon in. Integer, 889:899. See case statement below

; Assume common is initialize when IDL stated
common TVFAST_COM, ppp
; 0:2 used by TVFAST:  [X,Y Display pixel limits, backing]
; 3:5 general window creation: [xsize,ysize,backing]
;_Desc
; 1970GX     is 1280x1024 = 5/4
; Dell U3011 is 2560x1600 = 16/10

;_Hist 2012jan24 Hugh Kieffer
;_End

case kon of
 890: window,0,xsize=ppp[3],ysize=ppp[4],retain=ppp[5] ; create 0
 891: begin                     ; set backing
     i=0
     read,i,prompt='Retain: 0=none 1=X-windows 2=IDL > '
     ppp[5]=(i>0)<2             ; valid backing
 end
 892: window,2,xsize=ppp[3],ysize=ppp[4],retain=ppp[5] ; create 2
 893: window,3,xsize=ppp[3],ysize=ppp[4],retain=ppp[5] ; create 3
 894: ppp[3:4]=[640,512]        ; 5/4 default for older monitor
 895: ppp[3:4]=[800,640]        ; 5/4 mid-size
 896: ppp[3:4]=[800,800]        ; 30", appears square 
 897: ppp[3:4]=[1200,960]       ; 5/4 for 30", allows central gap
; 898: ppp[3:4]=[1280,800]       ; default  2560x1600. e.g., for Dell 30"
 898: ppp[3:4]=[2550,1520]      ; Maximum for Dell 30" with borders
 899: begin                     ; guide
     print,'890+ 0= create window 0:  2=2  3=3    1=set backing    9=this Guide'
     print,' Set size:  4=5/4 old  5=5/4 mid  6=square 7= 5/4 big  8=DellMax'
 end
else: message,'invalid kon'
endcase

return
end
