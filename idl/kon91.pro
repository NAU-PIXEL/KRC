Pro kon91, ptitl,prior,hold,kon,kons,kitel, maxk=maxk,get=get
;_Titl  KON91  Common minimal functionality in the kon case statement
; ptitl	in.	String name of calling program.
; CALLER SHOULD NOT MODIFY the next 2 arguments:
; prior both. 	Strarr of prior id and string(all). Reads source if changed.
; hold	both.	Strarr of the guide. Stored between calls
; kon	in.	Integer. The case statement item
; kons	in.	Intarr. Auto-sequence of kon
; kitel	in.	String.	Prior action by calling program
; maxk  in_     Integer. Max # of kon entries.  Sent to MAKE99. Default there
; get   in_     Integer  Flag to expand GETP_   Sent to MAKE99. Default there
;_Desc
; Normally do not stop in this routine. Thus arguments in calls to lower 
; routines will be lost unless passed in the above argument list
; Easiest to leave section before  ask: calling to setcolor in main routine
; because of surrogates for elements of kcc[]
;_Usage  The following actions are reserved here:
; -3 -1 -9 121 122 8 80 800 801 803 808 85 850:860 880:881 87 88  
; 9 99 990 991 992 994 995
;
;	992 To generate a line in storage for each kon.
;		Then 994 produces a full listing of all
;	991 To expand only the current kon
;_Hist 2007aug14 Hugh Kieffer Adopt from KON99 by eliminating all
;image function and all options
; 2007sep06 HK Activate calls to setcolor
; 2011jul13:15 HK Add .png output. Direct-code for .pjg output instead of TV2JPG
;          Direct-code for .eps output instead of TV2LP     
;_Liens If caller uses   common SETCOLOR_COM2  he will have to detect kon in
;  range 850:882 to refresh any variable dependant upon that common.
;_End

kite=ptitl+'@'+strtrim(kon,2)   ; follows date in subtitle      fils[2]+' '+
if kon ge 850 and kon le 882 then begin ; set to set of high-contrast colors
    SETCOLOR,init=kon 
    return 
end 
case kon of ;...................................................................

-3: i=1                         ;-  -------- KON91  null
-1: begin & print,'Any key to GO' & qi=get_kbrd(1) & end ;- Wait

-9: stop                     ;- Stop in KON91 --------

122: GETPINTS,'Action sequence',kons,0,0 ;- Modify  kons  sequence
121: kons=-3 ;- + Reset to null

; gcmplot @79 generates a color test pattern

801: begin ;- Write positive .eps file  Expect color on white
; wasdev=!D.NAME                  ; current device
; white background. To show all lines will need to convert everything that is 
; not equal to !P.background, which is 255 in all 3 panes, or total of 765
image=tvrd(true=3); read image in current window
; add all 3 panes, any non-white becomes 764, then 127
image=byte((fix(total(image,3))>764)-637) ; either 127 or 128
set_plot,'ps'                   ; set output device to postscript
device,filename='idl.eps',bits_per_pixel=1,/port ; define filename, 1-bit 
tv, image ; write negative image to ps device. All colors -> black
device,/close
set_plot,'x' ; , wasdev  Return plotting to original device
if !P.background eq 0 then print,'WARNING, will yield black background' 
print,'Created 1-bit PS file >> epstopdf idl.eps      mv idl.pdf' & end

802: begin & write_png,'idl.png',transpose(tvrd(true=3),[2,0,1]) ;- Write .png
print,'Created PNG file >> mv idl.png' & end

803: begin & write_jpeg,'idl.jpg',true=3,tvrd(true=3),quality=90 ;- Write .jpg
print,'Created qual=90 JPEG file >> mv idl.jpg' & end

804: begin ;- Reverse Black and white image to 1-bit .eps file
; wasdev=!D.NAME                  ; current device 
image=tvrd() ; black=0, white=255
set_plot,'ps'                   ; set output device to postscript
device,filename='idlr.eps',bits_per_pixel=1,/port ; define filename, 1-bit 
tv,not image ; write negative image to ps device. 
device,/close
set_plot,'x' ; , wasdev  Return plotting to original device
if !P.background ne 0 then print,'WARNING, may yield black background' 
print,'Created 1-bit PS file >> epstopdf idlr.eps      mv idlr.pdf' & end

8:  GRAPH,8,hard               ; Start Graph to file and printer
80: GRAPH,0,hard               ;- Reset plot output device
85: SETCOLOR,init=0            ;- Set Color,line,symbol scheme
87: GRAPH,7,hard               ;- Close plot device, no spawn of plot
88: SUBTITLE,id=kitel          ;- Add subtitle to plot
9:  GRAPH,9,hard               ;- End a plot, save and print the file

99: begin                       ; Action guide. Customise for each routine
MAKE99,ptitl,prior,hold,maxk=maxk,get=get; make guide to calling program
print,'0=Stop  121=kons=-3  122=Edit Kons  801/2/3/4 output to eps/png/jpg/-eps' 
print,'Plots : 8=new  80=restart  85[x]=SETCOLOR  87=close  88=subtitle  9=plot'
print,'MAKE99: 991=Expand current kons   992/995=1-line each   994=expand all'
print,'123: Do kons=',ST0(kons)
end  

991: if n_elements(kons) ge 2 then $ ; Expand current kons in MAKE99
     MAKE99,ptitl,prior,hold,all=kons,maxk=maxk,get=get ; Expand current Kons 
992: MAKE99,ptitl,prior,hold,all=23  ,maxk=maxk,get=get ; Full 1 line each
994: MAKE99,ptitl,prior,hold,all=8   ,maxk=maxk,get=get ; Expand all Kons
995: MAKE99,ptitl,prior,hold,all=23  ,maxk=maxk,get=get,comf='./subs/kon91.pro'; And kon91
  else: print,'KON91: Invalid entry'
endcase

return
end
