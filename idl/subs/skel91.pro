;_Titl skel91   Short IDL skeleton, uses KON91; no image processing
;_Calls  Utility: KON91  SETCOLOR  GETPAN  GETPSN  ST0
;_Hist 2009apr13 Hugh Kieffer   see skel99 for version using KON99
; 2010nov04 HK Update for SETCOLOR than handles 860+
; 2011oct01 2011dec03 HK Include sample of sequence with loop
;_End
ptitl='skel91'

common SETCOLOR_COM2, kcc,linecol,kkc,kkl,kkp,fixkkc,fixkkl,fixkkp,kink,scex1

labf=['Input DIR',' " file']
parf=['/work1/lunar/GOES/','d02346_4600_4800.out.txt']

komit=22 ; ask each time
labi=['komit 19=no ask','spare'] 
pari=[komit,-7]; ask each time

labr=['spare']
parr=[ 7.7] 

kite=''                         ; type definitions
prior=['1','1']                 ; impossible values, to initiate MAKE99
noop=60 & koop=70               ; insurance for looping @125
;^^^^^^^^^^^^^^^^^^^^^^ inputs
kons=[850,860] & lkon=0B ; sequence for initiate colors. May augment
kon=123 ; & goto,dokon ; do auto-initiation
;===============================================================================
ask: ;---------------------------------------- interactive parameter change
konl=kon
if lkon then begin              ; auto-sequence
    kkon=kkon+1                 ; increment to next item
    kon=kons(kkon)              ; get action from list
    print,'Doing -------------->',kon ; Notice of current action to monitor
    if kkon eq lastkon then lkon=0B ; turn off further auto-processing
endif else begin
    sureask: lkon=0B                 ; forced request for action
    READ, kon, prompt=ptitl+' Enter selection: 99=help 0=stop 123=auto> '
endelse
dokon: kitel=kite               ; remember prior action to use in subtitle
kite=ptitl+'@'+strtrim(kon,2)   ; follows date in subtitle      fils[2]+' '+
nkc=kcc[2] & nkl=kcc[3]         ; in case SETCOLOR was called
case kon of ;...................................................................
; DO NOT USE those defined in KON91:
; -1 -3 -9 100:3 121 122 8 80 85 87 88 801:4 808 850:860 880:899  
; 9 99 991 992 994 995

-2: return                      ;< return
0: stop                         ;< stop

123: begin & lkon=1b & kkon=-1  ;- start auto-script 
lastkon=n_elements(kons)-1 & end ; last preset command 

125: begin & kons=[20,21,22, 43,44,45,126,67,68] ; loop over set 
; above fake sequence; 2x to be done once. 4x to be in loop 
; 6x done once after loop. If no 6x, use a single -3 
koop=0
kon1=2               ;<<< index of last item to be done only once before looping
noop=fix(parp[1])               ;<<< number of loops
tus=pval[24] &  tus3=parp[0]    ;<<< any special loop initialization
hloop=fltarr(noop,5)            ;<<< any result storage
komit=19                        ; do not ask for approval
lastkon=n_elements(kons)-1      ; Start auto-sequence
lkon=1b & kkon=-1 & end         ; Start band Looping 

126: begin ;+ loop increment test
hloop[koop,*]=[koop,tus,aaa[0],bbb[1]] ;<<< save results of one loop
if koop lt noop-1 then begin     ; loop for another
    koop=koop+1                 ; loop index
    tus=tus+tus3                ; <<< any other increment
    kkon=kon1                   ; start at the beginning of loop
endif else begin                ; any completion action
    komit=pari[0]               ; return komit to prior state
endelse & end 

11: GETPSN,'File names: parf',parf,lab=labf ;- Modify parf strings 
15: begin & GETPAN,'Integer values: pari',pari,-1,999,labs=labi ;- Modify pari
komit=pari[0] & end
16: GETPAN,'Float values: parr',parr,0.,0.,labs=labr ;- Modify parr floats
18: help,kon             ;<< Guides and help
;<< add case steps here 

else: begin & KON91,ptitl,prior,hold,kon,kons,kitel ;=KON99 < needed by MAKE99
      if kon eq 99 then begin 
          print,'11: files:  parf= '
          print,'15: Integs: pari= ',ST0(pari)
          print,'16: Floats: parr= ',ST0(parr)
      endif & end
endcase
goto, ask

halt:  print,'SOME ERROR CONDITION at kon=',kon,'.  Any key to Go'
i=get_kbrd(1) & goto,sureask
end
