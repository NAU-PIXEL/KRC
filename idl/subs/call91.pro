function call91, kitel,text
;_Titl  CALL91  Quick access to the functions of KON91
; kitel in. String. Prior action by calling program; passed to KON91
; text  in_ String to print here
; func. out.  Flag: set true if caller should stop
;_Calls  Utility: KON91
; Check KON99 for actions already assigned
;_Hist 2015oct01 Hugh Kieffer  To allow plots to files 
;_End                .comp call91

ptitl='kon91'     ; reasonable taget for a source file. Should never be needed
prior=['1','1']   ; impossible values, to initiate MAKE99 if called
kon=0             ; type definition
kons=[-1,-1]      ; dummy
;===============================================================================
if keyword_set(text) then print,'<<call91>> ',text
ask: ;------------ top of action loop ------------ interactive parameter change

READ, kon, prompt=' KON91 select: 1or-2=done 0=stopInCall > '
if abs(kon) le 2 then if kon eq 0 then return,1B else return,0B

KON91,ptitl,prior,hold,kon,kons,kitel ;=KON99 < needed by MAKE99
goto, ask

end
