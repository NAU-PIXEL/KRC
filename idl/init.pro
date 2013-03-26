PRO init
;_Titl INIT  Procedure to initialize IDL environment
;_Calls  SETCOLOR
;_Hist  2003jul01 Hugh Kieffer
; 2004sep27 HK Add setting some environment variables
; 2005jul16 HK Copy and modify for ROLO distribution (delete KRC stuff)
; 2006oct06 HK Add ice spectra and shared object library locations
; 2008mar23 HK Fix solib to be CPU-dependant
; 2008jul16 HK add PROJDAT and PROJSRC, set to OLI Dirs.
; 2008aug02 HK Add debug global flag. Clean up for OLI distribution
; 2008aug~15 Andres E. Haralson of BATC, where HOST is not defined for adcws6
; 2008aug21 HK Reorganize to isolate the HOST issue
; 2009aug01 HK Minor cleanup, more generic naming
; 2010apr15 HK Define  psym[8]
; 2010jul20 HK Replace use of tvfast to set display sizes with explicit code
;_End

common TVFAST_COM, safe ; intarr(3) [X,Y Display pixel limits, backing]

cpu=getenv('HOST')              ; get current cpu, Will return null is undefined
if strlen(cpu) eq 0 then cpu='undefi' ;<<<1 Force in case HOST is not defined
if cpu eq 'hulk.localdomain' then cpu='hulk'

myhome=getenv('HOME')+'/'       ; Get home directory

; some common defaults
idltop=myhome+'idl/'            ; Top of IDL source tree
solib=idltop+'externals/ftnwrap.so' ; shared object library
prjdat='/NotInInit/'            ; Location of Project Large data files
prjsrc='/NotInInit/'            ; Location of Project other files
specdir='/NotInInit/'           ; Primary ice spectral models
outid='Kieffer'	                ; ID to appear in plot subtitles
spice='q'                       ; Indicate that SPICE is not installed
retain=2     ; Default backing store option; set to IDL provides. See IDL manual
win=[1280,1000]                 ; Common display size
mybw='HP_Laserjet_3330'         ; B&W printer
case cpu of
  'hulk': begin               ; CR primary computer
      spice='/work1/SPICE/icy/lib/icy.dlm'
      solib=idltop+'externals/ftnwrap64.so' ; shared object library
      prjdat='/work2/OLI/'      ; Project large files
      prjsrc=myhome+'cr/BOLI/'  ; Project other files
      specdir='/work1/mars/miebin/' ; Primary ice spectral models
      end
  'hkieffer': begin      ; MAC laptop
;;      myhome='/Users/hkieffer/'
  end
  'undefi': begin               ;<<<1 distribution recipient
      idltop='/data/OLI/Lunar_Cal/idl/' ;<<< Top of distribution IDL
      solib=idltop+'ext/ftnwrap64.so' ;<<< shared object library
      prjdat='/data/OLI/Lunar_Cal/' ;<<< Location of Project Large data files
      prjsrc='/data/OLI/Lunar_Cal/' ;<<< location of Project other files
      outid='Kvaran'            ;<<< User name on output plots
      retain=2                  ;<<< Set backing store option
      win=[1280,1000]           ;<<< set display size
      mybw='HP_Laserjet_3330'   ;<<< B&W printer
   end
  else: Message,'Computer not recognized, so globals not defined'

  endcase

wset,0                          ; set to window 0
setenv,'MYHOME='+myhome         ; my home path
setenv,'IDLTOP='+idltop         ; top of my IDL source tree
setenv,'SOLIB='+solib           ; shared object library
setenv,'MYBW='+mybw             ; black&white printer
setenv,'SPECDIR='+specdir       ; Ice spectra model computation
; Project areas
setenv,'PROJDAT='+prjdat        ; top of Project large data files
setenv,'PROJSRC='+prjsrc        ; top of Project source files [other than IDL code]

!x.style=1 & !y.style=1         ; set plots to use exact range

defsysv,'!idltop',exists=i      ; path to top of IDL run area
if i eq 0 then begin            ; no, so define it
    kok=9                       ; for use in next call to set read_only
    defsysv,'!idltop',idltop,kok; make it permanent
endif

defsysv,'!dbug',exists=i        ; debug flag
if i eq 0 then begin            ; no, so define it
    defsysv,'!dbug',0B          ; allow it to be modified
endif

defsysv,'!outid',exists=i       ; User ID for IDL plots
if i eq 0 then begin            ; no, so define it
    kok=9                       ; for use in next call to set read_only
    defsysv,'!outid',outid,kok  ; make it permanent
endif

if spice ne 'q' then begin ; option to enable SPICE system
    qq='0' & read,qq,prompt='Enter 1 to link to SPICE '
    if qq eq '1' then dlm_register,spice
endif

window,0,retain=retain          ; Open default window with backing store option
safe=[win,retain]               ; Init display size and backing store in Common
SETCOLOR,init=1                 ; Start the colors. Will define !binc
a=!dtor*30. & c=cos(a) &  s=sin(a)          ;| Define psym=8 as
usersym,[0.,c,-c,0.]*1.4,[-1.,s,s,-1.]*1.4  ;| inverted triangle
print,'env MYHOME= ',myhome
print,'   !idltop= ',!idltop
print,'   !outid = ',!outid
return
end
