# Makefile for KRC distribution  2012nov22

# Set up some shell-level specific variables
SHELL=/bin/csh

FC=gfortran  # NEWER    
#FC=g77 -pipe   # OLDER then get unrecognized command line option "-fd-lines-as-comments"   -xld  set to compile D lines ??

# -fd-lines-as-comment is listed as gfortran option: http://gcc.gnu.org/onlinedocs/gfortran/Option-Summary.html#Option-Summary  

FFLAGS= -g -fno-automatic -fno-second-underscore -fargument-alias -fd-lines-as-comments
# -fbounds-check
# -Wall
LD=g77

SYSLIBS =  -lg2c -lc -lm

#  library groups
LIBDIRS=

CLIB=./cfiles/libhk_cisis.a

# system dependencies
OBJECTS1 = krc.o tseas.o tlats.o tday.o tcard.o tprint.o tdisk.o  albvar.o alsubs.o aveday.o aveyear.o binf5.o catime.o climtau.o co2pt.o datime.o deding2.o eccanom.o epred.o evmono3.o finterp.o bigend.o idarch.o orbit.o porb0.o porb.o readtxt360.o rndex.o rotmsp.o rotv.o rotvec.o rterp.o seasalb.o seastau.o sigma.o tint.o vaddsp.o vcros.f vfdot.o vfmag.o vlpres.o vrotv.o white1.o xtreme.o 

OBJECTSM = mkrc.o  mlats.o tday.o tcard.o tprint.o tdisk.o epred.o \
           porb0.o tint.o albvar.o co2pt.o vlpres.o aveday.o porb.o orbit.o \
           eccanom.o alsubs.o deding2.o seasalb.o seastau.o readtxt360.o \
           finterp.o evmono3.o idarch.o r2i2s.o white0.o readaie.o \
	  binf5.o catime.o datime.o rndex.o rotmsp.o rterp.o sigma.o \
	vaddsp.o white1.o xtreme.o

OBJECTSP = porbmn.o porbio.o porb.o porb1.o porbqq.o orbit.o eccanom.o \
          obliq.o oblip.o ephemr.o spcrev.o caldate.o porbel.o ymd2jd.o \
	 ang360.o b2b.o caldat.o datime.o julday.o mprint.o mprod3.o rotax.o rotdia.o rotvec.o   

# normal link
krc: cfiles/libhk_cisis.a $(OBJECTS1) # -xl required for record length to be in words
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJECTS1) \
	$(CLIB) $(SYSLIBS)

cfiles/libhk_cisis.a:
	cd cfiles; ${MAKE} ${MFLAGS}

# map movie version
mkrc: $(OBJECTSM)     # -xl required for record length to be in words
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJECTSM) \
	$(CLIB) $(SYSLIBS)

# Planetary Orbits
porbmn: $(OBJECTSP)
	$(FC) $(LDFLAGS)  -o $@ $(OBJECTSP) \
	$(SYSLIBS)

# routines with dependencies
krc.o:   krc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc  
mkrc.o: mkrc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 

tseas.o: tseas.f krccom.inc latcom.inc hatcom.inc units.inc 
mlats.o: mlats.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc
tlats.o: tlats.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc
tday.o: tday.f krccom.inc daycom.inc hatcom.inc units.inc
tyearp.o: tyearp.f krccom.inc latcom.inc hatcom.inc
seasalb.o: seasalb.f units.inc filcom.inc
seastau.o: seastau.f units.inc filcom.inc
tcard.o: tcard.f krccom.inc latcom.inc daycom.inc units.inc 
tprint.o: tprint.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 
tdisk.o: tdisk.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc filcom.inc
porb1.o: porb1.f units.inc porbcm.inc

ephemr.o: ephemr.f   porbcm.inc
porb.o:   porb.f     porbcm.inc
porb1.o:  porb1.f    porbcm.inc
porbio.o: porbio.f   porbcm.inc
porbmn.o: porbmn.f   porbcm.inc
porbqq.o: porbqq.f   porbcm.inc


# no additional dependancies
albvar.o: albvar.f
alsubs.o: alsubs.f 
aveday.o: aveday.f 
aveyear.o: aveyear.f 
binf5.o: binf5.F
climtau.o: climtau.f
co2pt.o: co2pt.f
deding2.o: deding2.f
eccanom.o: eccanom.f
koft.o: koft.f
orbit.o: orbit.f
porb.o:  porb.f
porb0.o: porb0.f
porbel.o: porbel.f 
oblip.o:  oblip.f  
obliq.o:  obliq.f  
orbit.o:  orbit.f
readaie.o: readaie.f
readtxt360.o: readtxt360.f  
spcrev.o: spcrev.f
tint.o:  tint.f
tyearp.o: tyearp.f
vlpres.o: vlpres.f

# could be put in a library
ang360.o: ang360.f
b2b.o: b2b.f
caldat.o: caldat.f
julday.o: julday.f
catime.o: catime.f
datime.o: datime.f
evmono3.o: evmono3.f
# epred.o: epred.f 
finterp.o: finterp.f
# idarch.o: idarch.F
mprint.o: mprint.f 
mprod3.o: mprod3.f
r2i2s.o: r2i2s.f
rndex.o: rndex.f
rotax.o: rotax.f
rotdia.o: rotdia.f
rotmsp.o: rotmsp.f   # many routines 
# rotv.o 
rotvec.o: rotvec.f   
rterp.o: rterp.f
sigma.o: sigma.f
vaddsp.o: vaddsp.f   # many routines
vcros.O: vcros.f
vfdot.o: vfdot.f 
vfmag.o: vfmag.f
vrotv.o: vrotv.f 
white0.o: white0.f 
white1.o: white1.f
xtreme.o: xtreme.f
bigend.o: bigend.f
ymd2jd.o: ymd2jd.f

.PHONY: clean

clean: cfiles/clean 
	rm -f krc *.o

cfiles/clean:
	cd cfiles; ${MAKE} ${MFLAGS} clean
