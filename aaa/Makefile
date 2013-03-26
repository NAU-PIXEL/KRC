# Makefile for KRC distribution  2009may12

# Set up some shell-level specific variables
SHELL=/bin/csh

FC=g77 -pipe
FFLAGS= -O  -fno-automatic -fno-second-underscore -fargument-alias   # -g -C -xld 
LD=gcc

SYSLIBS =  -lg2c -lc -lm

#  library groups
HHKLIB=/home/hkieffer/cop/Dist/src  # top of libraries to use
LIBDIRS=-L$(HHKLIB)
#HLIB=-lkrc_f
CLIB=-lkrc_c

# system dependencies
OBJECTS1 = krc.o  tseas.o tyearp.o tlats.o tday.o tcard.o tprint.o tdisk.o  \
   epred.o porb0.o tint.o albvar.o co2pt.o vlpres.o aveday.o porb.o orbit.o \
           eccanom.o alsubs.o deding2.o seasalb.o seastau.o readtxt360.o \
           evmono3.o  \
	  binf5.o catime.o datime.o rndex.o rotmsp.o rterp.o sigma.o \
	vaddsp.o white1.o xtreme.o


# normal link
krc: $(OBJECTS1)     # -xl required for record length to be in words
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJECTS1) \
	$(CLIB) $(SYSLIBS)

# routines with dependencies
krc.o: krc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 
tseas.o: tseas.f krccom.inc units.inc 
tlats.o: tlats.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc
tday.o: tday.f krccom.inc daycom.inc hatcom.inc units.inc
tyearp.o: tyearp.f krccom.inc latcom.inc hatcom.inc
seasalb.o: seasalb.f units.inc filcom.inc
seastau.o: seastau.f units.inc filcom.inc
tcard.o: tcard.f krccom.inc latcom.inc daycom.inc units.inc # titcom.inc
tprint.o: tprint.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc # titcom.inc
tdisk.o: tdisk.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc filcom.inc
porb1.o: porb1.f units.inc porbcm.inc

# no additional dependancies
albvar.o: albvar.f
alsubs.o: alsubs.f
aveday.o: aveday.f 
binf5.o: binf5.F
co2pt.o: co2pt.f
deding2.o: deding2.f
eccanom.o: eccanom.f
evmono3.o: evmono3.f
koft.o: koft.f
orbit.o: orbit.f
porb.o:  porb.f
porb0.o: porb0.f
readtxt360.o: readtxt360.f
tint.o:  tint.f
tyearp.o: tyearp.f
vlpres.o: vlpres.f

# could be put in a library
catime.o: catime.f
datime.o: datime.f
rndex.o: rndex.f
rotmsp.o: rotmsp.f
rterp.o: rterp.f
sigma.o: sigma.f
vaddsp.o: vaddsp.f
white1.o: white1.f
xtreme.o: xtreme.f