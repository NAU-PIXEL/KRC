# Makefile for KRC Version 3  distribution  2014jun19
# 2014jun19 HK Incorporate Version 3 PORB

SHELL=/bin/csh

FC=gfortran

FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-comments  # -Wall -fbounds-check  -O  

# -g [gcc Debug] Produce debugging information in the op. system's native format 
# -O [gcc Optimize] Try to reduce code size and execution time. First stage
# -fbounds-check      [gcc Code]  Run-time checks for array subscripts 
# -Wall enables -Wunused-labels, -Waliasing, -Wampersand, -Wsurprising, -Wnonstd-intrinsic, and -Wline-truncation

LD=gfortran
#------------- Libraries -------------------------
SYSLIBS = -L  -lgfortran -lc -lm 

# LDFLAGS= -fdump-tree-slim  # used, but does nothing 2014mar11

# Library directories that always are searched in
HHKLIB=/mars/u/krios/krc/builds/3.2.1/  # top of libraries to use
LIBDIRS=-L$(HHKLIB)
CLIB=./cfiles/libhk_cisis.a

#------------- system dependencies -------------

OBJ8 = krc8.o tseas8.o tlats8.o tday8.o tcard8.o tprint8.o tdisk8.o tun8.o \
 epred8.o porb08.o tint8.o albvar8.o vlpres.o porbit.o orbit8.o eccanom8.o  \
 alsubs.o deding28.o seasalb.o seastau.o readtxt360.o finterp.o evmono38.o \
 climtau.o binf5.o bigend.o rotmdp8.o vadddp8.o cocodp8.o \
 b2b.o catime.o white1.o ksubs8.o

# ksubs8 has  aveday  aveyear  co2pt  sigma

OBJP3 = porbmn.o porbio.o ephemr.o ymd2j2.o porbig.o porbit.o porbel.o orbit8.o spcrev.o caldate.o caldat.o julday.o b2b.o upcase.o eccanom8.o catime.o prtpcom.o rotmdp8.o cocodp8.o vadddp8.o

# normal link

krcd: $(OBJ8) 
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJ8) \
	$(CLIB) $(SYSLIBS)

krcn: $(OBJN) 
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJN) \
	$(CLIB) $(SYSLIBS)

porbmn: $(OBJP3)
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJP3) \
	$(SYSLIBS)

# make routines for program dependencies
#
krc8.o:       krc8.f krcc8m.f latc8m.f dayc8m.f units.inc filcom.inc 
tseas8.o:   tseas8.f krcc8m.f latc8m.f          units.inc hatc8m.f 
tlats8.o:   tlats8.f krcc8m.f latc8m.f dayc8m.f units.inc hatc8m.f porbc8m.f
tday8.o:     tday8.f krcc8m.f          dayc8m.f units.inc hatc8m.f
tdayCN8.o: tdayCN8.f krcc8m.f          dayc8m.f units.inc hatc8m.f
tcard8.o:   tcard8.f krcc8m.f latc8m.f dayc8m.f units.inc filcom.inc 
tprint8.o: tprint8.f krcc8m.f latc8m.f dayc8m.f units.inc filcom.inc 
tdisk8.o:   tdisk8.f krcc8m.f latc8m.f dayc8m.f units.inc filcom.inc hatc8m.f
tun8.o:       tun8.f krcc8m.f          dayc8m.f           hatc8m.f

seasalb.o: seasalb.f units.inc filcom.inc
seastau.o: seastau.f units.inc filcom.inc
climtau.o: climtau.f  units.inc filcom.inc
readtxt360.o: readtxt360.f

albvar8.o: albvar8.f krcc8m.f
alsubs.o: alsubs.f 
averag.o: averag.f  # test for function should be defined and called
aveyear.o: aveyear.f
bigend.o: bigend.f
binf5.o: binf5.F  # uses  B2B  BIGEND  CATIME WHITE1  PIO_ system
deding28.o: deding28.f
eccanom8.o: eccanom8.f
ephemr.o: ephemr.f  porbc8m.f
evmono38.o: evmono38.f
finterp.o: finterp.f
jdate.o:  jdate.f  
julday.o:  julday.f  
orbit8.o: orbit8.f
porb08.o: porb08.f units.inc porbc8m.f
porbel.o: porbel.f 
porbit.o: porbit.f   porbc8m.f
porbig.o: porbig.f   porbc8m.f
porbio.o: porbio.f   porbc8m.f
porbit.o: porbit.f   porbc8m.f
prtpcom.o: prtpcom.f  porbc8m.f
#porb1.o: porb1.f units.inc porbcom8.f
readaie.o: readaie.f
#r2i2s.o: r2i2s.f
spcrev.o: spcrev.f 
tint8.o: tint8.f krcc8m.f
tridag8.o: tridah8.f   # uses prior partial solution
vlpres.o: vlpres.f
white0.o: white0.f
cocodp8.o: cocodp8.f  # collection
rotmdp8.o: rotmdp8.f  # collection
vadddp8.o: vadddp8.f  # collection
ymd2j2.o: ymd2j2.f


#----------------------- added after remove use of all but C library

b2b.o: /mars/u/krios/krc/builds/3.2.1/src/b2b.f
caldat.o: caldat.f
caldate.o: caldate.f
catime.o: /mars/u/krios/krc/builds/3.2.1/src/catime.f
datime.o: datime.f
upcase.o: upcase.f
white1.o: /mars/u/krios/krc/builds/3.2.1/src/white1.f
vec2code.o: vec2code.f

ksubs8.o: ksubs8.f  # has  AVEDAY  AVEYEAR  CO2PT  SIGMA
#
# Make clean
#
clean:
	-unalias rm; rm -f *.o 
realclean:
	-rm krc
	-rm porb
	-rm -f *.o 
