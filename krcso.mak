# Makefile for KRC, KRCC1, META, YEAR programs
# Modified to linux compatibility  98may26
include $(HHKMAKE)/hhk_system.rules

NRLIB = -L/usr/local/lib -lsys_nrf

IDLLIBS=-lg2c -lm -lc
LDIDLOPTS= -shared
SYSLIBS=$(IDLLIBS)
OPTFLAG = -g
LDOPTS = -g

OPT_CFLAGS=$(CCPICOPT)
OPT_FFLAGS=$(FCPICOPT)

# binding routines in this directory and others without path

#---------------------------------------------
#? ELIB2= -L/usr/local/lib -lsys_fieee  # for chkmath
# set search path for source files

# system dependencies

OBJECTS2 = idlkrc.o krcw.o tseas.o tlats.o tday.o tcard.o tprint.o tdisk.o \
	epred.o porb0.o tint.o albvar.o co2pt.o vlpres.o aveday.o porb.o  \
        orbit.o eccanom.o alsubs.o deding2.o

# Shared Library for KRC
krcso: $(OBJECTS2)     # -xl required for record length to be in words
	$(CC) $(LDFLAGS) $(LDIDLOPTS) $(LIBDIRS) -o krc.so $(OBJECTS2) \
	$(HLIB) $(CLIB) $(SYSLIBS)

# make routines for program dependencies
#
idlkrc.o: krc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 

krc.o: krc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 
test.o: test.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc
tseas.o: tseas.f krccom.inc units.inc 
tlats.o: tlats.f krccom.inc latcom.inc daycom.inc  units.inc
tday.o: tday.f krccom.inc daycom.inc units.inc

tcard.o: tcard.f krccom.inc latcom.inc daycom.inc units.inc

tprint.o: tprint.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc

tdisk.o: tdisk.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc

porb1.o: porb1.f units.inc $(HOME)/krc/porb/porbcm.inc

albvar.o: albvar.f
alsubs.o: alsubs.f
aveday.o: aveday.f
co2pt.o: co2pt.f
deding2.o: deding2.f
eccanom.o: eccanom.f
orbit.o: orbit.f
porb.o: porb.f
porb0.o: porb0.f
tint.o: tint.f
vlpres.o: vlpres.f

#
# Make clean
#
clean:
	-unalias rm; rm -f *.o
realclean:
	-rm krc
	-rm *.o 
