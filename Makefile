# Makefile for KRC, KRCC1, META, YEAR programs
# Modified to linux compatibility  98may26

NRLIB = -L/usr/local/lib -lsys_nrf
# Compiler and Linker variables on Sun systems (SunSoft)

#SYSLIBS=-lg2c -lm -lc
#$OPTFLAG = -g
#LDOPTS = -g

#OPT_CFLAGS=$(CCPICOPT)
#OPT_FFLAGS=$(FCPICOPT)


#---------------------------------------------
#? ELIB2= -L/usr/local/lib -lsys_fieee  # for chkmath
# set search path for source files

# system dependencies
OBJECTS1 = krc.o tseas.o tlats.o tday.o tcard.o tprint.o tdisk.o epred.o \
           porb0.o tint.o albvar.o co2pt.o vlpres.o aveday.o porb.o orbit.o \
           eccanom.o alsubs.o deding2.o

OBJD1 = krc.d tseas.d tlats.d tday.d tcard.d tprint.d tdisk.d epred.d \
           porb0.d tint.d albvar.d co2pt.d vlpres.d aveday.d porb.d orbit.d \
           eccanom.d alsubs.d deding2.d

OBJECTS2 = idlkrc.o tseas.o tlats.o tday.o tcard.o tprint.o tdisk.o epred.o \
           porb0.o tint.o albvar.o co2pt.o vlpres.o aveday.o porb.o orbit.o \
           eccanom.o alsubs.o deding2.o

OBJECTS3 = idummy.o idlkrc.o tseas.o tlats.o tday.o tcard.o tprint.o tdisk.o epred.o \
           porb0.o tint.o albvar.o co2pt.o vlpres.o aveday.o porb.o orbit.o \
           eccanom.o alsubs.o deding2.o

# normal link
krc: $(OBJECTS1)     # -xl required for record length to be in words
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJECTS1) \
	$(HLIB) $(CLIB) $(SYSLIBS)

# IDL driver link
krci: $(OBJECTS2)     # -xl required for record length to be in words
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJECTS2) \
	$(HLIB) $(CLIB) $(SYSLIBS)

# Dummy driver link
krcd: $(OBJECTS3)     # -xl required for record length to be in words
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJECTS3) \
	$(HLIB) $(CLIB) $(SYSLIBS)

# to invoke ISIS traps
krcf: $(OBJECTS1)     # -xl required for record length to be in words
	$(F77) -o krc $(OBJECTS1) $(HLIB) -L$(ISISLIB) -lisis -lisisgen -lbind



testr: testr.o 
	$(F77)  -o testr  testr.o  -L$(HOME)/lib  -lhk_futil

test: test.o vlpres.o 
	f77 -xl -o test  test.o vlpres.o  -L$(HOME)/lib -lhk_futil

#
# make routines for program dependencies
#
krc.o: krc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 
idlkrc.o: idlkrc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 

idummy.o: idummy.f

test.o: test.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc
tseas.o: tseas.f krccom.inc units.inc 
tlats.o: tlats.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc
tday.o: tday.f krccom.inc daycom.inc hatcom.inc units.inc

tcard.o: tcard.f krccom.inc latcom.inc daycom.inc units.inc
tprint.o: tprint.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc
tdisk.o: tdisk.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc filcom.inc

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

testt.o: testr.f
#
# Make clean
#
clean:
	-unalias rm; rm -f *.o
realclean:
	-rm krc
	-rm *.o 
