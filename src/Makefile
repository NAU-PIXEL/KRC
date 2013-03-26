# Makefile for KRC   2008mar31

HHKOS=Linux_ix86                 # target operating system
HHKLIB=/home/hkieffer/linux/lib  # top of libraries to use

# Set up some shell-level specific variables
SHELL=/bin/csh
#RM=/bin/rm -f  # needed for 'clean'

FC=g77 -pipe
FFLAGS= -O  -fno-automatic -fno-second-underscore -fargument-alias   # -g -C -xld 
LD=gcc

SYSLIBS =  -lg2c -lc -lm

# Library directories that always are searched in
LIBDIRS=-L$(HHKLIB)

#  Special Kieffer library groups
HLIB=-lhk_fmath -lhk_fgeom -lhk_futil -lhk_fchar -lhk_fNumRec # -lhk_rad
CLIB=-lhk_cisis			# -lhk_crest


#---------------------------------------------
#? ELIB2= -L/usr/local/lib -lsys_fieee  # for chkmath
# set search path for source files

# system dependencies
OBJECTS1 = krc.o tyearp.o tlats.o tday.o tcard.o tprint.o tdisk.o epred.o \
           porb0.o tint.o albvar.o co2pt.o vlpres.o aveday.o porb.o orbit.o \
           eccanom.o alsubs.o deding2.o seasalb.o seastau.o readtxt360.o \
           evmono3.o  tseas.o # blocktit.o

OBJD1 = krc.d tseas.d tlats.d tday.d tcard.d tprint.d tdisk.d epred.d \
           porb0.d tint.d albvar.d co2pt.d vlpres.d aveday.d porb.d orbit.d \
           eccanom.d alsubs.d deding2.d

OBJECTS2 = idlkrc.o tseas.o tlats.o tday.o tcard.o tprint.o tdisk.o epred.o \
           porb0.o tint.o albvar.o co2pt.o vlpres.o aveday.o porb.o orbit.o \
           eccanom.o alsubs.o deding2.o

OBJECTS3 = idummy.o idlkrc.o tseas.o tlats.o tday.o tcard.o tprint.o tdisk.o epred.o \
           porb0.o tint.o albvar.o co2pt.o vlpres.o aveday.o porb.o orbit.o \
           eccanom.o alsubs.o deding2.o

OBJECTS4 = testf.o koft.o evmono3.o

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

testf: $(OBJECTS4)
	$(FC)  -o testf  $(OBJECTS4)

testr: testr.o 
	$(F77)  -o testr  testr.o  -L$(HOME)/lib  -lhk_futil

test: test.o vlpres.o 
	$(FC) -xl -o test  test.o vlpres.o  -L$(HOME)/lib -lhk_futil

#
# make routines for program dependencies
#
krc.o: krc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 
idlkrc.o: idlkrc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc 

idummy.o: idummy.f

testf.o: testf.f 
koft.o: koft.f
evmono3.o: evmono3.f

test.o: test.fkrccom.inc latcom.inc daycom.inc units.inc filcom.inc
tseas.o: tseas.f krccom.inc units.inc 
tlats.o: tlats.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc
tday.o: tday.f krccom.inc daycom.inc hatcom.inc units.inc
tyearp.o: tyearp.f krccom.inc latcom.inc hatcom.inc
seasalb.o: seasalb.f units.inc filcom.inc
readtxt360.o: readtxt360.f
# blocktit.o: blocktit.f  titcom.inc
tcard.o: tcard.f krccom.inc latcom.inc daycom.inc units.inc # titcom.inc
tprint.o: tprint.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc # titcom.inc
tdisk.o: tdisk.f krccom.inc latcom.inc daycom.inc hatcom.inc units.inc filcom.inc

porb1.o: porb1.f units.inc porbcm.inc

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
	-rm -f *.o 
