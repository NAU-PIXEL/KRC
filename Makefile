# Makefile for exported KRC   2003sep02
##########################################################################
# krc_system.rules
#
# Set up some shell-level specific variables
SHELL=/bin/csh
RM=/bin/rm -f

#
# Set up compiler flags
KRCDBG=-g
KRCOS=Linux
#
#  These are the GNU C compiler flags
CC_WARN=-Wall
CC=gcc -pipe
CCKLUDGE=gcc -ansi
CCOPTS=
OPT_CFLAGS=
EXTRA_CFLAGS=-Dunix -D$(KRCOS)
CCPICOPT=-fPIC

#
#  These are the FORTRAN compiler flags
FC_KRCOP=-O             #  Optimize
FC_KRCXL=-fvxt         #  Linux extended support 
FC_KRCDL=               #  D_LINES option
FC_KRCDBG=-g
FC_WARN=
FC=g77
FCOPTS=$(FC_KRCXL)
#OPT_FFLAGS=-O2
OPT_FFLAGS=
EXTRA_FFLAGS=-ftypeless-boz -fno-automatic -fno-second-underscore \
             -fargument-alias -Dunix -D$(KRCOS)
FCPICOPT=-fPIC


#  Special load flags.  These flags are utilized in all builds, whether
#  they are FORTRAN or C
LD=gcc
LDOPTS=-g
LDSHAREDOPTS=-shared -Wl,--whole-archive
SHARELIBS=
LDFLAGS=$(LDOPTS)

# Special library MACROS
AR=ar
ARFLAGS=-rvs
RANLIB=/bin/echo


#  Set up operating system dependant libraries.  EXTRALIBS is for
#  any libraries that may need to be linked prior to SYSLIBS
#  OPTLIBS is intended to be used for libraries that are needed for
#  other versions of compilers
KRCLIBDIRS=
USRLIBDIRS=
EXTRALIBDIRS=

EXTRALIBS=
# CURSESLIB=-lncurses
# TERMLIB=-ltermcap
CURSESLIB=
TERMLIB=
SYSLIBS = $(EXTRALIBS) $(CURSESLIB) $(TERMLIB) -lg2c -lc -lm

#  Set up X and Motif root paths
XROOT=/usr/X11
MOTIFROOT=/usr/local

# Include files paths
INCLUDES=-I.

# Library directores that always are searched in
LIBDIRS=-L. $(USRLIBDIRS) $(EXTRALIBDIRS)

# Specify compilation and build flags (default: None)
OPTFLAG=-g
CFLAGS=$(CCOPTS) $(OPTFLAG)
FFLAGS=$(FCOPTS) $(OPTFLAG)


#  Set up suffixes and special ISIS targets for those suffixes
.SUFFIXES: .o .c .F .f
.c.o: 
	$(CC) $(CC_WARN) $(OPT_CFLAGS) $(CFLAGS) $(EXTRA_CFLAGS) $(INCLUDES) -c $<

.F.o: 
	$(FC) $(FC_WARN) $(OPT_FFLAGS) $(FFLAGS) $(EXTRA_FFLAGS) $(INCLUDES) -c $<
.f.o: 
	$(FC) $(FC_WARN) $(OPT_FFLAGS) $(FFLAGS) $(EXTRA_FFLAGS) $(INCLUDES) -c $<
##############################___end_of_system_rules_##############


F77=g77
FC=$(F77)


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
OBJUTIL = catime.o datime.o idarch.o vaddsp.o xtreme.o binf5.o white1.o
OBJC = b2b.o r2r.o u_move1.o u_move4.o u_swapn.o primio.o pio_bind_c.o  \
	binf5_bind.o b_alloc.o b_c2fstr.o b_f2cstr.o b_free.o
OBJS =  $(OBJC) $(OBJUTIL) $(OBJECTS1) 
# normal link
krc: $(OBJS) # -xl required for record length to be in words
	$(FC) $(LDFLAGS) $(LIBDIRS) -o $@ $(OBJS) \
	$(HLIB) $(CLIB) $(SYSLIBS)
#
# make routines for program dependencies
#
krc.o: krc.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc

tseas.o: tseas.f krccom.inc units.inc 

tlats.o: tlats.f krccom.inc latcom.inc daycom.inc  units.inc

tday.o: tday.f krccom.inc daycom.inc units.inc

tcard.o: tcard.f krccom.inc latcom.inc daycom.inc units.inc

tprint.o: tprint.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc

tdisk.o: tdisk.f krccom.inc latcom.inc daycom.inc units.inc filcom.inc

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



# utilities
r2r.o: r2r.c
#datime.o: datime.f
bin5f.o: bin5f.F
primio.o: primio.c primio.h
#rotate.o: rotmsp.f  # many routines in one file
# Make clean
#
clean:
	-unalias rm; rm -f *.o
realclean:
	-rm krc
	-rm *.o 
