
### IDL module make section
# Makefile for IDL externals for KRC users
#_Hist 2014feb28 HK Derive from Hugh's idl/externals/Makefile
# Tried  -m32 on IDLCFLAGS IDLFFLAGS and LDFAGS; this caused errors
# 2014may05 -lg2c >> -lgfortran
#########################################################################

# Set up some shell-level specific variables
SHELL=/bin/bash 

#  These are the GNU C compiler flags.
FC=gfortran
IDLCC= gcc -pipe  
IDLCFLAGS= -fPIC -Wall
#  These are the FORTRAN compiler flags
IDLFFLAGS= -fno-automatic -fno-second-underscore -fargument-alias -fd-lines-as-comments -fallow-argument-mismatch -fPIC 

#  Special load flags, utilized in all builds, whether FORTRAN or C
LD=gcc
IDLLDFLAGS= -shared -fPIC # -Wall -Wl
# whole archive only needed if refer to .a libs

# Special library MACROS for .a libraries  There are none built here
# AR=ar   # archinve.. moves .o into .a    No harm 
# ARFLAGS=-rvs
# RANLIB=/bin/echo  # nedded for .a lib

#------------- Libraries and paths-------------------------
# Include files paths
INCLUDES=-I.   # in this directory

# L are Library directores that always are searched in
# l are libraries to include  -lc==libc.a  etc. 
LIBDIRS=-L.  # -L/home/hkieffer/linux/lib  #<<< last is for NumRec
#SYSLIBS = -lg2c -lc -lm   commented 2014may05
SYSLIBS = -lgfortran -lc -lm  

#------------------- target dependencies -------------------
IDLOBJDIR = ./objects
IDLSRCDIR = ./extern

EXCW = exfuncw.c exfunctionw.c exroutinew.c

# Find C and Fortran sources and make object targets
IDLSRCW = $(shell find $(IDLSRCDIR) -type f -name "*.c")
IDLSRCW_FILTER = $(filter-out $(addprefix $(IDLSRCDIR)/,$(EXCW)), $(IDLSRCW))
IDLOBJSW = $(IDLSRCW_FILTER:.c=.o)

IDLSRCF = $(shell find $(IDLSRCDIR) -type f -name "*.f")
IDLOBJSF = $(IDLSRCF:.f=.o)

IDLOBJSALL= $(IDLOBJSF) $(IDLOBJSW) # concatonate objects

# Pattern rules to match files in source directory with object targets
# Not needed for KRC C files 
$(IDLOBJSW): %.o: %.c
	$(IDLCC) $(IDLCFLAGS) $(INCLUDES) -c $< -o $@

$(IDLOBJSF): %.o: %.f
	$(FC) $(IDLFFLAGS) $(INCLUDES) -c $< -o $@

%.o : $(IDLSRCDIR)/%.c 
	$(IDLCC) $(IDLCFLAGS) $(INCLUDES) -c $<
%.o : $(IDLSRCDIR)/%.f
	$(FC) $(IDLFFLAGS) $(INCLUDES) -c $<
#------------- Actions -----------------------------

# machines running in 64-bit mode, IDL must be in same mode
ftnwrap64.so:	$(IDLOBJSALL)    
	$(LD) $(IDLLDFLAGS)   -o $(IDLSRCDIR)/$@ $(IDLOBJSALL) $(LIBDIRS) \
	$(SYSLIBS)
#	 -lhk_fNumRec $(SYSLIBS)

clean:
	-unalias rm; rm -f extern/*.o extern/ftnwrap64.so