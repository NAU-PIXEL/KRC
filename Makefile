# Makefile for KRC double precision version
# Attempt to have minimum differences between versions used for development and
# for formal distributions
# 2014mar11  Hugh Kieffer Derive from single precision version
# 2014jun19  HK Incorporate Version 3 PORB
# 2016feb13:mar23  HK  Version 33 commons
# 2016may12  HK  Incorporate V3.4 routines and testing programs, update common 
#      names. Move notes on unused options to  krc/aaa/makeNotes
# 2016may26 Kenny Rios method to handle development vrs production environments 
# 2016jun02 Comment out Rios distribution items
# 2016aug20 Commented options for debugger
# 2017feb23 HK Clarify differences between H3 and Distribution builds 
# lines containing  #<>H should be active if and only if on Hughs computer
# lines containing  #<>D should be active if and only if building a Distribution
# 2017mar15 HK include eclipse routines
# 2017apr29 HK Add krcdb as target with for debug options
# 2017sep30 HK Include  $(KRCLIB)/nowhite.f  to clean-up errorlog filename
# 2018feb01 HK include $(KRCLIB)/orlint.f
# 2018oct14 HK Include $(KRCLIB)/wraper.f
# 2019dec04 HK Change from $(KRCLIB)/wraper.f to  $(KRCLIB)/wraper8.f
####################################################
.SUFFIXES:
COMPILE.f = $(FC) $(FFLAGS) -c 
COMPILE.c = $(CC)  -c 


SHELL=/bin/bash
RM=/bin/rm -f

FC=gfortran

# gcc version is used to decide compiler flags
GCC_VERSION=$(shell $(FC) -dumpversion)
UNAME:=$(shell uname)

BINDIR := bin
OBJDIR := bin/obj

$(shell mkdir -p $(OBJDIR))

ifeq ($(UNAME), Linux)
# 8 needs some special flags
	ifeq ($(GCC_VERSION), 8)
		FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-comments -ffixed-line-length-none  -Wall -cpp

# All of the default GCC versions included with LTS systems work with these flags
	else
		FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-comments -fallow-argument-mismatch -ffixed-line-length-none  -Wall -cpp
	endif
endif

ifeq ($(UNAME), Darwin)
# Github actions provides gfortran 13, 14, and 15 in all MacOS images through Homebrew, but not a unified gfortran link.
# We assume that users will have a general gfortran link set up, so this next assignment should only occur on the Action runner.
	ifeq (, $(shell which gfortran))
	FC=gfortran-13
	endif
	FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-comments 		-fallow-argument-mismatch -ffixed-line-length-none  -Wall -cpp

endif
# Use 2nd version below to allow debugger and enable most IDBG actions
#FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-code  -fbounds-check # -Wall   #  -O

# next line for fortrancallgraph-master
#FFLAGS= -fno-automatic -fno-second-underscore -fd-lines-as-comments -Wall -S -g -O0

LD=gfortran
#------------- Libraries -------------------------
# Pointers to top of libraries to use. 
#<>D  Only for distribution
KRCLIB=./src/

#SYSLIBS = -L /usr/lib64  -lg2c -lgfortran -lc -lm    ## ? lgfortran 
 SYSLIBS = -L -lgfortran -lc -lm 

# LDFLAGS= -fdump-tree-slim  # used, but does nothing 2014mar11
LDBFLAGS= -g -fbounds-check -fprofile-arcs -ftest-coverage -pg -O0 -lgcov --coverage # prepare for debugger. Used only by krcdb

# Library directories that always are searched
LIBDIRS=-L$(KRCLIB)              #<>D

# FMODSRC := 
FMODDIR := $(KRCLIB)/module/fortran
FMODSOURCES := $(wildcard $(FMODDIR)/*.f)
FMODOBJS := $(subst .f,.o,$(FMODSOURCES))
MODOBJDIR := $(OBJDIR)/module
FFLAGS += -J$(MODOBJDIR)
$(shell mkdir -p $(MODOBJDIR))

CMOD_CC=gcc -pipe -O0  -Wall -fPIC -g -std=gnu17

CMODDIR := $(KRCLIB)/module/c
CMODSOURCES := $(wildcard $(CMODDIR)/*.c)
CMODHEADERS := $(wildcard $(CMODDIR)/*.h)
CMODOBJS := \
    $(patsubst $(CMODDIR)/%.c,$(OBJDIR)/%.o,$(CMODSOURCES))

$(OBJDIR)/%.o: $(CMODDIR)/%.c $(CMODHEADERS)
	@mkdir -p $(dir $@)
	$(CMOD_CC) -g -c $< -o $@

# ifneq ($(FMODDIR),)
# 	$(shell test -d $(FMODDIR) || mkdir $(FMODDIR))
# 	MODOUT=-J$(FMODDIR)
# 	FFLAGS+= $(MODOUT)
# endif

$(MODOBJDIR)/%.o: $(FMODDIR)/%.f
	$(COMPILE.f) -c $< -o $@

$(OBJDIR)/%.o: $(KRCLIB)/%.f
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -o $@
#  Special Kieffer library groups
#HLIB=-lhk_fmath -lhk_fgeom -lhk_futil -lhk_fchar  ##2-lhk_fNumRec # -lhk_rad


.PHONY : call cclean clean cleanall cleanmods
#
# Make clean
#
clean:
	- $(RM) $(KRCLIB)/*.o 
cleanbin: 
	- $(RM) krc
	- $(RM) porb

cleandocs:
	-unalias rm; mkdir doc_build; cd doc_build; rm -f *; cd ../doc_output; rm -f *.pdf

cleanmods:
	$(RM) -r $(OBJDIR)

cleanall: cclean clean cleanbin cleanmods

#------------- system dependencies -------------

OBJ8 = $(OBJDIR)/krc8.o $(OBJDIR)/tseas8.o $(OBJDIR)/tlats8.o $(OBJDIR)/tday8.o $(OBJDIR)/tcard8.o $(OBJDIR)/tprint8.o $(OBJDIR)/tdisk8.o $(OBJDIR)/tun8.o \
 $(OBJDIR)/epred8.o $(OBJDIR)/tint8.o $(OBJDIR)/albvar8.o $(OBJDIR)/vlpres.o $(OBJDIR)/porb08.o $(OBJDIR)/porbit.o $(OBJDIR)/orbit8.o $(OBJDIR)/eccanom8.o \
 $(OBJDIR)/deding28.o $(OBJDIR)/seasalb.o $(OBJDIR)/seastau.o $(OBJDIR)/readtxt360.o $(OBJDIR)/finterp.o \
 $(OBJDIR)/climtau.o $(OBJDIR)/binf5.o $(OBJDIR)/bigend.o $(OBJDIR)/rotmdp8.o $(OBJDIR)/vadddp8.o $(OBJDIR)/cocodp8.o $(OBJDIR)/readzone.o \
 $(OBJDIR)/catime.o $(OBJDIR)/white1.o $(OBJDIR)/ksubs8.o $(OBJDIR)/tfar8.o $(OBJDIR)/cubuterp8.o $(OBJDIR)/sigma8.o $(OBJDIR)/fillmv.o \
 $(OBJDIR)/eclipse.o $(OBJDIR)/tfine8.o $(OBJDIR)/dspline.o $(OBJDIR)/dsplint.o $(OBJDIR)/evmono3d.o $(OBJDIR)/strumi.o $(OBJDIR)/strumr8.o $(OBJDIR)/gaspt8.o \
 $(OBJDIR)/orlint8.o $(OBJDIR)/wraper8.o
# replace  nowhite  with code in krc

# PORB double precision
OBJP3 = $(OBJDIR)/porbmn.o $(OBJDIR)/porbio.o $(OBJDIR)/ephemr.o $(OBJDIR)/ymd2j2.o $(OBJDIR)/porbig.o $(OBJDIR)/porbit.o $(OBJDIR)/porbel.o \
 $(OBJDIR)/orbit8.o $(OBJDIR)/spcrev.o $(OBJDIR)/caldate.o $(OBJDIR)/caldat.o $(OBJDIR)/julday.o $(OBJDIR)/upcase.o $(OBJDIR)/eccanom8.o \
 $(OBJDIR)/catime.o $(OBJDIR)/prtpcom.o $(OBJDIR)/rotmdp8.o $(OBJDIR)/cocodp8.o $(OBJDIR)/vadddp8.o

$(BINDIR)/krc: $(OBJ8) call $(CMODOBJS)
	@mkdir -p $(BINDIR)
	$(FC) -o $@ $(OBJ8) \
	    $(CISISLIB) $(SYSLIBS) $(CMODOBJS)

$(BINDIR)/porbmn: $(OBJP3)
	@mkdir -p $(BINDIR)
	$(FC) -o $@ $(OBJP3) \
	    $(SYSLIBS)

# normal link
krc: $(BINDIR)/krc

porbmn: $(BINDIR)/porbmn
# testing and development

krcdb: $(OBJ8) call $(CMODOBJS)  # -  with debug
	$(FC) $(LDBFLAGS) $(LIBDIRS) -o $@ $(OBJ8) \
	$(CISISLIB) $(SYSLIBS) $(CMODOBJS) $(FDBFLAGS)

porbmndb: $(OBJP3)
	$(FC) $(LDBFLAGS) $(LIBDIRS) -o $@ $(OBJP3) \
	$(SYSLIBS) $(FDBFLAGS)

# make routines for program dependencies 
#
$(OBJDIR)/krc8.o:       $(KRCLIB)/krc8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f
$(OBJDIR)/tseas8.o:   $(KRCLIB)/tseas8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f          $(KRCLIB)/unic8m.f          $(KRCLIB)/hatc8m.f $(KRCLIB)/porbc8m.f
$(OBJDIR)/tlats8.o:   $(KRCLIB)/tlats8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f $(KRCLIB)/porbc8m.f $(CMODOBJS) $(MODOBJDIR)/array_structs.o
$(OBJDIR)/tday8.o:     $(KRCLIB)/tday8.f $(KRCLIB)/krcc8m.f          $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f $(KRCLIB)/porbc8m.f $(MODOBJDIR)/array_structs.o
$(OBJDIR)/tfine8.o:   $(KRCLIB)/tfine8.f $(KRCLIB)/krcc8m.f          $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f          $(KRCLIB)/hatc8m.f
$(OBJDIR)/tcard8.o:   $(KRCLIB)/tcard8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f
$(OBJDIR)/tprint8.o: $(KRCLIB)/tprint8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f 
$(OBJDIR)/tdisk8.o:   $(KRCLIB)/tdisk8.f $(KRCLIB)/krcc8m.f $(KRCLIB)/latc8m.f $(KRCLIB)/dayc8m.f $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f $(KRCLIB)/hatc8m.f
$(OBJDIR)/tfar8.o:     $(KRCLIB)/tfar8.f $(KRCLIB)/krcc8m.f                   $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f 
# above containss TFAREAD
$(OBJDIR)/tun8.o:       $(KRCLIB)/tun8.f $(KRCLIB)/krcc8m.f          $(KRCLIB)/dayc8m.f                   $(KRCLIB)/hatc8m.f
$(OBJDIR)/readzone.o: $(KRCLIB)/readzone.f $(KRCLIB)/krcc8m.f                 $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f
$(OBJDIR)/seasalb.o: $(KRCLIB)/seasalb.f                            $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f
$(OBJDIR)/seastau.o: $(KRCLIB)/seastau.f                            $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f
$(OBJDIR)/climtau.o: $(KRCLIB)/climtau.f                            $(KRCLIB)/unic8m.f $(KRCLIB)/filc8m.f
$(OBJDIR)/albvar8.o: $(KRCLIB)/albvar8.f $(KRCLIB)/krcc8m.f
$(OBJDIR)/gaspt8.o:   $(KRCLIB)/gaspt8.f $(KRCLIB)/krcc8m.f
$(OBJDIR)/tint8.o:     $(KRCLIB)/tint8.f $(KRCLIB)/krcc8m.f 
$(OBJDIR)/ephemr.o:   $(KRCLIB)/ephemr.f           $(KRCLIB)/porbc8m.f
$(OBJDIR)/porb08.o:   $(KRCLIB)/porb08.f           $(KRCLIB)/porbc8m.f        $(KRCLIB)/unic8m.f
$(OBJDIR)/porbig.o:   $(KRCLIB)/porbig.f           $(KRCLIB)/porbc8m.f
$(OBJDIR)/porbio.o:   $(KRCLIB)/porbio.f           $(KRCLIB)/porbc8m.f
$(OBJDIR)/porbit.o:   $(KRCLIB)/porbit.f           $(KRCLIB)/porbc8m.f  # this is DP   porbit4 is SP
$(OBJDIR)/prtpcom.o: $(KRCLIB)/prtpcom.f           $(KRCLIB)/porbc8m.f
$(OBJDIR)/wraper8.o: $(KRCLIB)/wraper8.f                            $(KRCLIB)/unic8m.f  # only if D lines
#------------------  do not have includes
$(OBJDIR)/bigend.o: $(KRCLIB)/bigend.f
$(OBJDIR)/binf5.o: $(KRCLIB)/binf5.f  # uses  B2B  BIGEND  CATIME WHITE1  PIO_system
$(OBJDIR)/cocodp8.o: $(KRCLIB)/cocodp8.f  # Contains: COCOCM  COCOMC  COCOSC  COCOCS  
#                       COCOSM  COCEMC  COCECM 
$(OBJDIR)/cubuterp8.o: $(KRCLIB)/cubuterp8.f
$(OBJDIR)/deding28.o: $(KRCLIB)/deding28.f
$(OBJDIR)/eccanom8.o: $(KRCLIB)/eccanom8.f
$(OBJDIR)/eclipse.o: $(KRCLIB)/eclipse.f 
$(OBJDIR)/epred8.o: $(KRCLIB)/epred8.f
$(OBJDIR)/evmono3d.o: $(KRCLIB)/evmono3d.f
$(OBJDIR)/finterp.o: $(KRCLIB)/finterp.f
$(OBJDIR)/julday.o:  $(KRCLIB)/julday.f 
$(OBJDIR)/orbit8.o: $(KRCLIB)/orbit8.f
$(OBJDIR)/porbel.o: $(KRCLIB)/porbel.f 
$(OBJDIR)/readtxt360.o: $(KRCLIB)/readtxt360.f
$(OBJDIR)/rotmdp8.o: $(KRCLIB)/rotmdp8.f  # Contains:  MEQUAL  MPROD3  ROTAX  ROTCOL  ROTDIA 
#                      ROTEST  ROTEXM  ROTEXV  ROTMAT  ROTORB  ROTRIP  
#                      ROTROW  ROTSHO  ROTV  ROTVEC  ROTZXM  TRANS3  VROTV
$(OBJDIR)/sigma8.o: $(KRCLIB)/sigma8.f   # used for debug of cubuterp
$(OBJDIR)/spcrev.o: $(KRCLIB)/spcrev.f 
$(OBJDIR)/st2real6.o: $(KRCLIB)/st2real6.f
$(OBJDIR)/strumi.o: $(KRCLIB)/strumi.f
$(OBJDIR)/strumr8.o: $(KRCLIB)/strumr8.f
$(OBJDIR)/tridag8.o: $(KRCLIB)/tridah8.f   # uses prior partial solution
$(OBJDIR)/vlpres.o: $(KRCLIB)/vlpres.f
$(OBJDIR)/vadddp8.o: $(KRCLIB)/vadddp8.f  # Contains:  VADD  VCROSS  VDOT  VEQUAL  VMAG  VNEG 
 #                       VNORM  VPRF  VPRINT  VSCALE  VSHOW  VSUB  VUNIT
$(OBJDIR)/ymd2j2.o: $(KRCLIB)/ymd2j2.f

#----------------------- added after remove use of all but C library
$(OBJDIR)/caldat.o: $(KRCLIB)/caldat.f
$(OBJDIR)/caldate.o: $(KRCLIB)/caldate.f
$(OBJDIR)/catime.o: $(KRCLIB)/catime.f
$(OBJDIR)/fillmv.o: $(KRCLIB)/fillmv.f  # has  FILLB FILLI FILLL FILLR FILLD MVB MVI MVL MVR MVD
#                     MVDF MVDM MVD21
$(OBJDIR)/ksubs8.o: $(KRCLIB)/ksubs8.f  # has  AVEDAY  AVEYEAR  CO2PT  SIGMA 
$(OBJDIR)/nowhite.o: $(KRCLIB)/nowhite.f
$(OBJDIR)/white1.o: $(KRCLIB)/white1.f
$(OBJDIR)/vec2code.o: $(KRCLIB)/vec2code.f

# ------------------used only for testing
$(OBJDIR)/bigend1.o: $(KRCLIB)/bigend1.f
catime:o /home/hkieffer/src/for/util/$(KRCLIB)/catime.f
$(OBJDIR)/climtau.o: $(KRCLIB)/climtau.f
$(OBJDIR)/deding2.o: $(KRCLIB)/deding2.f
$(OBJDIR)/dpythag.o: $(KRCLIB)/dpythag.f # - /home/hkieffer/src/for/NumRec/$(KRCLIB)/dpythag.f
$(OBJDIR)/dspline.o: $(KRCLIB)/dspline.f # -
$(OBJDIR)/dsplint.o: $(KRCLIB)/dsplint.f # -
$(OBJDIR)/dsvbksb.o: $(KRCLIB)/dsvbksb.f # -
$(OBJDIR)/dsvdcmp.o: $(KRCLIB)/dsvdcmp.f # calls dpythag
$(OBJDIR)/evrf4.o: $(KRCLIB)/evrf4.f
$(OBJDIR)/hratlsq.o: $(KRCLIB)/hratlsq.f # calls ratval,dsvbksb,dsvdcmp spline splint
$(OBJDIR)/kratlsq.o: $(KRCLIB)/kratlsq.f # calls ratval,dsvbksb,dsvdcmp dspline dsplint
$(OBJDIR)/m2eul.o: $(KRCLIB)/m2eul.f
$(OBJDIR)/qtlats.o: $(KRCLIB)/qtlats.f
$(OBJDIR)/ratval.o: $(KRCLIB)/ratval.f
$(OBJDIR)/spline.o: $(KRCLIB)/spline.f # -
$(OBJDIR)/splint.o: $(KRCLIB)/splint.f # -
$(OBJDIR)/test8.o: $(KRCLIB)/test8.f

### C Isis library make section
# Set up some shell-level specific variables
C_DBG=-g -C

HOST_ARCH ?= $(shell uname -s)
HOST_MACH  = $(shell uname -m)

#  Temporary defines for building in foreign systems
HHKOS      ?= $(HOST_MACH)
HHKINC     ?= $(CURDIR)

#  GNU C compiler and flags.
CC=gcc -pipe -O2  -Wall -fPIC -Dunix -D$(HOST_ARCH) -D$(HOST_MACH) -D$(HHKOS)

#  Special load flags.  These flags are utilized in all builds, whether
#  they are FORTRAN or C
CISIS_LDFLAGS=-shared

# Special library MACROS
# handles archives
AR=ar
# r=replace v=verbose s=include index
ARFLAGS=-rvs

#  Set up source dependancies
CISISSRC_DIR=$(KRCLIB)/cfiles/
CISISLIB=$(CISISSRC_DIR)/libhk_cisis.a
CISISSRCS = $(wildcard $(CISISSRC_DIR)*.c)
CISISOBJS = $(addsuffix .o, $(basename $(CISISSRCS)))

#  Define all required targets
call: $(CISISLIB)

# $(CMODOBJS): $(CMODDIR)/%.o: $(CMODDIR)/%.c $(CMODHEADERS)
#   $(CMOD_CC) -g -c $< -o $@

$(CISISOBJS): $(CISISSRC_DIR)%.o: $(CISISSRC_DIR)/%.c
	$(COMPILE.c) $(CISIS_LDFLAGS) -c $< -o $@

$(CISISLIB): $(CISISOBJS)
	$(AR) $(ARFLAGS) $(CISISLIB) $(CISISOBJS)

#  Clean up 
cclean: 
	- $(RM) $(CISISOBJS) $(CISISLIB)

### Documentation build section

DOCSRC = doc/

DOCOUT = $(CURDIR)/doc_output/

DOCBUILD = $(CURDIR)/doc_build/

UGDIR = $(DOCSRC)/user_guide/

TEXMKFLAGS = -pdf -pdflatex="pdflatex -interaction=nonstopmode"
TEXMKDEST = -outdir=$(DOCOUT) -auxdir=$(DOCBUILD) -emulate-aux-dir
export BIBINPUTS = $(CURDIR)/doc

.PHONY: docs

docs: doc_output/V34UG.pdf doc_output/slopes.pdf doc_output/eclipse.pdf doc_output/helplist.pdf doc_output/hporb.pdf doc_output/PUG.pdf

doc_output/V34UG.pdf: $(UGDIR)/V34UG.tex $(UGDIR)/farg.tex $(UGDIR)/fard.tex $(UGDIR)/v34p.tex
	cd $(UGDIR); \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	V34UG.tex 

doc_output/slopes.pdf: $(DOCSRC)/slopes/slopes.tex
	cd $(DOCSRC)/slopes; \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	slopes.tex

doc_output/eclipse.pdf: $(DOCSRC)/eclipse/eclipse.tex
	cd $(DOCSRC)/eclipse; \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	eclipse.tex

doc_output/helplist.pdf: $(DOCSRC)/helplist/helplist.tex
	cd $(DOCSRC)/helplist; \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	helplist.tex

doc_output/hporb.pdf: $(DOCSRC)/hporb/hporb.tex
	cd $(DOCSRC)/hporb; \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	hporb.tex

doc_output/PUG.pdf: $(DOCSRC)/PUG/PUG.tex
	cd $(DOCSRC)/PUG; \
	latexmk $(TEXMKFLAGS) $(TEXMKDEST) \
	PUG.tex

