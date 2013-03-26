#include <stdio.h>
#include <string.h>
#include "primio.h"

#if defined(VMS)
#include <descrip.h>
#define FTN_NAME(a)          a
#define CC_NAME(a)           z##a
#else
#define FTN_NAME(a)          a##_
#define CC_NAME(a)            a
#endif


#if defined(VMS)
void FTN_NAME(pio_in) (int *fid, struct dcs$descriptor *vms_string, 
                       long *nbytes, long *mode, long *ret)
#else
void FTN_NAME(pio_in) (int *fid, char *flspec, long *nbytes, long *mode, 
                       long *ret, int flspec_len)
#endif
{
   char fname[260];
   register int i;
   int ret_int;

#if defined(VMS)
   int flspec_len = vms_string->dcs$w_length;
   char *flspec =   vms_string->dcs$a_pointer;
#endif

   for (i = flspec_len-1 ; i >= 0 ; i--)
     if (flspec[i] != ' ') break;

   (void) strncpy(fname, flspec, i+1);
   fname[i+1] = '\0';

   (void) CC_NAME(pio_in)(fid, fname, nbytes, *mode, &ret_int);
   *ret = ret_int;
   return;
}



void FTN_NAME(pio_cl)(int *fid, int *idele, int *ret)
{

  (void) CC_NAME(pio_cl)(*fid, *idele, ret);
  return;
}



void FTN_NAME(pio_rd)(int *fid, long *ibyte, long *nbytes, void *buf, int *ret)
{

  (void) CC_NAME(pio_rd)(*fid, *ibyte, *nbytes, buf, ret);
  return;
}



void FTN_NAME(pio_wt)(int *fid, long *ibyte, long *nbytes, void *buf, int *ret)
{

  (void) CC_NAME(pio_wt)(*fid, *ibyte, *nbytes, buf, ret);
  return;
}



void FTN_NAME(pio_ap)(int *fid, long *nbytes, long *ntbytes, int *ret)
{

  (void) CC_NAME(pio_ap)(*fid, *nbytes, ntbytes, ret);
  return;
}
