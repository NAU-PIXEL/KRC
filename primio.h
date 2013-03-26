/********************************************************************
*_Title primio.h Prototype include file for I/O routines
*_Args NONE
*/

#ifndef PRIMIO_H
#define PRIMIO_H

extern int pio_in(int *fid, char flspec[], long *nbytes, int mode, int *ret);
extern int pio_cl(int fid, int idele, int *ret);
extern int pio_rd(int fid, long ibyte, long nbytes, void *buf, int *ret);
extern int  pio_wt(int fid, long ibyte, long nbytes, void *buf, int *ret);
extern int  pio_ap(int fid, long nbytes, long *ntbytes, int *ret);

#endif

/*
*_Hist Dec 02 1993 Kris Becker USGS, Flagstaff Original Version
*_End
*********************************************************************/
