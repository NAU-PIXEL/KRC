/* Unix system include files */
#include	<stdio.h>
#include	<string.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include        <fcntl.h>
#include        <unistd.h>
#include        <stdlib.h>
#include        <errno.h>
#include        "primio.h"

#define FSTAT_AVAIL_OS
#define FTRUNCATE_AVAIL_OS
#define UNLINK_AVAIL_OS
#if defined(UNLINK_AVAIL_OS) && defined(VAX)
#define unlink     delete
#endif

#define MAX(a,b)         (((a) > (b)) ? (a) : (b))

/**********************************************************************
  This is the basic structure that contains information about each file
  opened by the primio file package.
***********************************************************************/
typedef struct pio_fcb {
   int  fid;			/* File descriptor */
   int  access;			/* Access mode of file */
   int  mode;			/* I/O mode for the file */
   int  file_handle;		/* Handle for file */
   char filnam[256];		/* Name of file */
   long nbytes;			/* Number of bytes in file */
   long tbytes;			/* Total bytes in file */
   int  disposition;		/* Status of file */
  } PRIMIO_FCB;


/* Some constants */
#define		FCB_LEN	  			512
#define         MAX_FILES  	 		 20
#define         NO_FCB          (PRIMIO_FCB *) NULL
#define         TRUE              		  1
#define         FALSE             		  0
#define         SAVE              		  0
#define         KEEP              		  0
#define         DELETE            		  1
#define         CREATE            		  0
#define         READ_WRITE        		  1
#define         READ_ONLY         		  3
#define         TEMPORARY         		  4


/* These variables are the control blocks for PRIMIO package */
static PRIMIO_FCB *file_fcbs[MAX_FILES];	/* Pointers to file blocks */
static int fcb_init = FALSE;		/* Specifies initialization of arrays */


/* Declare internal functions here */
static PRIMIO_FCB *pio_get_fcb(int fid);

int pio_in(int *fid, char flspec[], long *nbytes, int mode, int *ret)
/****************************************************************************
*_Title pio_in Create a new file or open an existing file
*_Args   Type  Name        I/O      Description
*_Param  int   *fid;        O       File identifier of the created or opened
*                                   file.
*_Param  char  flspec[];    I       Name of the file to create or open.
*_Param  long  *nbytes;     B       For access mode CREATE or TEMPORARY, nbytes
*                                   specifies the size of the file in bytes.
*                                   For access mode READ_ONLY or READ_WRITE,
*                                   nbytes will return the total number of
*                                   bytes in the file.
*_Param  int   mode;        I       Specifies the access mode for the file.
*                                      0 - Open an existing file READ_ONLY
*                                      1 - Open an existing file READ_WRITE
*                                      2 - CREATE a new file for read/write
*                                      3 - Open an existing file READ_WRITE
*                                          if it exists, otherwise CREATE a
*                                          new file.
*                                      4 - Create a TEMPORARY file for
*                                          read/write access.
*_Param  int    *ret;       O       Return code:
*					 0 - ok
*					-1 - invalid access requested
*					-2 - file open/create failed

*_Desc  PIO_IN will open an existing file or create a new file for access
*       by the PRIMIO low level routines.  This routine needs to be 
*       called before any PRIMIO routines can perform I/O operations.

*       The fid is an integer value used internally by these routines 
*       to uniquely associate each file to an internal structure known
*       as the FCB (File Control Block) which holds descriptive
*       information about the opened file.  For the PRIMIO routines,
*       the information is meant to be for internal use only.  PIO_IN
*       creates the FCB dynamically and associates it with the file
*       'flspec'.  pio_in will completely initialize the FCB.  Subsequent
*       references to a file by other PRIMIO routines will be via the fid.

*_KEYS  FILE_I/O

*_Hist  Oct 10 1992 Kris Becker USGS, Flagstaff Original C Version
*_End
**************************************************************************/
{
  static int new_fids = 1000;
  register PRIMIO_FCB  *fb;
  register long nblocks;
  long tbytes;
  long file_pos;		/* Position of file pointer */
  int file_no;

  register int i;
  int omode, flags;		
  char *open_mode;

#if defined(FSTAT_AVAIL_OS)
  struct stat sb;
#endif

  int  status;
  char errbuf[256];

/***********************************************************************
  Determine if the fcb array has been initialized - if not, initial it
************************************************************************/
  if (fcb_init == FALSE) {
     for (i = 0 ; i < MAX_FILES ; i++) 
       file_fcbs[i] = NO_FCB;
     fcb_init = TRUE;
  }

/***********************************************************************
  If the file is to be created, calculate the total number of blocks
  to allocate to the file.  Create the open mode flags for the proper
  open call.
************************************************************************/
  switch (mode) {

/* Set up to access a file READ_ONLY */
          case 0:
                  flags = O_RDONLY;
                  omode = 0;
                  open_mode = "open (READ_ONLY)";
                  break;
 
/* Set up for READ_WRITE access */
          case 1:
          case 3:
                  flags = O_RDWR;
                  omode = 0;
                  open_mode = "open (READ_WRITE)";
                  break;

/* Create a new file for read/write access */
          case 2:
          case 4:
                   flags = O_RDWR | O_CREAT | O_EXCL;
                   omode = 0664;
                   nblocks = (*nbytes + 511) / 512;
                   tbytes = nblocks * 512;
                   open_mode = "create";
                   break;

/* Invalid access option specified */
          default:
                   (void) sprintf(errbuf,
                      "%%PIOIN-INVACC - Invalid file access mode requested: %d",
                                  mode);
                   errno = 0;
                   (void) perror(errbuf);
                   *ret = -1;
                   return (-1);
                   break;
  }


/*********************************************************************
  Open the file with the requested access mode
**********************************************************************/        
  file_no = open( flspec, flags, omode );

/* Check for failure under access mode 3.  If it fails for this case,
   the file didn't exist so try to create it.  */
  if (file_no < 0 && mode == 3) { 
    flags = O_RDWR | O_CREAT | O_EXCL;
    omode = 0744;
    nblocks = (*nbytes + 511) / 512;
    tbytes = nblocks * 512;
    file_no = open( flspec, flags, omode);
  }

/* Now we can check to determine if the file was opened successfully */
  if (file_no < 0) {
    (void) sprintf(errbuf,"%%PIOIN-CREFAI - Unable to %s the file %s",
                   open_mode, flspec);
    (void) perror(errbuf);
    *ret = -2;
    return (-2);
  }

/***********************************************************************
  Determine the size of the file
************************************************************************/
  if (flags & O_CREAT) {
    file_pos = tbytes;
#if defined(FTRUNCATE_AVAIL_OS)
    status = ftruncate(file_no, (off_t) tbytes);
    if (status < 0) {
      (void) sprintf(errbuf,
                     "%%PIOIN-FILERR - Error allocating %ld bytes to file %s",
                     tbytes, flspec);
      (void) perror(errbuf);
      *ret = -3;
      return (-3);
    }
#endif
  }
  else { 
#if defined(FSTAT_AVAIL_OS)
    status = fstat( file_no, &sb );
    tbytes = (long) sb.st_size;
#else
    tbytes = (long) lseek(file_no , (off_t) 0, SEEK_END);
    if (tbytes < 0) status = -1;
    else status = 0;
#endif
    if (status < 0) {
      (void) sprintf(errbuf,
                     "%%PIOIN-FILERR - Error determining size of file %s",
                     flspec);
      (void) perror(errbuf);
      *ret = -3;
      return (-3);
    }  
    *nbytes = tbytes;
    nblocks = (tbytes + 511) / 512;
  }
        
/************************************************************************
  Allocate an internal PRIMIO FCB.
*************************************************************************/
  fb = (PRIMIO_FCB *) malloc(sizeof(PRIMIO_FCB));
  if (fb == NO_FCB) { 
    (void) sprintf(errbuf,"%%PIOIN-ALLFAI - Unable to allocate FCB for file %s",
                   flspec);
    (void) perror(errbuf);
    *ret = -4;
    return (-4);
  }
 
/*************************************************************************
  Set up the internal FCB for the file
**************************************************************************/
  *fid = fb->fid = new_fids++;
  fb->access = ((flags & O_RDWR) ? READ_WRITE : READ_ONLY);
  fb->mode = mode;
  fb->file_handle = file_no;
  strcpy( fb->filnam, flspec);
  fb->tbytes = tbytes;
  fb->nbytes = *nbytes;
  fb->disposition = ((mode == 4) ? DELETE : KEEP);

/*************************************************************************
  Insert the FCB into the PRIMIO internal file table
**************************************************************************/
  for (i = 0 ; i < MAX_FILES ; i++) {
    if (file_fcbs[i] == NO_FCB) {
       file_fcbs[i] = fb;
       break;
    }
  }
        
/* Ensure that the FCB has been added to the open file list */
  if (i >= MAX_FILES) {
    (void) sprintf(errbuf,
             "%%PIOIN-MAXOVR - Maximum number files exceeded (%d) for file %s", 
                   MAX_FILES, flspec);
    errno = 0;
    (void) perror(errbuf);
    *ret = -5;
    return (-5);
  }

#ifdef DEBUG
printf("\nTotal bytes in file: %ld   Blocks: %ld\n",tbytes, nblocks);
printf("File name:%s   Handle: %d\n", fb->filnam, fb->file_handle);
#endif

  *ret = 0;
  return (0);
}



int pio_cl(int fid, int idele, int *ret)
/**************************************************************************
*_Title pio_cl Close an opened file
*_Args  Type  Name            I/O	Description
*_Parm  int   fid;             I	File identifier of the opened file
*_Parm  int   idele;           I	Specifies to KEEP or DELETE the
*                                       file upon closure.
*                                         0 - Don't delete the file upon close
*                                         1 - Delete the file upon close
*_Parm  int   *ret;            O	Return code:
*                                         0 - ok
*                                        -1 - operation failed
*                                        -2 - error deleting file

*_Desc  pio_cl closes a file that has been opened by pio_in.  The caller
*       may optionally request to delete the file upon closure of the
*       file. 

*_Keys  FILE_I/O

*_Hist  Oct 21 1992 Kris Becker, U.S.G.S., Flagstaff Original C Version
*_End
**************************************************************************/
{
  register int i, status;
  char errbuf[256];

/************************************************************************
  Find the specified file
*************************************************************************/
  for (i = 0 ; i < MAX_FILES ; i++ )
    if (file_fcbs[i] != NO_FCB) 
      if (file_fcbs[i]->fid == fid) break;
       
/***********************************************************************
  Close the file and optionally delete it
************************************************************************/
  if (i < MAX_FILES) {
    status = close( file_fcbs[i]->file_handle );
    if (status != 0) {
      (void) sprintf(errbuf,
            "%%PIOCL-CLSERR - Error closing file %s associated with file id %d",
                     file_fcbs[i]->filnam, fid);
      (void) perror(errbuf);
      *ret = -1;
      return (-1);
    }

#if defined(UNLINK_AVAIL_OS)
/*  Now check if the file is to be deleted */
    if (idele == DELETE || file_fcbs[i]->disposition == DELETE) {
      status = unlink(file_fcbs[i]->filnam);
      if (status < 0) {
        (void) sprintf(errbuf,
             "%%PIOCL-CLSERR - Error deleting file %s associated to file id %d",
                       file_fcbs[i]->filnam, fid);
        (void) perror(errbuf);
        *ret = -2;
        return (-2);
      }
    }
#endif

/* Free the PRIMIO FCB memory */
    (void) free((void *) file_fcbs[i]);
    file_fcbs[i] = NO_FCB;
  }

/****************************************************************
   All done...return to caller
*****************************************************************/
  *ret = 0;
  return (0); 
}



int pio_rd(int fid, long ibyte, long nbytes, void *buf, int *ret)
/************************************************************************
*_Title pio_rd Read data from a file
*_Args  Type  Name            I/O	Description
*_Parm  int   fid;             I	File identifier of the opened file
*_Parm  long  ibyte;           I	Starting byte of the file at which
*                                       to read the data from.  (ibyte=1
*                                       specifies the first byte in the
*                                       file.)
*_Parm  long  nbytes;          I        Number of bytes to read from the
*                                       file.  
*_Parm  void  *buf;            O        Buffer that will contain the data read
*                                       from the file upon return to the caller.
*_Parm  int   *ret;            O	Return code:
*                                         0 - ok
*                                        -1 - operation failed

*_Desc  pio_rd reads data from a disk file.  A file is considered to be
*       a contiguous stream of bytes.  ibyte specifies the starting byte
*       position in the file to begin reading data from.  The first byte
*       position in the file is 1.  nbytes specifies the number of bytes
*       to read.

*_Keys  FILE_I/O

*_Hist  Oct 22 1992 Kris Becker U.S.G.S., Flagstaff Original C Version
*_End
**************************************************************************/
{
  register PRIMIO_FCB *fb;
  long n;
  register int i;

  char  errbuf[256];


/**********************************************************************
  Get the PRIMIO file control block for this file id
***********************************************************************/
  if ((fb = pio_get_fcb(fid)) == NO_FCB) {
    (void) sprintf(errbuf,
               "%%PIORD-INVFID - Invalid file identifier (%d) - file not found",
                   fid);
    errno = 0;
    (void) perror(errbuf);
    *ret = -1;
    return (-1);
  }

/**********************************************************************
  Verify the requested byte addresses for the I/O operation
***********************************************************************/
  if (ibyte < 1 || ibyte > fb->nbytes) {
    (void) sprintf(errbuf,
           "%%PIORD-INVADR - Invalid byte address (%ld) for read in file %s", 
                   ibyte, fb->filnam);
    errno = 0;
    (void) perror(errbuf);
    *ret = -2;
    return (-2);
  }
       
  if (nbytes < 1 || (ibyte + nbytes -1) > fb->nbytes) {
    (void) sprintf(errbuf,
             "%%PIORD-INVADR - Invalid number bytes (%ld) for read in file %s", 
                   nbytes, fb->filnam);
    errno = 0;
    (void) perror(errbuf);
    *ret = -3;
    return (-3);
  }

/***********************************************************************
  Position the file pointer to the proper requested file byte address
  and read the data.
************************************************************************/
  n = lseek( fb->file_handle,  ibyte-1, SEEK_SET );
  if (n != (ibyte -1)) {
    (void) sprintf(errbuf,
         "%%PIORD-POSERR - Error position file pointer at byte %ld in file %s", 
                   ibyte, fb->filnam);
    (void) perror(errbuf);
    *ret = -4;
    return (-4);
  }

/* Read the data to the file */
  n = read( fb->file_handle, buf, nbytes );
  if (n != nbytes) {
    (void) sprintf(errbuf,
               "%%PIORD-IOERR - Error reading %ld bytes at byte %ld from file", 
                   nbytes, ibyte, fb->filnam);
    (void) perror(errbuf);
    *ret = -5;
    return (-5);
  }

  *ret = 0;
  return (0);
}



int  pio_wt(int fid, long ibyte, long nbytes, void *buf, int *ret)
/************************************************************************
*_Title pio_wt Write data to a file
*_Args  Type  Name            I/O	Description
*_Parm  int   fid;             I	File identifier of the opened file
*_Parm  long  ibyte;           I	Starting byte of the file at which
*                                       to write the data from.  (ibyte=1
*                                       specifies the first byte in the
*                                       file.)
*_Parm  long  nbytes;          I        Number of bytes to write to the
*                                       file.  
*_Parm  void  *buf;            I        Buffer that contains the data to write
*                                       the file.
*_Parm  int   *ret;            O	Return code:
*                                         0 - ok
*                                        -1 - operation failed

*_Desc  pio_wt writes data to a disk file.  A file is considered to be
*       a contiguous stream of bytes.  ibyte specifies the starting byte
*       position in the file to begin writing data to.  The first byte
*       position in the file is 1.  nbytes specifies the number of bytes
*       to write.

*_Keys  FILE_I/O

*_Hist  Oct 22 1992 Kris Becker U.S.G.S., Flagstaff Original C Version
*_End
**************************************************************************/
{
  register PRIMIO_FCB *fb;
  long n;
  register int i;

  char  errbuf[256];


/**********************************************************************
  Get the PRIMIO file control block for this file id
***********************************************************************/
  if ((fb = pio_get_fcb(fid)) == NO_FCB) {
    (void) sprintf(errbuf,
               "%%PIOWT-INVFID - Invalid file identifier (%d) - file not found",
                   fid);
    errno = 0;
    (void) perror(errbuf);
    *ret = -1;
    return (-1);
  }

/*********************************************************************
  Ensure we have write access to the file
**********************************************************************/
  if (fb->access != READ_WRITE) {
    (void) sprintf(errbuf,"%%PIOWT-INVACC - File %s does not have write access",
                   fb->filnam);
    errno = 0;
    (void) perror(errbuf);
    *ret = -2;
    return (-2);
  }
       
/**********************************************************************
  Verify the requested byte addresses for the I/O operation
***********************************************************************/
  if (ibyte < 1 || ibyte > fb->nbytes) {
    (void) sprintf(errbuf,
            "%%PIOWT-INVADR - Invalid byte address (%ld) for write in file %s", 
                   ibyte, fb->filnam);
    errno = 0;
    (void) perror(errbuf);
    *ret = -3;
    return (-3);
  }
       
/***********************************************************************
  Position the file pointer to the proper requested file byte address
  and write the data.
************************************************************************/
  n = lseek( fb->file_handle,  ibyte-1, SEEK_SET );
  if (n != (ibyte - 1)) {
    (void) sprintf(errbuf,
          "%%PIOWT-POSERR - Error position file pointer at byte %ld in file %s",
                   ibyte, fb->filnam);
    (void) perror(errbuf);
    *ret = -4;
    return (-4);
  }

/* Write the data to the file */
  n = write( fb->file_handle, buf, nbytes );
  if (n != nbytes) {
    (void) sprintf(errbuf,
                 "%%PIOWT-IOERR - Error writing %ld bytes at byte %ld to file", 
                   nbytes, ibyte, fb->filnam);
    (void) perror(errbuf);
    *ret = -5;
    return (-5);
  }

/* Determine if the write extends the file and update the internal FCB */
  if ((ibyte+nbytes-1) > fb->nbytes) {
    fb->nbytes = ibyte + nbytes - 1;
    fb->tbytes = MAX(fb->nbytes, fb->tbytes);
  }

  *ret = 0;
  return (0);
}


int  pio_ap(int fid, long nbytes, long *ntbytes, int *ret)
/************************************************************************
*_Title pio_ap Extend a file size by a specified number of bytes
*_Args  Type  Name            I/O	Description
*_Parm  int   fid;             I	File identifier of an open file
*_Parm  long  nbytes;          I        Number of bytes to extend the file by.  
*_Parm  long  ntbytes;         O        Total number of bytes in the file
*                                       after the extension.
*_Parm  int   *ret;            O	Return code:
*                                         0 - ok
*                                        -1 - operation failed

*_Desc  pio_ap will extend a file by nbytes bytes.  These bytes are added
*       to the end of the file.  Upon return, ntbytes will contain the
*       total number of bytes in the file.

*_Keys  FILE_I/O

*_Hist  Oct 22 1992 Kris Becker U.S.G.S., Flagstaff Original C Version
*_End
**************************************************************************/
{
  register PRIMIO_FCB *fb;
  long n;
  register int i;
  unsigned char dummy = 0;
 
  int status;  
  char  errbuf[256];


/**********************************************************************
  Get the PRIMIO file control block for this file id
***********************************************************************/
  if ((fb = pio_get_fcb(fid)) == NO_FCB) {
    (void) sprintf(errbuf,
               "%%PIOAP-INVFID - Invalid file identifier (%d) - file not found",
                   fid);
    errno = 0;
    (void) perror(errbuf);
    *ret = -1;
    return (-1);
  }

/*********************************************************************
  Ensure we have write access to the file so that the file may be
  extended.  Then extend the file to the requested size
**********************************************************************/
  if (nbytes > 0) {
    if (fb->access != READ_WRITE) {
      (void) sprintf(errbuf,
                     "%%PIOAP-INVACC - File %s does not have write access",
                     fb->filnam);
      errno = 0;
      (void) perror(errbuf);
      *ret = -2;
      return (-2);
    }

/* Now update the FCB */
    fb->nbytes += nbytes;
    fb->tbytes = MAX(fb->nbytes, fb->tbytes);

/* Now physically extend the file */
#if defined(FTRUNCATE_AVAIL_OS)
    status = ftruncate(fb->file_handle, (off_t) fb->nbytes);
#else
    (void) pio_wt(fid, fb->nbytes, 1, (void *) &dummy, &status);
#endif

/* Check for status of append operation */
    if (status < 0) {
      (void) sprintf(errbuf,
                     "%%PIOAP-EXTERR - Error extending %ld bytes in file %s",
                     nbytes, fb->filnam);
      (void) perror(errbuf);
      *ret = -3;
      return (-3);
    }
  }

  *ntbytes = fb->nbytes;
  *ret = 0;
  return (0);
}


static PRIMIO_FCB *pio_get_fcb(int fid)
/************************************************************************
*_Title pio_get_fcb Return the PRIMIO file control block for file id
*_Nobind
*_Args  Type  Name            I/O	Description
*_Parm  int   fid;             I	File identifier of the opened file
*_Desc  pio_get_fcb returns the address of a PRIMIO file control block
*       structure that is associated with fid.

*_Keys  FILE_I/O

*_Hist  Oct 22 1992 Kris Becker U.S.G.S., Flagstaff Original C Version
*_End
**************************************************************************/
{
  register int i;

/**********************************************************************
  Get the PRIMIO file control block for this file id
***********************************************************************/
  for (i = 0 ; i < MAX_FILES ; i++ )
    if (file_fcbs[i] != NO_FCB)
      if (file_fcbs[i]->fid == fid) 
        return (file_fcbs[i]);

/* If we reach here, it doesn't exist */
  return (NO_FCB);
}     
