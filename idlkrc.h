/*****************************************************************
*_Title IDLKRC Common areas for MGS KRC interface
*_Descr See ~/tes/KRC/ *.inc for a definitive explanation of
*       variables and their use. 
*
* NOTE:  It is EXTREMELY IMPORTANT that each variable in these
*        commons are of the same type, dimension and order as
*        dictated by the actual Fortran COMMON statement.
*
*_HIST 2002mar01 Hugh Kieffer  Adopted from docube.h
*      Mar 01 2002 Kris Becker - Tidied up the definitions a bit and
*                                completed missing elements
* 2002mar09 HK Move ALAT & ELEV from LATCOM to KRCCOM
* 2002jul28 HK Add items to LATCOM and DAYCOM related to new atmosphere
**********************************************************************/
#ifndef KRC_H
#define KRC_H

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus  */
#include "binding.h"
 
/****************************************************************
  Very machine specific type declarations
*****************************************************************/
typedef char               CHAR;
typedef char               INT1;
typedef unsigned char      UINT1;
typedef short              INT2;
typedef int                INT4;
typedef unsigned short     UINT2;
typedef unsigned int       UINT4;
typedef int                BOOLEAN;
typedef float              FLOAT4;
typedef double             FLOAT8;

/*****************************************************************
  KRC array dimensions
******************************************************************/
#define NUMFD    80      /* KRCCOM */
#define NUMID    40      /* KRCCOM */
#define NUMLD    20      /* KRCCOM */
#define MAXN4    19      /* LATCOM dimension of latitudes */
#define MAXN1    30      /* DAYCOM dimension of layers */
#define MAXN2    384     /* DAYCOM dimension of times of day     24*12 */
#define MAXN3    16      /* DAYCOM dimension of iteration days */
#define MAXN24   24      /* DAYCOM dimension of saved times of day */
#define MAXN1P   MAXN1+1 /* DAYCOM dimension layer temperture points */
#define MAXBOT   10      /* DAYCOM dimension of time divisions */
#define NUMCH    80      /* FILCOM number of characters in each name */

/****************************************************************
  Thes structures map to the KRC numerical commons.  This provides
  the binary data interface to KRC.  Note that this is a
  direct map in that there is no need for an intermediate
  secondary structure.
*****************************************************************/
typedef struct krc_krccom {
    FLOAT4  fd[NUMFD];   /* B Real parameters */
    INT4    id[NUMID];   /* B Integer parameters */
    BOOLEAN ld[NUMLD];   /* B Logical parameters */
    CHAR    title[80];   /* B 80-character title */
    CHAR    daytime[20]; /* B 20-character date-time */
    FLOAT4  alat[MAXN4]; /* B latitude in degrees */
    FLOAT4  elev[MAXN4]; /* B elevation in km */
} KRC_KRCCOM;

typedef struct krc_latcom {
 FLOAT4 ndj4[MAXN4];
 FLOAT4 dtm4[MAXN4];
 FLOAT4 tst4[MAXN4];
 FLOAT4 tts4[MAXN4];
 FLOAT4 ttb4[MAXN4];
 FLOAT4 frost4[MAXN4];
 FLOAT4 afro4[MAXN4];
 FLOAT4 tta4[MAXN4];
 FLOAT4 ttx4[MAXN4];
 FLOAT4 t4[MAXN4][MAXN1];
 FLOAT4 tin[MAXN4][MAXN1];
 FLOAT4 tax[MAXN4][MAXN1];
 FLOAT4 tsf[MAXN4][MAXN24];
 FLOAT4 tpf[MAXN4][MAXN24];
} KRC_LATCOM;


typedef struct krc_daycom {
 FLOAT4 x[MAXN1];
 FLOAT4 sconvg[MAXN1];
 FLOAT4 tlay[MAXN1];
 FLOAT4 tmin[MAXN1];
 FLOAT4 tmax[MAXN1];
 FLOAT4 t[MAXN1P];
 FLOAT4 tt[MAXN3][MAXN1];
 FLOAT4 tts[MAXN3];
 FLOAT4 ttb[MAXN3];
 FLOAT4 tta[MAXN3];
 FLOAT4 dtmj[MAXN3];
 FLOAT4 fro[MAXN3];
 FLOAT4 asol[MAXN2];
 FLOAT4 adgr[MAXN2];
 FLOAT4 tout[MAXN2];
 FLOAT4 tsfh[MAXN24];
 FLOAT4 tpfh[MAXN24];
 FLOAT4 n1k[MAXBOT];
} KRC_DAYCOM;
    
/****************************************************************
  This structure maps to the Fortran CHARACTER common and provides
  the character data interface to Fortran.  Note that since
  Fortran and C handle character strings differently, there must
  be an intermediate C structure that is a C-compatible copy
  of the Fortran CHARACTER common
*****************************************************************/
typedef struct krc_filcom {
  CHAR finput[NUMCH];
  CHAR fout[NUMCH];
  CHAR fdisk[NUMCH];
} KRC_FILCOM;

/********************************************************************
  Here's our direct connection to KRC Fortran commons.   This
  connection occurs at link time.
*********************************************************************/
extern KRC_KRCCOM   FTN_COMMON(krccom);
extern KRC_LATCOM   FTN_COMMON(latcom);
extern KRC_DAYCOM   FTN_COMMON(daycom);
extern KRC_FILCOM   FTN_COMMON(filcom);

/********************************************************************
  This declares IDLKRCs variable that allows access to KRC's
  commons.  Note that the CHARACTER common, FILCOM, is directly
  linked because there needs to be no general C access to support
  C-like coding functions.  Its merely a transport mechanism.
  These connections are also made at link time.
*********************************************************************/
static KRC_KRCCOM *KRCCOM = &FTN_COMMON(krccom);
static KRC_LATCOM *LATCOM = &FTN_COMMON(latcom);
static KRC_DAYCOM *DAYCOM = &FTN_COMMON(daycom);
static KRC_FILCOM *FILCOM = &FTN_COMMON(filcom);
                        
#ifdef __cplusplus
}
#endif /* __cplusplus  */
#endif
