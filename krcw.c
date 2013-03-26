#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"
#include "idlkrc.h"

IDL_RETURN idlkrc (int argc, void *argv[])
{
  int *ARG1;
  KRC_KRCCOM *idl_krccom;
  KRC_LATCOM *idl_latcom;
  KRC_DAYCOM *idl_daycom;
  KRC_FILCOM *idl_filcom;

  int result;

  INT4 FTN_NAME(idlkrc) (int *ARG1 );

  if (argc != 5) {
    (void) fprintf(stderr,
      "IDL caller of idlkrc did not pass the 5 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/*===============================================================
  Prior to calling the FORTRAN routine, transfer all incoming
  data from IDL to the FORTRAN commons and the integer action
  code argument
=================================================================*/
  ARG1  =  (int  *) argv[0];
  idl_krccom = (KRC_KRCCOM *) argv[1];
  idl_latcom = (KRC_LATCOM *) argv[2];
  idl_daycom = (KRC_DAYCOM *) argv[3];
  idl_filcom = (KRC_FILCOM *) argv[4];

  (void) memmove((void *) KRCCOM, (void *) idl_krccom, sizeof(KRC_KRCCOM));
  (void) memmove((void *) FILCOM, (void *) idl_filcom, sizeof(KRC_FILCOM));

  
/* Call the FORTRAN routine */
  result =  FTN_NAME(idlkrc) (ARG1);

/*=================================================================
  Return all the results of the run to the IDL caller
=================================================================*/
  (void) memmove((void *) idl_krccom, (void *) KRCCOM, sizeof(KRC_KRCCOM));
  (void) memmove((void *) idl_latcom, (void *) LATCOM, sizeof(KRC_LATCOM));
  (void) memmove((void *) idl_daycom, (void *) DAYCOM,  sizeof(KRC_DAYCOM));
  (void) memmove((void *) idl_filcom, (void *) FILCOM,  sizeof(KRC_FILCOM));
  return (result);
}
