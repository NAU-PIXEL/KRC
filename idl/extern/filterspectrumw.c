#include <stdio.h>
#include <string.h>
#include "idl.h"
#include "binding.h"

IDL_INT filterspectrum (int argc, void *argv[])
/***********************************************************************
*_TITLE	filterspectrum  C wrapper for IDL to use FORTRAN routine of same name
*_ARGS	TYPE       VARIABLE 	I/O	DESCRIPTION
*_Parm  int        argc;         I      Number of arguments passed from
*                                       the IDL caller
*_Parm  void       *argv[];      I      Array of pointers to list of
*                                       arguments that are called from 
*                                       the IDL program.
*_Parm  IDL_RETURN filterspectrum ;          O      Return status code.
*
*_DESC derived from skeleton,
*  see original fortran source code for argument description
*
*_HIST 1997nov19  Hugh Kieffer
*_END
***************************************************************************/
{
  int *ARG1;
  double *ARG2;
  double *ARG3;
  int *ARG32;
  double *ARG4;
  double *ARG5;
  double *ARG6;

  int result;

/* Declare the function prototype; 
          append [] to arrays, preceed scalars with *    */
 extern int FTN_NAME(filterspectrum) (int *ARG1, double ARG2[], double ARG3[]
     ,int *ARG32, double ARG4[], double *ARG5 , double *ARG6 );

 
/***********************************************************************
  Check to ensure proper number of arguments are passed
************************************************************************/
  if (argc != 7) {
    (void) fprintf(stderr,
      "IDL caller of filterspectrum did not pass the 7 required arguments, but %ld",
                    (long) argc);
    return (-1);
  }

/***********************************************************************
  Map the arguments to IDL passing mechanism
************************************************************************/
  ARG1  =  (int  *) argv[0];
  ARG2  =  (double  *) argv[1];
  ARG3  =  (double  *) argv[2];
  ARG32  =  (int  *) argv[3];
  ARG4  =  (double  *) argv[4];
  ARG5  =  (double  *) argv[5];
  ARG6  =  (double  *) argv[6];
  
/***********************************************************************
  Call the FORTRAN version
************************************************************************/
  result =  FTN_NAME(filterspectrum) (ARG1, ARG2, ARG3, ARG32, ARG4, ARG5, ARG6);

/***********************************************************************
  All done...return to caller
************************************************************************/
  return (result);
}
