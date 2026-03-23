#ifndef FLUX_STRUCTS_H
#define FLUX_STRUCTS_H

#include <stdbool.h>

typedef enum FLUX_TYPE {
  FLUX_UNKNOWN = 200,
  FLUX_ASOL,
  FLUX_SOLDIF,
  FLUX_PLANV,
  FLUX_PLANH,
  FLUX_RAW
} FLUX_TYPE;


typedef struct lt_fluxes
{
  int n_rows;
  int n_cols;
  double * ordered_columns[6];
  double * unordered_columns[6];
  double *time;
  double *asol;
  double *soldif;
  double *planv;
  double *planh;
  double *raw;
} lt_fluxes;

typedef struct flux_table
{
  // VERSION is future proofing for new table formats and features,
  // we expect to implement this as a union on the table pointer
  // when this is implemented, and then switching based on VERSION
  int VERSION;
  lt_fluxes *lt_table; 
} flux_table;

typedef struct flux_booleans
{
  bool * asol;
  bool * soldif;
  bool * planv;
  bool * planh;
  bool * raw;
} flux_booleans;

#endif