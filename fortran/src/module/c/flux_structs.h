#ifndef FLUX_STRUCTS_H
#define FLUX_STRUCTS_H

#define JD_TABLE_RECT_STRING "RECT"
#define JD_TABLE_JAGGED_STRING "JAGGED"

typedef enum JD_TABLE_FORMAT {
  JD_TABLE_UNKNOWN = 100,
  JD_TABLE_RECTANGULAR,
  JD_TABLE_JAGGED
} JD_TABLE_FORMAT;

typedef enum FLUX_TYPE {
  FLUX_UNKNOWN = 200,
  FLUX_VIS,
  FLUX_IR
} FLUX_TYPE;

typedef struct lt_fluxes
{
  int n_lt;
  double *lt;
  double *vis;
  double *ir;
} lt_fluxes;

typedef struct flux_table
{
  JD_TABLE_FORMAT format;
  // VERSION is future proofing for new table formats and features,
  // we expect to implement this as a union on the table pointer
  // when this is implemented, and then switching based on VERSION
  int VERSION;
  lt_fluxes *lt_table; 
} flux_table;

const static char * const JD_TABLE_FORMAT_STRINGS[] = {
  JD_TABLE_RECT_STRING,
  JD_TABLE_JAGGED_STRING
};

static const int JD_TABLE_FORMAT_COUNTS = sizeof(JD_TABLE_FORMAT_STRINGS) / sizeof(char *);

#endif