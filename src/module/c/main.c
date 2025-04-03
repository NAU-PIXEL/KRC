#include "main.h"


void main() {
  printf("Hello from C!\n");

  if (debug) {
    printf("Debug mode is on.\n");
  }

  // we're gonna start with reading an example L_s table,
  // then demo some of the interpolation functions

  // probably should do this with a real unit testing library, but for now, just do it manually

  // read rectangular table
  flux_table * table = build_table("data/jd_lt_table.txt");

  if (table == NULL) {
    printf("Error: Rectangular table could not be built.\n");
    exit(1);
  }
}