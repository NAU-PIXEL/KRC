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
  jd_table * rect_table = build_table("data/jd_table_random_rect.txt");

  if (rect_table == NULL) {
    printf("Error: Rectangular table could not be built.\n");
    exit(1);
  }

  // read jagged table
  jd_table * jagged_table = build_table("data/jd_table_random_jagged.txt");
  
  if (jagged_table == NULL) {
    printf("Error: Jagged table could not be built.\n");
    exit(1);
  }

}