#ifndef FLUX_LOAD_H
#define FLUX_LOAD_H

#include <stdio.h>
#include <stdbool.h>

#include "flux_structs.h"
#include "read_tools.h"
#include "flux_tools.h"

jd_table *build_table(char *filename);

#endif