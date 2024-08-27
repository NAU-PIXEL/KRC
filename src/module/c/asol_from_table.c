# include "asol_from_table.h"

# include <stdio.h>

double get_val(FILE *fp)
{
  char buffer[64];
  char current_char;
  int buffer_loc = 0;
  do
  {
    current_char = fgetc(fp);
    buffer[buffer_loc] = current_char;
    buffer_loc++;
  } while (current_char != '\t' && !feof(fp));

  buffer[buffer_loc] = NULL;

  return atof(buffer);
}

FILE *open_csv(int *status)
{
  static FILE *fp = NULL;
  if (fp == NULL)
  {
    fp = fopen("down_vis.csv", "r");
    if (fp == NULL)
    {
      printf("Could not open file\n");
    }
  }
  if (feof(fp))
  {
    *status = 1;
  }
  return fp;
}

void reset_csv()
{
  int status = 0;
  FILE *fp = open_csv(&status);
  rewind(fp);
}

// function to open csv and iterate through the values in it
double read_csv()
{
  int status = 0;
  FILE *fp = open_csv(&status);
  if (fp == NULL)
  {
    printf("Could not open file\n");
    return -1.0;
  }
  if (status != 0)
  {
    printf("Could not open file\n");
    return -1.0;
  }
  // lines can be very long, so read character by character
  // until we read a tab (separator) and then parse the current buffered string
  double res = get_val(fp);
  return res;
}

void insert_from_csv(f_real_array *arr, int insert_index)
{
  // Fortran is 1 based, C is 0 based
  arr->data[insert_index - 1] = read_csv();
}