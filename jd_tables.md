Enabling JD tables in the input file
=====================================

To enable JD tables, add a line to your input file `8 26 0 '<input_file>' /`. This tells KRC to load the file at the selected location as a JD table. If the file cannot be found KRC will throw an error.

Building the JD table
=====================

The JD table consists of a 1D ASCII table with a header, where each row has a timestamp, visible down flux value, and IR down flux value. 
The timestamp is represented in the form `step.local`, where `step` is the index of the seasonal step, and `local` is the proportion through the day, e.g. 00:00 is 0.0, 06:00 is 0.25, 12:00 is 0.5, 18:00 is 0.75, and 23:59 is ~0.9993.
The fluxes are in watts per square meter. Together, a row of this table looks something like the following.
```123.456 789 101```

This table is expected to be sparse, and times sampled between explicitly provided timestamps are linearly interpolated. Despite this, it is suggested to have at least two rows per seasonal step, so that interpolation between days does not give unexpected results.

The header of the table is currently just the number of rows in the table, but this is subject to change.

It is recommended that the table end with a newline character.

See an example of the [table](./example/example_table.txt) and [input file](./example/krc_tab.inp).

Special considerations
======================

Because of the way KRC handles seasonal time steps, if continuous time flow is required, e.g. there is no extra time between seasonal steps, the input file must be specifically tuned for this. 
Specifically, PERIOD must be an exact multiple of DELJUL, and N5 must be tuned to match the expected number of days in a run. 

Another feature to consider is the repeat functionality.
Currently, and subject to change, if a input table file has less seasonal timestamps than the number of seasons in the run, it will wrap around and begin again at timestamp 0. 
If this is not desired, ensure that your number of seasonal steps matches the number of steps in the table.