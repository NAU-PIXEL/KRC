Input Flux Tables for KRC
=========================

While KRC has built-in functionality to calculate how much energy arrives at a surface based on the body's orbit, rotation, atmosphere, and local slope conditions, sometimes a user may want to pre-calculate for themself the downwelling flux that is incident on a surface. 
This can be for many reasons, but generally is done to model some phenomenon not otherwise captured within the basic assumptions of KRC, such as self-shadowing terrain, alternative atmospheric models, or complex tumbling rotation of a body. 
So long as the flux can be expressed as a table over a span of time, broken out into visible and infrared components, that flux can be read as input by KRC, and used to run the same 1-D thermal model as KRC's normal behavior. 

Enabling flux tables in the input file
=====================================

To enable reading of input flux tables, add the following line to your input file: 

`8 26 0 '<input_file>' /` 

This tells KRC to load the file at the selected location as a flux table. If the file cannot be found KRC will throw an error.

Building the flux table
=====================

Each flux table consists of a 1D ASCII table with a 1-line header. 
Each row has a time index, visible downwelling flux value, and IR downwelling flux value. 
Together, a row of this table looks something like the following.

```123.456 789 101```

The header of the table is currently just the number of rows in the table, but this is subject to change. 
Reporting this number in the file's header speeds up data import time. 
It is recommended that the table end with a newline character.

See an example of the [table](./example/example_table.txt) and [input file](./example/krc_tab.inp).

## Time representation in KRC
The time index is how KRC determines when to apply each pair of downwelling fluxes to the core thermal model. 
To understand how KRC interprets these values, it is helpful to review how KRC's core model functions, and some of the counter-intuitive ways KRC's internal understanding of time differs from reality.
A complete explanation of these processes can be found in [Kieffer, 2013](./doc/Kieffer13krc.pdf), sections 2.1, and 3.2.6-3.2.7.

KRC understands time proceeding in two ways: "hours", representing some integer number of subdivisions of a solar day, and "seasons", representing some integer number of subdivisions of a solar year. 
The length of a solar day (sol) is required as an input (```PERIOD```), so KRC knows how to relate these internal timesteps to physical units.
The length of a year is typically a required input (```OPERIOD```), but when using flux tables, this is calculated indirectly using two other inputs (```DELJUL``` and the number of seasons in the flux table), as discussed below. 


KRC makes two assumptions, first that a year is much longer than a sol (not the case for all solar system bodies, e.g., Mercury and Venus), and second that a season must be at least as long as a sol. 
In general, this means that, for each season, KRC will iteratively solve the heat diffusion equation, looping over the diurnal flux curve representing the illumination conditions of that season, beginning at midnight, and repeating those fluxes until one of two things happens:

1. If a season is much longer than a sol, KRC will monitor several convergence criteria, and if those criteria are met, KRC will perform a fit to determine the asymptotic trend in temperature at midnight in order to extrapolate the temperatures of the system at the beginning of the following season. 
2. If a season is not long enough to achieve convergence, then a simple linear fit is used instead to predict the starting temperatures for the next season. In this way, even if a season is only slightly longer than a single sol (or even exactly equal to a sol), appropriate starting temperatures can be computed. 

Once KRC has calculated the starting temperatures for the following season, the process repeats, cycling through the diurnal curve of fluxes representing the next season.

Note that both methods have the advantage that seasons need not be an integer number of sols. 
For example, if a season were 1.5 sols long, then we would begin a season at midnight (t=0), advance forward in time by one sol (t=1), fit the two midnight temperatures with a line, extend that line out to the start of the next season (t=1.5), and use those forecast temperatures to begin the following season, again starting at midnight. 
Without this extrapolation, the ending temperatures at t=1.5 would represent temperatures at noon when beginning the following season, a problem that becomes quite a bit trickier to resolve when seasons can be any real number multiple of a sol in length. 

Starting each season at midnight is computationally quite useful, in that we can define the input fluxes as representing the diurnal curve, from midnight to midnight, for a theoretical instantaneous seasonal time step through the year.  
When considering a single diurnal curve representing many seconds worth of physical time, the orbital position used to calculate that diurnal curve remains constant. 
The body does not proceed through its orbit as each sol passes, until the start of the next season, and so the single diurnal curve is used for all calculations within that season.  
In effect, this completely decouples the diurnal passage of time from the annual passage of the seasons. 
However, this makes discussion and representation of time a bit trickier for our purposes.
The time index is our solution to this problem. 

## Time index
The time index combines the seasonal timestep with the diurnal timestep in a single floating-point value. These values must be monotonically increasing, and if KRC detects that they are not, it will exit early. 

### Season
The integer portion of the time index represents the season, beginning at 0 for the first season being computed. 
Incrementing this value by 1 represents advancing the season by DELJUL Earth days of time. 

The logic that interprets the flux table will proceed through the table season by season until all computation is completed (with input parameter ```N5``` setting the number of season steps to calculate). 
If the end of the table is reached before finishing all requested seasons, the process repeats, looping back around to the beginning of the file. 
In this way, the flux table is generally implied to represent one complete year.

Note that, because the flux table itself is agnostic to the number of seasons in a year, the user must ensure that they select ```DELJUL``` which, when multiplied by (max season - min season) (i.e., the number of seasons in the table, assuming starting from 0, and no seasons skipped), equals exactly one year (in units of Earth days) for the target body. 

However, in particular use cases, you could prepare a flux table with any number of seasons, and so long as you set the number of seasons for KRC to calculate (```N5```) to be less than or equal to the number of season in your flux table, you would never loop back to the start, and so could choose to represent any span of time you wanted (i.e., more than or less than one year, though still restricted to the maximum ```N5``` of 2161).

### Hour
The decimal portion of the time index represents the diurnal time step, or hour, of that row in the table. 
This value ranges from [0,1), starting at midnight, representing a fractional portion of a solar day, (e.g., 00:00 is .0, 06:00 is .25, 12:00 is .5, 18:00 is .75, and 23:59 is ~.9993). 
In each season, KRC will loop over that season's diurnal flux curve, from .000 to .999... and back to .000.

### Time index interpolation
The decimal portions of the time indices are not required to precisely match the exact fractional divisions of a sol used by KRC (as determined by ```N2```, the number of timesteps computed in each solar day).
In fact, it is not required that there be ```N2``` hourly timesteps listed at all. 
At runtime, KRC will determine the flux to input into the heat diffusion calculation by linearly interpolating between records ```n-1``` and ```n``` of the flux table, where the time index of ```n``` is >= to KRC's internal timestep.

There is also logic to handle the case of times between the final defined hour of a sol and midnight, which should work as expected, so long as midnight itself (.000) is defined. 
Behavior when midnight is not defined is not currently documented.

This interpolation allows for the user to prepare flux tables with some seasons having sparser diurnal time sampling than others.

It is suggested to have at least two rows per seasonal step (midnight and one other time), so that interpolation between days does not give unexpected results.

## Fluxes
Fluxes must be recorded in watts per square meter. 
Visible flux is considered to include all direct solar illumination, while IR flux is considered to include all indirect illumination, such as from the atmosphere (if present), self-illumination, or other secondary illuminators.  
>>> Do these fluxes use separate albedos or something? Why have them be reported separately at all? How are they handled differently? 


Special considerations
======================

## Continuous linear time
With particular input parameters, a flux table can be used to model a continuous, linear flow of time, with none of KRC's usual diurnal and annual looping. 
To achieve this, there must be no extra time between seasonal steps in the flux table, i.e., the last hour of each season must be immediately followed by the first hour of the following season. 
Additionally, in the KRC input file, ```PERIOD``` must be exactly equal to ```DELJUL``` (to prevent any inter-seasonal extrapolation), and ```N5``` must be less than or equal to the total number of seasons in the flux table (so that the flux table does not restart from the beginning at any point). 

## L_s
While a body's solar longitude (L_s) is often a convenient way to understand how conditions change throughout its year, for all orbits with non-zero eccentricity, L_s does not change over time at a constant rate.
These flux tables take advantage of how KRC internally handles time, and so must follow its internal conventions. 
That means evenly dividing the year into an integer number of seasons, which must be evenly spaced in time by ```DELJUL```. 

Currently no conversion function exists to convert from seasonal fluxes sampled evenly in L_s to seasons sampled evenly in time. 
Should one be developed, it would require interpolating every flux value in the entire table, which would be significantly computationally expensive for a large table.

Where possible, we suggest the following procedure for users needing to work in terms of L_s for whatever reason:
1. Generate an array of timesteps dividing a year evenly in time. 
2. Convert those timesteps to L_s.
3. Perform your required logic to generate fluxes. 
4. Record your output in a flux table, using the indices of the timesteps from step 1. 

This will avoid the need to interpolate fluxes selected by L_s into seasons spaced evenly in time.

## Unused inputs
Due to the use of flux tables entirely replacing KRC's internal logic for determining incident flux, many traditional KRC inputs needed for such calculations instead go unused. 

This includes nearly all parameters controlling the body's orbital motion, with the crucial exceptions of ```PERIOD``` and ```DELJUL```, which relate the time indices to physical units as discussed above. 

Also unused are all parameters describing the relationship between the surface under consideration and the larger body. 
A surface modeled using a flux table has no latitude nor longitude, and has no relative slope. 
Nor does it have an atmosphere, nor any relationship to the luminosity of the Sun. 

Such considerations are assumed to be relegated to whatever method is generating the flux table in the first place, and care should be taken to ensure all such factors are accounted for there.