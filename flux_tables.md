Input Flux Tables for KRC
=========================

While KRC has built-in functionality to calculate how much energy arrives at a surface based on the body's orbit, rotation, atmosphere, and local slope conditions, sometimes a user may want to pre-calculate for themself the downwelling flux that is incident on a surface. 
This can be for many reasons, but generally is done to model some phenomenon not otherwise captured within the basic assumptions of KRC, such as self-shadowing terrain, alternative atmospheric models, or complex tumbling rotation of a body.
KRC considers the following sources of flux which we can make use of when supplying custom fluxes:
1. Direct solar illumination
2. Diffuse solar illumination from the sky
3. Infrared downwelling from the sky
4. Additional indirect visible-wavelength illumination
5. Additional indirect infrared-wavelength illumination
6. Any additional "raw" heat flow, imparting energy directly into the surface 

So long as the flux you're interesting in modeling can be expressed as a table over a span of time, broken out into some combination of these components, that flux can be read as input by KRC, and used to run the same 1-D thermal model as KRC's normal behavior. 

Enabling flux tables in the input file
=====================================

To enable reading of input flux tables, add the following line to your input file: 

`8 26 0 '<input_file>' /` 

This tells KRC to load the file at the selected location as a flux table. If the file cannot be found KRC will throw an error.

Building the flux table
=====================

Each flux table consists of a 1D ASCII table with a 2-line header. 
Each row has a time index and some number of flux values, corresponding to the flux types designated in the header. 
Each value is assumed to be a floating-point number, with no particular constraints on precision. 
Values are separated by spaces.
Together, a row of this table looks something like the following:

```123.456 789.10 11.12```

The first line of the header must be the total number of rows in the table (excluding the header). 
Reporting this number in the file's header speeds up data import time. 
The second line of the header must label the columns in the table that follows.
The table must include a TIME column, but all other columns are optional, and may be listed in any order.
The column labels must be one of the following strings (not? case sensitive):
1. ```TIME```
2. ```ASOL```
3. ```SOLDIF```
4. ```ATMRAD```
5. ```PLANV```
6. ```PLANH```
7. ```RAW```

For clarity and consistency, we recommend always using this order, skipping any columns not present in your use case.
Explanations of the columns and their usage are found in the sections below.

It is recommended that the table end with a newline character.

See an example of the [table](./example/example_table.txt) and [input file](./example/krc_tab.inp).

## Time representation in KRC
The time index is how KRC determines when to apply each pair of downwelling fluxes to the core thermal model. 
To understand how KRC interprets these values, it is helpful to review how KRC's core model functions, and some of the counter-intuitive ways KRC's internal understanding of time differs from reality.
The discussion in this section reflects how time is represented in both standard KRC and KRC using flux tables.
A complete explanation of these processes can be found in [Kieffer, 2013](./doc/Kieffer13krc.pdf), sections 2.1, and 3.2.6-3.2.7.

KRC understands time proceeding in two ways: "hours", representing some integer number of subdivisions of a solar day, and "seasons", representing some integer number of subdivisions of a solar year. 
The length of a solar day (sol) is required as an input (```PERIOD```), so KRC knows how to relate these internal timesteps to physical units.
The length of a year is typically a required input (```OPERIOD```), but when using flux tables, this is calculated indirectly using two other inputs (```DELJUL``` and the number of seasons in the flux table), as discussed below. 

KRC makes two assumptions, first that a year is much longer than a sol (not the case for all solar system bodies, e.g., Mercury and Venus), and second that a season must be at least as long as a sol. 
In general, this means that, for each season, KRC will iteratively solve the heat diffusion equation, looping over the diurnal flux curve representing the illumination conditions of that season, beginning at midnight, and repeating those fluxes until one of two things happens:

1. If a season is much longer than a sol, KRC will monitor several convergence criteria, and if those criteria are met, KRC will perform a fit to determine the asymptotic trend in temperature at midnight in order to extrapolate the temperatures of the system at the beginning of the following season. 
2. If a season is not long enough to achieve convergence, then a simple linear fit is used instead to predict the starting temperatures for the next season. In this way, even if a season is only slightly longer than a single sol (or even exactly equal to a sol), appropriate starting temperatures can be computed. 

Once KRC has calculated the starting temperatures for the following season, the process repeats, cycling through the diurnal curve of fluxes representing the next season.

Note that both methods have the advantage that seasons always start at midnight, without needing to be an integer number of sols. 
For example, suppose a season were 1.5 sols long. 
We might imagine a different model which begins a season at midnight (t=0) and steps forward in time continuously to the end of the season (t=1.5), setting the starting conditions for the following season to be the same as the end of the previous one. 
However, if we wanted the model to begin the next season at midnight, the system would then experience a solar time discontinuity between the end of the previous season (noon) and the beginning of the next (midnight).
If instead we wanted the model to pick up from where we left off in time, we would lose our control over which times of day were being modeled and recorded. 
In this example, half of our seasons would begin at midnight, and half would begin at noon.
Resolving this in the general case, where a season can be any real number of sols long, would involve a lot of expensive interpolation to produce standardized output.
Instead, KRC begins each season at midnight, and uses forecast temperatures to begin the following season, again starting at midnight, regardless of the length of a season in sols. 
This means all diurnal timesteps remain consistent across all the seasons being computed.
Note that when using flux tables the above scenario can be implemented, but special care must be taken to ensure it is logically consistent.
Please review the below sections on `time index interpolation` and `continuous linear time` for more info on this.

Starting each season at midnight is computationally quite useful, in that we can define the input fluxes as representing the diurnal curve, from midnight to midnight, for a theoretical instantaneous seasonal time step through the year. 
When considering a single diurnal curve representing many seconds worth of physical time, the effective orbital position used to calculate that diurnal curve remains constant. 
The body does not proceed through its orbit as each sol passes, until the start of the next season, and so that a single diurnal curve is used for all calculations within that season. 
In effect, this completely decouples the diurnal passage of time from the annual passage of the seasons. 
However, this makes discussion and representation of time a bit trickier for our purposes.
The time index is our solution to this problem. 

## Time index (```TIME```)
The time index combines the seasonal timestep with the diurnal timestep in a single floating-point value. In a flux table, these values must be monotonically increasing, and if KRC detects that they are not, it will exit early. 

### Season
The integer portion of the time index represents the season, beginning at 0 for the first season being computed. 
Incrementing this value by 1 represents advancing the season by DELJUL Earth days of time. 

The logic that interprets the flux table will proceed through the table season by season until all computation is completed (with input parameter ```N5``` setting the number of season steps to calculate). 
If the end of the table is reached before finishing all requested seasons, the process repeats, looping back around to the beginning of the file. 
In this way, the flux table is generally implied to represent one complete year.

Note that, because the flux table itself is agnostic to the number of seasons in a year, the only way to ensure that a year is modeled with the correct length is for the user to select a ```DELJUL``` which, when multiplied by (max season - min season) (i.e., the number of seasons in the table, assuming starting from 0, and no seasons skipped), equals exactly one year (in units of Earth days) for the target body. 
> Example:
> The Mars system orbits the Sun every 686.98 Earth days. 
> To model a surface on Mars (or either of its moons) using 10 seasons/year, you would prepare a flux table with 10 diurnal flux curves, evenly spaced in time throughout the year, and set ```DELJUL``` to 68.698 Earth days.

However, in particular use cases, you could prepare a flux table with any number of seasons, and so long as you set the number of seasons for KRC to calculate (```N5```) to be less than or equal to the number of season in your flux table, you would never loop back to the start, and so could choose to represent any span of time you wanted (i.e., more than or less than one year, though still restricted to the maximum ```N5``` of 2161).
One such use case is described in the section "Continuous linear time".

### Hour
The decimal portion of the time index represents the diurnal time step, or hour, of that row in the table. 
This value ranges from [0,1), starting at midnight, representing a fractional portion of a solar day, (e.g., 00:00 is .0, 06:00 is .25, 12:00 is .5, 18:00 is .75, and 23:59 is ~.9993). 
In each season, KRC will loop over that season's diurnal flux curve, from .000 to .999... and back to .000.

### Time index interpolation
The decimal portions of the time indices are not required to precisely match the exact fractional divisions of a sol used by KRC (as determined by ```N2```, the number of timesteps computed in each solar day).
In fact, it is not required that there be ```N2``` hourly timesteps listed at all. 
At runtime, KRC will determine the flux to input into the heat diffusion calculation by linearly interpolating between records ```n``` and ```n+1``` of the flux table, where KRC's internal timestep is between the time indices of ```n``` and ```n+1```.
This interpolation allows for the user to prepare flux tables with some seasons having sparser diurnal time sampling than others.

Note that for this interpolation, there is no special logic to handle the case between the final defined hour of a sol and midnight. 
KRC will interpolate between the final hour of one season's diurnal curve and the first hour of the next season, if KRC's internal time index fall into this period. 
This is to allow for the Continuous Linear Time case, discussed below. 
For runs where the typical diurnal looping behavior is desired (i.e., more than one sol per season), it is expected that each season's midnight fluxes will be duplicated at both start (.000) and end (.99999) of the diurnal curve. 

The interpolation will work between the final record in the table and the first record, when the internal timestep reaches the end of the available time indices. 
This means that looping back to the start of the year will work the same as interpolating between any other two adjacent seasons.

Though the interpolation allows for sparser sampling of the diurnal curve in certain seasons, it is not advisable to skip seasons entirely, as all timesteps in those seasons will be linearly interpolated between the values in the existing rows.
If the rest of the flux table is formed as generally expected, this means a linear interpolation between two midnights, which means no illumination whatsoever in the skipped seasons.

## Fluxes
All fluxes must be recorded in watts per square meter. 

### Direct Solar Illumination (```ASOL```)
This column includes all direct solar illumination incident on the surface.
In the current implementation, the fluxes in this column replace ASOL in tlats8, and so are scaled by a photometric function to determine the energy absorbed by the surface. 
This photometric function is selected by the user using the PHOG input parameter, and is typically a function of the solar incidence angle. 
(When KRC thinks sun is behind the local horizon, this photometric function defaults to the Lambertian albedo, so solar flux defined during this condition is still absorbed more or less as expected.)
This means that this column should only be used for direct illumination by the Sun, and not visible flux from any other sources, such as diffuse illumination from the atmosphere or secondary illuminators. 
This flux value is assumed to account for the slope of a surface and the slope's corresponding impact on solar incidence angle. 

This column cannot be used at the same time as KRC's conical pits (see Constraints below).

### Diffuse Solar Illumination (```SOLDIF```)
This column is for visible-wavelength diffuse flux from the sky, such as from visible atmospheric downwelling.
Values in this column are scaled by the fraction of exposure to the sky, ```SKYFAC```, as well as the hemispheric (Lambertian) albedo, to determine the energy absorbed by the surface.
This means that KRC must accurately know the slope of the surface in question when using this column, to accurately calculate ```SKYFAC```.

This column cannot be used at the same time as KRC's conical pits (see Constraints below).

### Infrared Atmospheric Downwelling (```ATMRAD```)
This column is for indirect hemispherical infrared flux from the sky, such as from infrared atmospheric downwelling. 
Fluxes in this column are scaled by the fraction of exposure to the sky, ```SKYFAC```, as well as the IR emissivity, EMIS, to determine the energy absorbed by the surface.
This means KRC must accurately know the slope of the surface in question when using this column, to accurately calculate ```SKYFAC```.

### Additional Indirect Visible Illumination (```PLANV```)
This column is for additional visible illumination from secondary light sources, such as overhead planets or moons. 
Fluxes in this column are scaled by the hemispheric (Lambertian) albedo to determine the energy absorbed by the surface.
This flux value is assumed to account for the slope of a surface and the slope's corresponding impact on the incidence angle of incident light.

This column overwrites the ```PLANV``` variable used for planetary visible illumination on the surfaces of moons.
However, it is agnostic regarding the source of illumination, and can also be used, for example, for self-illumination in complex terrain. 

### Additional Indirect Infrared Illumination (```PLANH```)
This column is for additional infrared illumination from secondary light sources, such as overhead planets or moons. 
Fluxes in this column are scaled by the IR emissivity to determine the energy absorbed by the surface.
This flux value is assumed to account for the slope of a surface and the slope's corresponding impact on the incidence angle of incident light.

This column overwrites the ```PLANH``` variable used for planetary infrared heating on the surfaces of moons.
However, it is agnostic regarding the source of illumination, and can also be used, for example, for self-illumination in complex terrain. 

### "Raw" Heat Flow (```RAW```)
This column is for any additional heat flowing into (+) or out of (-) the surface. 
Rather than replacing an existing KRC variable, the values of this column are added directly to the final net absorbed heat energy, ```POWER```, in tday8. 
These values are not scaled by anything before being added to the net absorbed energy.

This provides ultimate flexibility for the user to specify the contribution of any process which imparts a change in energy at the surface, such as the sublimation of volitiles, or the convection of an atmosphere.

> Note: The energy added in this way is not currently accounted for when determining average temperatures TAEQ4 and TSEQ4 when calculating TEQUIL during initialization in tlats8. 
> The impacts of this are currently untested (as of 2026.03.02), and it's unclear whether this would significantly impact results, or merely impose a loss of computational performance. 
> While theoretically, the user could implement their own illumination model, and fully specify all absorbed energy through this single column, we recommend only using this column for small contributions to the overall heat balance at this time.

Special considerations
======================

## Constraints
### Eclipses
The use of any flux tables will disable the use of KRC's eclipse implementation.
### Pits
KRC's calculation of indirect illumination inside conical pits relies on its internal illumination model for direct incident light on a level plane. 
As this is not available when specifying ```ASOL``` with a flux table, indirect illumination in conical pits cannot be used. 
tlats8.f checks for this conflict and will throw an error code if it detects conical pits are being used alongside either ```ASOL``` or ```SOLDIF``` flux tables.
### Snow
Using ```ASOL```, ```SOLDIF```, or ```ATMRAD``` columns will disable KRC's calculation of snow freezing out of the atmosphere (and in fact, it will prevent KRC's calculation of the atmospheric temperature altogether).
The presence of frost will depend entirely on the direct deposition (and sublimation) of frost based on the surface temperature.
JBARE can still be used as normal to remove all frost at a selected date.

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

## Self-heating 
By default, KRC assumes a surface is in thermal equilibrium with any other surface material it is exposed to.
This is achieved simply by only allowing emission into the portion of the hemisphere exposed to sky (```SKYFAC```).
When you wish to disable this behavior and instead allow emission into the full hemisphere, include the following input line to set ```LHEMISEMIS``` to True:

`8 28 0 'TRUE' /` 

This will allow you to use flux tables to specify incoming flux from neighboring terrain while correctly accounting for the net exchange of energy.

<!-- ## Unused inputs
Due to the use of flux tables entirely replacing KRC's internal logic for determining incident flux, many traditional KRC inputs needed for such calculations instead go unused. 

This includes nearly all parameters controlling the body's orbital motion, with the crucial exceptions of ```PERIOD``` and ```DELJUL```, which relate the time indices to physical units as discussed above. 

~~Also unused are all parameters describing the relationship between the surface under consideration and the larger body. 
A surface modeled using a flux table has no latitude nor longitude, and has no relative slope.~~

Nor does it have an atmosphere, nor any relationship to the luminosity of the Sun. 

Such considerations are assumed to be relegated to whatever method is generating the flux table in the first place, and care should be taken to ensure all such factors are accounted for there. -->