#! /bin/bash/python

# defines constants used by pyorb

# PORB version date:
porb_version = '2025nov20'

##### Constants #####
#earth sidereal year in Earth days 
earth_year = 365.256363004
# EOBL  : Earth's obliquity, in radians
earth_obliquity = 0.409092601        # ecliptic obliquity 84381.406" in radians IAU2009
# Full julian date of 2000.0
j2000_JD = 2451545.0
# 1 AU in km
km_per_au = 149597870.6996262
# solar mass parameter in km^3/s^2:
mu = 1.32712440042 * 10**11

##### Unused flags #####
# SFLAG : Input system flag 0:default, +1: pole=ec +10:orb=eq
sflag   = 0
# PBUG  : Debug Flag
pbug    = 0
# spar17: unused
spar17  = 0
# spar21: unused
spar21  = 0