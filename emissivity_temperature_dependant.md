Use a Temperature-Dependent Emissivity in KRC
========================================

The basic version of **KRC** uses a fixed emissivity value for the ground.  
However, as the emissivity of a material can vary with wavelength, one may want to reproduce this behavior in KRC.  

This new version allows such procedure, and the temperature dependence of the emissivity $\epsilon$ can be described by a 3$^{\mathrm{rd}}$ order polynomial function:

$$
\epsilon(T) = \epsilon_0 + \epsilon_1 \, x + \epsilon_2 \, x^2 + \epsilon_3 \, x^3
$$

where:

$$
x = (T - 220) \times 0.01
$$

and:
- $T$ is the surface temperature (**K**)  
- $\epsilon$ is the emissivity (dimensionless)  
- $\epsilon_0, \epsilon_1, \epsilon_2, \epsilon_3$ are polynomial coefficients 

---

Modifications in KRC Input File
========================================

To use this new capability, modify the KRC input file as follows:

- Add the line: "18 1 X 'EmisT' /" where **X** must be set to `0` for a fixed emissivity or `1` for a temperature-dependent emissivity.

- Add the line: "18 2 Y0 'Emis0' /" where **Y0** is the value of $\epsilon_0$.

- Add the line: "18 3 Y1 'Emis1' /" where **Y1** is the value of $\epsilon_1$.

- Add the line: "18 4 Y2 'Emis1' /" where **Y1** is the value of $\epsilon_2$.

- Add the line: "18 5 Y3 'Emis1' /" where **Y1** is the value of $\epsilon_3$.


---

## Modifications in the DaVinci Interface

To use this new capability with the KRC interface, add the following to the KRC call:

```r
krc(lat = ..., EmisT = .., Emis0 = .., Emis1 = .., Emis2 = .., Emis3 = ..)

where:

- EmisT is a boolean set to 0 for a fixed emissivity or 1 for a temperature-dependent emissivity.

- Emis0 is the value of $\epsilon_0$.

- Emis1 is the value of $\epsilon_1$.

- Emis2 is the value of $\epsilon_2$.

- Emis3 is the value of $\epsilon_3$.

Special Considerations
========================================

Note that at the moment, if frost is forming at the surface, then a constant emissivity is used. 
