Enabling Penetration of Solar Flux in the Ground
========================================

The current version of **KRC** solves the following problem:

$$ 
\rho C \frac{\partial T_{\rm{soil}}}{\partial t}= \nabla \big( k(z) \nabla T_{\rm{soil}}(z) \big)
$$

$$
\epsilon \sigma T_{\rm{surf}}^4 = S_0 (1-A) - k \frac{\partial T_{\rm{soil}}}{\partial z}\bigg|_{z=0}
$$

where:  
- $\rho$ is the density (**kg·m⁻³**)  
- $C$ is the heat capacity (**J·kg⁻¹·K⁻¹**)  
- $T_{\rm{soil}}$ is the ground temperature (**K**)  
- $k(z)$ is the thermal conductivity at depth $z$ (**W·m⁻¹·K⁻¹**)  
- $\epsilon$ is the emissivity (dimensionless)  
- $\sigma$ is the Stefan–Boltzmann constant (**W·m⁻²·K⁻⁴**)  
- $T_{\rm{surf}}$ is the surface temperature (**K**)  
- $S_0$ is the solar flux reaching the surface (**W·m⁻²**)  
- $A$ is the albedo (dimensionless)  

*(Note: atmospheric fluxes, such as infrared fluxes, can be added in the last equation.)*

---

Solar Radiation Penetration
========================================

In this version, we allow the solar radiation to penetrate into the ground. The new problem is:

$$
\rho C \frac{\partial T_{\rm{soil}}}{\partial t}= \nabla \big( k(z) \nabla T_{\rm{soil}}(z) \big) - \frac{\partial S(z)}{\partial z}
$$

$$
\epsilon \sigma T_{\rm{surf}}^4 = - k \frac{\partial T_{\rm{soil}}}{\partial z}\bigg|_{z=0}
$$

with:

$$
S(z) = S_0 (1-A) e^{-z/\delta}
$$

$$
\delta = \delta_0 \cos(i)
$$

where:  
- $\delta_0$ is the e-folding depth for solar penetration (**m**)  
- $i$ is the solar incidence angle (**rad**)  

---

Modifications in KRC Input File
========================================

To use this new capability, one must change the KRC input file accordingly:

- Add the line: "17 1 X 'RADGND' /" where **X** must be set to `0` (solar energy deposited only at the surface) or `1` (solar energy distributed in the ground).

- Add the line: "17 2 Y 'EFOLD_RADGND' /" where **Y** must be set to the value of the e-folding depth $\delta_0$ (**m**, typically a few centimeters).

---

 Modifications in the DaVinci Interface
========================================

To use this new capability with the KRC interface, add the following to the KRC call:

"krc(lat = ..., RADGND, EFOLD_RADGND)"

where:
- `RADGND` is a boolean set to `0` (solar energy at surface) or `1` (solar energy distributed in the ground).
- `EFOLD_RADGND` is the e-folding depth $\delta_0$ (**m**, typically a few centimeters).

---

Special Considerations
========================================

For very low values of `EFOLD_RADGND` ($\delta_0$), KRC might be slower than usual because small $\delta_0$ values require a very fine grid for an accurate solution.

