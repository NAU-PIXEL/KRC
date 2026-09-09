import numpy as np 
import matplotlib.pyplot as plt  
import PyMieScatt as ps
from tqdm import tqdm 


def compute_ssa_and_emissivity(wn, refractive_index, grain_size):
    """
    Computes Mie scattering albedos and hemispherical emissivity, including diffraction correction
    Parameters:
    ----------
    wn : array of float
        wavenumber (cm^-1)
    refractive_index : array of complex (should be the same len as wn)
        Refractive index at a given wavenumber
    grain_size : float
        Radius of the regolith (in m)
    Returns:
    -------
    SSA : array of float
       Single scattering albedo 
    SSA_eddcor : array of float
        Single scattering albedo , corrected from diffraction using the delta-Eddington appromiation
    emis : array of float
        Hemispherical emissivity computed using SSA 
    emiss_eddcor : array of float
        Hemispherical emissivity computed using SSSA_eddcorSA 
    """
    if(len(wn) != len(refractive_index)):
       raise ValueError('Fields "wn" and "refractive_index" do not have the same length')
    R_nm = 2*grain_size*1e9  # convert grain size in nm. Factor of 2 because the algorithm work with the diameter and not radius
    SSA = 0*wn               # Mie Scatterring Albedo
    SSA_eddcor = 0*wn        # Mie Scatterring Albedo, after eddigton correction for diffraction
    emis = 0*wn              # Hemispherical emissivity of the ice
    emiss_eddcor = 0*wn      # Hemispherical emissivity of the ice, after eddigton correction for diffraction
    lambdan = 1e7/wn         # convert wavenumber in wavelength, in nm
    for i in range(len(wn)):
        output = ps.AutoMieQ(refractive_index[i], lambdan[i], R_nm)
        alb = output[1]/output[0]
        g = output[3]
        alb_corr = (1-g**2)/(1-g**2*alb)*alb
        gamma = np.sqrt(1-alb)
        gamma_corr = np.sqrt(1-alb_corr)
        r = (1-gamma)/(1+gamma)
        r_corr = (1-gamma_corr)/(1+gamma_corr)
        eh = 2*gamma/(1+gamma)*(1+r/6)
        eh_corr = 2*gamma_corr/(1+gamma_corr)*(1+r_corr/6)
        SSA[i] = alb
        SSA_eddcor[i] = alb_corr
        emis[i] = eh
        emiss_eddcor[i] = eh_corr   
    return(SSA,SSA_eddcor,emis,emiss_eddcor)


def compute_emissivity(wn, refractive_index, grain_size):
    """
    Computes Mie scattering albedos and hemispherical emissivity, including diffraction correction
    Parameters:
    ----------
    wn : array of float
        wavenumber (cm^-1)
    refractive_index : array of complex (should be the same len as wn)
        Refractive index at a given wavenumber
    grain_size : float
        Radius of the regolith (in m)
    Returns:
    -------
    emiss_eddcor : array of float
        Hemispherical emissivity computed with Eddigton correction 
    """
    if(len(wn) != len(refractive_index)):
       raise ValueError('Fields "wn" and "refractive_index" do not have the same length')
    R_nm = 2*grain_size*1e9  # convert grain size in nm. Factor of 2 because the algorithm work with the diameter and not radius
    emiss_eddcor = 0*wn      # Hemispherical emissivity of the ice, after eddigton correction for diffraction
    lambdan = 1e7/wn         # convert wavenumber in wavelength, in nm
    for i in range(len(wn)):
        output = ps.AutoMieQ(refractive_index[i], lambdan[i], R_nm)
        alb = output[1]/output[0]
        g = output[3]
        alb_corr = (1-g**2)/(1-g**2*alb)*alb
        gamma = np.sqrt(1-alb)
        gamma_corr = np.sqrt(1-alb_corr)
        r = (1-gamma)/(1+gamma)
        r_corr = (1-gamma_corr)/(1+gamma_corr)
        eh = 2*gamma/(1+gamma)*(1+r/6)
        eh_corr = 2*gamma_corr/(1+gamma_corr)*(1+r_corr/6)
        emiss_eddcor[i] = eh_corr   
    return(emiss_eddcor)




wn = np.loadtxt('Syntheticsppectrum_wavenumber.txt')
Syntheticsppectrum_real_nr_crystalline = np.loadtxt('Syntheticsppectrum_real_nr_crystalline.txt')
Syntheticsppectrum_real_nr_amorphous = np.loadtxt('Syntheticsppectrum_real_nr_amorphous.txt')

Syntheticsppectrum_imag_nk_crystalline = np.loadtxt('Syntheticsppectrum_imag_nk_crystalline.txt')
Syntheticsppectrum_imag_nk_amorphous = np.loadtxt('Syntheticsppectrum_imag_nk_amorphous.txt')

refractive_amorphous = Syntheticsppectrum_real_nr_amorphous+Syntheticsppectrum_imag_nk_amorphous*1j
refractive_crystalline = Syntheticsppectrum_real_nr_crystalline+Syntheticsppectrum_imag_nk_crystalline*1j


SSA_5cm,SSA_eddcor_5cm,emis_5cm,emis_eddcor_5cm = compute_ssa_and_emissivity(wn, refractive_crystalline, 5e-2)
SSA_2mm,SSA_eddcor_2mm,emis_2mm,emis_eddcor_2mm = compute_ssa_and_emissivity(wn, refractive_crystalline, 2e-3)
SSA_100micro,SSA_eddcor_100micro,emis_100micro,emis_eddcor_100micro = compute_ssa_and_emissivity(wn, refractive_crystalline, 100e-6)
SSA_10micro,SSA_eddcor_10micro,emis_10micro,emis_eddcor_10micro = compute_ssa_and_emissivity(wn, refractive_crystalline, 10e-6)

## Load Dta From Ferrari 2024

emiss_fer_10micro = np.loadtxt('10microms_emis.csv')
emiss_fer_10micro_cor = np.loadtxt('10microms_emis_cor.csv')
emiss_fer_5cm = np.loadtxt('5cm_emis.csv')
emiss_fer_5cm_cor = np.loadtxt('5cm_emis_cor.csv')

ssa_fer_10micro = np.loadtxt('10microms_mie.csv')
ssa_fer_10micro_cor = np.loadtxt('10microms_mie_cor.csv')
ssa_fer_5cm = np.loadtxt('5cm_mie.csv')
ssa_fer_5cm_cor = np.loadtxt('5cm_mie_cor.csv')

lambdan = 1e7/wn 



plt.figure(figsize=((10,6)))
# Measured data
plt.scatter(ssa_fer_5cm[:,0], ssa_fer_5cm[:,1], marker='o', color='k', label='Ferrari (2024) - 5 cm ')
plt.scatter(ssa_fer_5cm_cor[:,0], ssa_fer_5cm_cor[:,1], marker='x', color='k', label='Ferrari (2024) - 5 cm - corrected')

plt.scatter(ssa_fer_10micro[:,0], ssa_fer_10micro[:,1], marker='o', color='b', label='Ferrari (2024) -  10 μm - measured')
plt.scatter(ssa_fer_10micro_cor[:,0], ssa_fer_10micro_cor[:,1], marker='x', color='b', label='Ferrari (2024) -  10 μm - corrected')

# Model data
plt.plot(lambdan * 1e-9 * 1e6, SSA_5cm, color='k', linestyle='-', label='5 cm - model')
plt.plot(lambdan * 1e-9 * 1e6, SSA_eddcor_5cm, color='k', linestyle='--', label='5 cm - model corrected')

plt.plot(lambdan * 1e-9 * 1e6, SSA_10micro, color='b', linestyle='-', label='10 μm - model')
plt.plot(lambdan * 1e-9 * 1e6, SSA_eddcor_10micro, color='b', linestyle='--', label='10 μm - model corrected')

# Axes, legend, and formatting
plt.xlim((5, 14))
plt.xlabel("Wavelength (μm)")
plt.ylabel("Single Scattering Albedo")
plt.legend()
plt.tight_layout()
plt.savefig('Comp_Ferrari_ssa.png',dpi=300)
plt.show()







plt.figure(figsize=((10,6)))

# Measured data
plt.scatter(emiss_fer_5cm[:,0], emiss_fer_5cm[:,1], marker='o', color='k', label='Ferrari (2024) - 5 cm ')
plt.scatter(emiss_fer_5cm_cor[:,0], emiss_fer_5cm_cor[:,1], marker='x', color='k', label='Ferrari (2024) - 5 cm - corrected')

plt.scatter(emiss_fer_10micro[:,0], emiss_fer_10micro[:,1], marker='o', color='b', label='Ferrari (2024) -  10 μm - measured')
plt.scatter(emiss_fer_10micro_cor[:,0], emiss_fer_10micro_cor[:,1], marker='x', color='b', label='Ferrari (2024) -  10 μm - corrected')

# Model data
plt.plot(lambdan * 1e-9 * 1e6, emis_5cm, color='k', linestyle='-', label='5 cm - model')
plt.plot(lambdan * 1e-9 * 1e6, emis_eddcor_5cm, color='k', linestyle='--', label='5 cm - model corrected')

plt.plot(lambdan * 1e-9 * 1e6, emis_10micro, color='b', linestyle='-', label='10 μm - model')
plt.plot(lambdan * 1e-9 * 1e6, emis_eddcor_10micro, color='b', linestyle='--', label='10 μm - model corrected')

# Axes, legend, and formatting 
plt.xlim((5, 14))
plt.xlabel("Wavelength (μm)")
plt.ylabel("Emissivity")
plt.legend()
plt.tight_layout()
plt.savefig('Comp_Ferrari_emis.png',dpi=300)

plt.show()






Radius = [1e-6,2.5e-6,5e-6, 7.5e-6,1e-5,2.5e-5,5e-5, 7.5e-5,1e-4,2.5e-4,5e-4, 7.5e-4,1e-3,2.5e-3,5e-3, 7.5e-3,1e-2,2.5e-2,5e-2, 7.5e-2,1e-1,2.5e-1,5e-1, 7.5e-1,1]
emis_crystalline = np.zeros((len(Radius),len(wn)))
emis_amorphous = np.zeros((len(Radius),len(wn)))

for i in tqdm(range(len(Radius))): 
    emis_crystalline[i,:] = compute_emissivity(wn, refractive_crystalline, Radius[i])
    emis_amorphous[i,:] = compute_emissivity(wn, refractive_amorphous, Radius[i])

np.savetxt('Radius_emissivity.txt',Radius)
np.savetxt('emis_crystalline_ice.txt',emis_crystalline)
np.savetxt('emis_amorphous_ice.txt',emis_amorphous)
