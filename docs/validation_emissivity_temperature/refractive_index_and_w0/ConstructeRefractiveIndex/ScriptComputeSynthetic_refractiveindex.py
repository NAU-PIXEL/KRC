import numpy as np
from scipy.interpolate import interp1d
import matplotlib.pyplot as plt


## Load data

# note: for the imaginary part, I manually performed a pre-processing stage where some points have been added through linear inerpolation around 1050 cm-1. The file contained these points
Hudgins_real_amorphous = np.loadtxt('Hudgins_real_amorphous.txt')
Hudgins_imag_amorphous = np.loadtxt('Hudgins_imag_amorphous.txt')

Hudgins_real_cryst = np.loadtxt('Hudgins_real_cryst.txt')
Hudgins_imag_cryst = np.loadtxt('Hudgins_imag_cryst.txt')

Curtis_amorphous_imag = np.loadtxt('Curtis_amorphous_imag.txt')
Curtis_amorphous_real = np.loadtxt('Curtis_amorphous_real.txt')

Curtis_crystaline_imag = np.loadtxt('Curtis_crystaline_imag.txt')
Curtis_crystaline_real = np.loadtxt('Curtis_crystaline_real.txt')

# concatenate everything
# *_nr : real part of refractive index
# *_nk : imaginary part of refractive index


data_amorphous_wn_nr = np.concatenate((Curtis_amorphous_real[:,0],Hudgins_real_amorphous[:,0]))
data_amorphous_nr = np.concatenate((Curtis_amorphous_real[:,1],Hudgins_real_amorphous[:,1]))
data_amorphous_wn_nk = np.concatenate((Curtis_amorphous_imag[:,0],Hudgins_imag_amorphous[:,0]))
data_amorphous_nk = np.concatenate((Curtis_amorphous_imag[:,1],Hudgins_imag_amorphous[:,1]))

data_crystaline_wn_nr = np.concatenate((Curtis_crystaline_real[:,0],Hudgins_real_cryst[:,0]))
data_crystaline_nr = np.concatenate((Curtis_crystaline_real[:,1],Hudgins_real_cryst[:,1]))
data_crystaline_wn_nk = np.concatenate((Curtis_crystaline_imag[:,0],Hudgins_imag_cryst[:,0]))
data_crystaline_nk = np.concatenate((Curtis_crystaline_imag[:,1],Hudgins_imag_cryst[:,1]))

# build regular wavenumber vector

common_wn = np.arange(np.max([data_amorphous_wn_nr[0],data_crystaline_wn_nr[0]]),np.min([data_amorphous_wn_nr[-1],data_crystaline_wn_nr[-1]]),4.) #*_nr have the same edges as *_nk, the sampling just change because of the processing for imagineray around 1050 cm^-1
common_wn = np.arange(np.max([data_amorphous_wn_nr[0],data_crystaline_wn_nr[0]]),1900,4.) #*_nr have the same edges as *_nk, the sampling just change because of the processing for imagineray around 1050 cm^-1

# Now build interpolated spectrum
f_cubic = interp1d(data_amorphous_wn_nr, data_amorphous_nr, kind='cubic')
common_amorphous_nr = f_cubic(common_wn)

f_cubic = interp1d(data_amorphous_wn_nk, data_amorphous_nk, kind='cubic')
common_amorphous_nk = f_cubic(common_wn)

f_cubic = interp1d(data_crystaline_wn_nr, data_crystaline_nr, kind='cubic')
common_crystalline_nr = f_cubic(common_wn)

f_cubic = interp1d(data_crystaline_wn_nk, data_crystaline_nk, kind='cubic')
common_crystalline_nk = f_cubic(common_wn)


# Visual check
plt.loglog(data_amorphous_wn_nr, data_amorphous_nr, 'o', label='Processed Data')
plt.loglog(common_wn, common_amorphous_nr, '-', label='Interpolated Data')
plt.legend()
plt.show()
                  
plt.loglog(data_amorphous_wn_nk, data_amorphous_nk, 'o', label='Processed Data')
plt.loglog(common_wn, common_amorphous_nk, 'o', label='Interpolated Data')
plt.legend()
plt.show()

plt.loglog(data_crystaline_wn_nr, data_crystaline_nr, 'o', label='Processed Data')
plt.loglog(common_wn, common_crystalline_nr, '-', label='Interpolated Data')
plt.legend()
plt.show()
                  
plt.loglog(data_crystaline_wn_nk, data_crystaline_nk, 'o', label='Processed Data')
plt.loglog(common_wn, common_crystalline_nk, '-', label='Interpolated Data')
plt.legend()
plt.show()

# Now compare with actual data
#                       

Hudgins_ima_raw = np.loadtxt('Hudgins_ima_raw.txt')
Hudgins_rea_raw = np.loadtxt('Hudgins_rea_raw.txt')
Curtis_crystaline_imag_raw = np.loadtxt('Curtis_crystaline_imag_raw.txt')
Curtis_crystaline_real_raw = np.loadtxt('Curtis_crystaline_real_raw.txt')
Curtis_amorphous_imag_raw = np.loadtxt('Curtis_amorphous_imag_raw.txt')
Curtis_amorphous_real_raw = np.loadtxt('Curtis_amorphous_real_raw.txt')

plt.figure(figsize=(12,5))
plt.loglog(Hudgins_rea_raw[:,0], Hudgins_rea_raw[:,1], 'o', label='Hudgins et al. (1993) - Amorphous',color='b')
plt.loglog(Curtis_amorphous_real_raw[:,0], Curtis_amorphous_real_raw[:,1], 'o', label='Curtis et al. (1993) - Amorphous',color='r')
plt.loglog(common_wn, common_amorphous_nr, '-', label='Reference Spectrum',color='k')
plt.legend()
plt.xlabel('Wavenumber (cm$^{-1}$)')
plt.ylabel('Real part of the refractive index')
plt.xlim((50,1900))
plt.title('Amorphous ice')
plt.savefig('Spectrum_amorphous_nr.png',dpi=300)
plt.show()

plt.figure(figsize=(12,5))
plt.loglog(Hudgins_ima_raw[:,0], Hudgins_ima_raw[:,1], 'o', label='Hudgins et al. (1993) - Amorphous',color='b')
plt.loglog(Curtis_amorphous_imag_raw[:,0], Curtis_amorphous_imag_raw[:,1], 'o', label='Curtis et al. (1993) - Amorphous',color='r')
plt.loglog(common_wn, common_amorphous_nk, '-', label='Reference Spectrum',color='k')
plt.legend()
plt.xlabel('Wavenumber (cm$^{-1}$)')
plt.ylabel('Imaginary part of the refractive index')
plt.title('Amorphous ice')
plt.xlim((50,1900))
plt.ylim((1e-4,2))
plt.savefig('Spectrum_amorphous_nk.png',dpi=300)
plt.show()


plt.figure(figsize=(12,5))
plt.loglog(Hudgins_rea_raw[:,0], Hudgins_rea_raw[:,1], 'o', label='Hudgins et al. (1993) - Amorphous',color='b')
plt.loglog(Curtis_crystaline_real_raw[:,0], Curtis_crystaline_real_raw[:,1], 'o', label='Curtis et al. (1993) - Crystalline',color='r')
plt.loglog(common_wn, common_crystalline_nr, '-', label='Reference Spectrum',color='k')
plt.legend()
plt.xlabel('Wavenumber (cm$^{-1}$)')
plt.ylabel('Real part of the refractive index')
plt.xlim((50,1900))
plt.title('Crystalline ice')
plt.savefig('Spectrum_crystalline_nr.png',dpi=300)
plt.show()

plt.figure(figsize=(12,5))
plt.loglog(Hudgins_ima_raw[:,0], Hudgins_ima_raw[:,1], 'o', label='Hudgins et al. (1993) - Amorphous',color='b')
plt.loglog(Curtis_crystaline_imag_raw[:,0], Curtis_crystaline_imag_raw[:,1], 'o', label='Curtis et al. (1993) - Crystalline',color='r')
plt.loglog(common_wn, common_crystalline_nk, '-', label='Reference Spectrum',color='k')
plt.legend()
plt.xlabel('Wavenumber (cm$^{-1}$)')
plt.ylabel('Imaginary part of the refractive index')
plt.title('Crystalline ice')
plt.xlim((50,1900))
plt.ylim((1e-4,2))
plt.savefig('Spectrum_crystalline_nk.png',dpi=300)
plt.show()



# Validation with Ferrari 2024 for crystalline imaginary part
Fer_nk = np.loadtxt('FerrariData_nk.txt')
Fer_nr = np.loadtxt('FerrariData_nr.txt')

plt.figure(figsize=(12,5))
plt.loglog(Fer_nk[:,0], Fer_nk[:,1], 'o', label='Ferrari (2024)',color='b')
plt.loglog(common_wn, common_crystalline_nk, '-', label='Reference Spectrum',color='k')
plt.legend()
plt.xlabel('Wavenumber (cm$^{-1}$)')
plt.ylabel('Imaginary part of the refractive index')
plt.title('Crystalline ice')
plt.xlim((50,1900))
plt.ylim((1e-4,2))
plt.savefig('Spectrum_crystalline_nk_validationwithFer.png',dpi=300)




plt.figure(figsize=(12,5))
plt.loglog(Fer_nr[:,0], Fer_nr[:,1], 'o', label='Ferrari (2024)',color='b')
plt.loglog(common_wn, common_crystalline_nr, '-', label='Reference Spectrum',color='k')
plt.legend()
plt.xlabel('Wavenumber (cm$^{-1}$)')
plt.ylabel('Real part of the refractive index')
plt.title('Crystalline ice')
plt.xlim((50,1900))
plt.savefig('Spectrum_crystalline_nr_validationwithFer.png',dpi=300)
