import numpy as np
import matplotlib.pyplot as plt

plt.rcParams.update({'font.size': 22})
plt.rcParams['axes.linewidth'] = 2 # set the value globally
plt.rcParams['ytick.major.size'] = 6
plt.rcParams['ytick.major.width'] = 2
plt.rcParams['xtick.major.size'] = 6
plt.rcParams['xtick.major.width'] = 2
plt.rcParams['xtick.minor.visible'] = True
plt.rcParams['xtick.minor.size'] = 4
plt.rcParams['xtick.minor.width'] = 1.5
plt.rcParams['ytick.minor.visible'] = True
plt.rcParams['ytick.minor.size'] = 4
plt.rcParams['ytick.minor.width'] = 1.5

## load data 
True_Emis_c_1em6 = np.loadtxt('True_emiss_c_1e-06.txt')
True_Emis_c_1em4 = np.loadtxt('True_emiss_c_0.0001.txt')
True_Emis_c_1em2 = np.loadtxt('True_emiss_c_0.01.txt')
True_Emis_c_1em0 = np.loadtxt('True_emiss_c_1.txt')
True_Emis_a_1em6 = np.loadtxt('True_emiss_a_1e-06.txt')
True_Emis_a_1em4 = np.loadtxt('True_emiss_a_0.0001.txt')
True_Emis_a_1em2 = np.loadtxt('True_emiss_a_0.01.txt')
True_Emis_a_1em0 = np.loadtxt('True_emiss_a_1.txt')


Fit_Emis_c_1em6 = np.loadtxt('Fit_emiss_c_1e-06.txt')
Fit_Emis_c_1em4 = np.loadtxt('Fit_emiss_c_0.0001.txt')
Fit_Emis_c_1em2 = np.loadtxt('Fit_emiss_c_0.01.txt')
Fit_Emis_c_1em0 = np.loadtxt('Fit_emiss_c_1.txt')
Fit_Emis_a_1em6 = np.loadtxt('Fit_emiss_a_1e-06.txt')
Fit_Emis_a_1em4 = np.loadtxt('Fit_emiss_a_0.0001.txt')
Fit_Emis_a_1em2 = np.loadtxt('Fit_emiss_a_0.01.txt')
Fit_Emis_a_1em0 = np.loadtxt('Fit_emiss_a_1.txt')

Temp = np.loadtxt('Temp.tab')

## Plot 
plt.figure(figsize=((12,12)))
plt.plot(Temp,True_Emis_c_1em6,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Fit_Emis_c_1em6,label = "Analytical Emissivity", marker = "x",color='red')
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Emissivity')
plt.title('Crystalline, Grain Size = 1e-6 m')
plt.savefig('Emis_c_1em6.png',dpi=300)
plt.show()

