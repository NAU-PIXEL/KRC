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
True_Emis_c_1em6 = np.loadtxt('True_emiss_c_1e-06.tab')
True_Emis_c_1em4 = np.loadtxt('True_emiss_c_0.0001.tab')
True_Emis_c_1em2 = np.loadtxt('True_emiss_c_0.01.tab')
True_Emis_c_1em0 = np.loadtxt('True_emiss_c_1.tab')
True_Emis_a_1em6 = np.loadtxt('True_emiss_a_1e-06.tab')
True_Emis_a_1em4 = np.loadtxt('True_emiss_a_0.0001.tab')
True_Emis_a_1em2 = np.loadtxt('True_emiss_a_0.01.tab')
True_Emis_a_1em0 = np.loadtxt('True_emiss_a_1.tab')


Fit_Emis_c_1em6 = np.loadtxt('Fit_emiss_c_1e-06.tab')
Fit_Emis_c_1em4 = np.loadtxt('Fit_emiss_c_0.0001.tab')
Fit_Emis_c_1em2 = np.loadtxt('Fit_emiss_c_0.01.tab')
Fit_Emis_c_1em0 = np.loadtxt('Fit_emiss_c_1.tab')
Fit_Emis_a_1em6 = np.loadtxt('Fit_emiss_a_1e-06.tab')
Fit_Emis_a_1em4 = np.loadtxt('Fit_emiss_a_0.0001.tab')
Fit_Emis_a_1em2 = np.loadtxt('Fit_emiss_a_0.01.tab')
Fit_Emis_a_1em0 = np.loadtxt('Fit_emiss_a_1.tab')

Temp = np.loadtxt('Temp.tab')

             

## Plot 
plt.figure(figsize=((11,11)))
plt.plot(Temp,True_Emis_c_1em6,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Fit_Emis_c_1em6,label = "Analytical Emissivity", marker = "x",color='red')
RMSE = np.sqrt(np.mean((Fit_Emis_c_1em6-True_Emis_c_1em6)**2))
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Emissivity')
plt.title('Crystalline, Grain Size = 1e-6 m,RMSE='+str(RMSE)[0:8])
plt.savefig('Emis_c_1em6.png',dpi=300)
plt.show()

plt.figure(figsize=((11,11)))
plt.plot(Temp,True_Emis_c_1em4,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Fit_Emis_c_1em4,label = "Analytical Emissivity", marker = "x",color='red')
RMSE = np.sqrt(np.mean((Fit_Emis_c_1em4-True_Emis_c_1em4)**2))
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Emissivity')
plt.title('Crystalline, Grain Size = 1e-4 m,RMSE='+str(RMSE)[0:8])
plt.savefig('Emis_c_1em4.png',dpi=300)
plt.show()

plt.figure(figsize=((11,11)))
plt.plot(Temp,True_Emis_c_1em2,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Fit_Emis_c_1em2,label = "Analytical Emissivity", marker = "x",color='red')
RMSE = np.sqrt(np.mean((Fit_Emis_c_1em2-True_Emis_c_1em2)**2))
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Emissivity')
plt.title('Crystalline, Grain Size = 1e-2 m,RMSE='+str(RMSE)[0:8])
plt.savefig('Emis_c_1em2.png',dpi=300)
plt.show()

plt.figure(figsize=((11,11)))
plt.plot(Temp,True_Emis_c_1em0,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Fit_Emis_c_1em0,label = "Analytical Emissivity", marker = "x",color='red')
RMSE = np.sqrt(np.mean((Fit_Emis_c_1em0-True_Emis_c_1em0)**2))
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Emissivity')
plt.title('Crystalline, Grain Size = 1 m,RMSE='+str(RMSE)[0:8])
plt.savefig('Emis_c_1em0.png',dpi=300)
plt.show()




plt.figure(figsize=((11,11)))
plt.plot(Temp,True_Emis_a_1em6,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Fit_Emis_a_1em6,label = "Analytical Emissivity", marker = "x",color='red')
RMSE = np.sqrt(np.mean((Fit_Emis_a_1em6-True_Emis_a_1em6)**2))
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Emissivity')
plt.title('Amorphous, Grain Size = 1e-6 m,RMSE='+str(RMSE)[0:8])
plt.savefig('Emis_a_1em6.png',dpi=300)
plt.show()

plt.figure(figsize=((11,11)))
plt.plot(Temp,True_Emis_a_1em4,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Fit_Emis_a_1em4,label = "Analytical Emissivity", marker = "x",color='red')
RMSE = np.sqrt(np.mean((Fit_Emis_a_1em4-True_Emis_a_1em4)**2))
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Emissivity')
plt.title('Amorphous, Grain Size = 1e-4 m,RMSE='+str(RMSE)[0:8])
plt.savefig('Emis_a_1em4.png',dpi=300)
plt.show()

plt.figure(figsize=((11,11)))
plt.plot(Temp,True_Emis_a_1em2,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Fit_Emis_a_1em2,label = "Analytical Emissivity", marker = "x",color='red')
RMSE = np.sqrt(np.mean((Fit_Emis_a_1em2-True_Emis_a_1em2)**2))
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Emissivity')
plt.title('Amorphous, Grain Size = 1e-2 m,RMSE='+str(RMSE)[0:8])
plt.savefig('Emis_a_1em2.png',dpi=300)
plt.show()

plt.figure(figsize=((11,11)))
plt.plot(Temp,True_Emis_a_1em0,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Fit_Emis_a_1em0,label = "Analytical Emissivity", marker = "x",color='red')
RMSE = np.sqrt(np.mean((Fit_Emis_a_1em0-True_Emis_a_1em0)**2))
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Emissivity')
plt.title('Amorphous, Grain Size = 1 m,RMSE='+str(RMSE)[0:8])
plt.savefig('Emis_a_1em0.png',dpi=300)
plt.show()
