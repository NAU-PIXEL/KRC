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

## Fit

Fitcond_GJRKBB_a_0_26_0_1 = np.loadtxt('Fitcond_GJRKBB_a_0.26_0.1_.tab')
Fitcond_GJRKBB_a_0_26_0_0001 = np.loadtxt('Fitcond_GJRKBB_a_0.26_0.0001_.tab')
Fitcond_GJRKBB_a_0_84_0_1 = np.loadtxt('Fitcond_GJRKBB_a_0.84_0.1_.tab')
Fitcond_GJRKBB_a_0_84_0_0001 = np.loadtxt('Fitcond_GJRKBB_a_0.84_0.0001_.tab')

Fitcond_GJRKBB_c_0_26_0_1 = np.loadtxt('Fitcond_GJRKBB_c_0.26_0.1_.tab')
Fitcond_GJRKBB_c_0_26_0_0001 = np.loadtxt('Fitcond_GJRKBB_c_0.26_0.0001_.tab')
Fitcond_GJRKBB_c_0_84_0_1 = np.loadtxt('Fitcond_GJRKBB_c_0.84_0.1_.tab')
Fitcond_GJRKBB_c_0_84_0_0001 = np.loadtxt('Fitcond_GJRKBB_c_0.84_0.0001_.tab')

Fitcond_WBB_a_0_26_0_1 = np.loadtxt('Fitcond_WBB_a_0.26_0.1_.tab')
Fitcond_WBB_a_0_26_0_0001 = np.loadtxt('Fitcond_WBB_a_0.26_0.0001_.tab')
Fitcond_WBB_a_0_84_0_1 = np.loadtxt('Fitcond_WBB_a_0.84_0.1_.tab')
Fitcond_WBB_a_0_84_0_0001 = np.loadtxt('Fitcond_WBB_a_0.84_0.0001_.tab')

Fitcond_WBB_c_0_26_0_1 = np.loadtxt('Fitcond_WBB_c_0.26_0.1_.tab')
Fitcond_WBB_c_0_26_0_0001 = np.loadtxt('Fitcond_WBB_c_0.26_0.0001_.tab')
Fitcond_WBB_c_0_84_0_1 = np.loadtxt('Fitcond_WBB_c_0.84_0.1_.tab')
Fitcond_WBB_c_0_84_0_0001 = np.loadtxt('Fitcond_WBB_c_0.84_0.0001_.tab')


## True

Truecond_GJRKBB_a_0_26_0_1 = np.loadtxt('Truecond_GJRKBB_a_0.26_0.1_.tab')
Truecond_GJRKBB_a_0_26_0_0001 = np.loadtxt('Truecond_GJRKBB_a_0.26_0.0001_.tab')
Truecond_GJRKBB_a_0_84_0_1 = np.loadtxt('Truecond_GJRKBB_a_0.84_0.1_.tab')
Truecond_GJRKBB_a_0_84_0_0001 = np.loadtxt('Truecond_GJRKBB_a_0.84_0.0001_.tab')

Truecond_GJRKBB_c_0_26_0_1 = np.loadtxt('Truecond_GJRKBB_c_0.26_0.1_.tab')
Truecond_GJRKBB_c_0_26_0_0001 = np.loadtxt('Truecond_GJRKBB_c_0.26_0.0001_.tab')
Truecond_GJRKBB_c_0_84_0_1 = np.loadtxt('Truecond_GJRKBB_c_0.84_0.1_.tab')
Truecond_GJRKBB_c_0_84_0_0001 = np.loadtxt('Truecond_GJRKBB_c_0.84_0.0001_.tab')

Truecond_WBB_a_0_26_0_1 = np.loadtxt('Truecond_WBB_a_0.26_0.1_.tab')
Truecond_WBB_a_0_26_0_0001 = np.loadtxt('Truecond_WBB_a_0.26_0.0001_.tab')
Truecond_WBB_a_0_84_0_1 = np.loadtxt('Truecond_WBB_a_0.84_0.1_.tab')
Truecond_WBB_a_0_84_0_0001 = np.loadtxt('Truecond_WBB_a_0.84_0.0001_.tab')

Truecond_WBB_c_0_26_0_1 = np.loadtxt('Truecond_WBB_c_0.26_0.1_.tab')
Truecond_WBB_c_0_26_0_0001 = np.loadtxt('Truecond_WBB_c_0.26_0.0001_.tab')
Truecond_WBB_c_0_84_0_1 = np.loadtxt('Truecond_WBB_c_0.84_0.1_.tab')
Truecond_WBB_c_0_84_0_0001 = np.loadtxt('Truecond_WBB_c_0.84_0.0001_.tab')

Temp = np.loadtxt('Temp.tab')

## Plot 
plt.figure(figsize=((12,12)))
plt.plot(Temp,Fitcond_GJRKBB_a_0_26_0_1,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Truecond_GJRKBB_a_0_26_0_1,label = "Analytical Conductivity", marker = "x",color='red')

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (W/m/K)')
plt.title('Tigh Contact, Amorphous, R = 0.1 m, Porosity = 0.26')
plt.savefig('Figures/TighContact_Amorphous_R_0_1m_P_0_26.png',dpi=300)
plt.show()


plt.figure(figsize=((12,12)))
plt.plot(Temp,Fitcond_GJRKBB_c_0_26_0_1,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Truecond_GJRKBB_c_0_26_0_1,label = "Analytical Conductivity", marker = "x",color='red')

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (W/m/K)')
plt.title('Tigh Contact, Crystalline, R = 0.1 m, Porosity = 0.26')
plt.savefig('Figures/TighContact_Crystalline_R_0_1m_P_0_26.png',dpi=300)
plt.show()



plt.figure(figsize=((12,12)))
plt.plot(Temp,Fitcond_GJRKBB_a_0_26_0_0001,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Truecond_GJRKBB_a_0_26_0_0001,label = "Analytical Conductivity", marker = "x",color='red')

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (W/m/K)')
plt.title('Tigh Contact, Amorphous, R = 0.0001 m, Porosity = 0.26')
plt.savefig('Figures/TighContact_Amorphous_R_0_0001m_P_0_26.png',dpi=300)
plt.show()



## Plot 
plt.figure(figsize=((12,12)))
plt.plot(Temp,Fitcond_WBB_a_0_26_0_1,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Truecond_WBB_a_0_26_0_1,label = "Analytical Conductivity", marker = "x",color='red')

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (W/m/K)')
plt.title('Looose Contact, Amorphous, R = 0.1 m, Porosity = 0.26')
plt.savefig('Figures/LooseContact_Amorphous_R_0_1m_P_0_26.png',dpi=300)
plt.show()


plt.figure(figsize=((12,12)))
plt.plot(Temp,Fitcond_WBB_c_0_26_0_1,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Truecond_WBB_c_0_26_0_1,label = "Analytical Conductivity", marker = "x",color='red')

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (W/m/K)')
plt.title('Looose Contact, Crystalline, R = 0.1 m, Porosity = 0.26')
plt.savefig('Figures/LooseContact_Crystalline_R_0_1m_P_0_26.png',dpi=300)
plt.show()





## Effect of large temperature ##

NewFitcond_GJRKBB_a_0_84_0_0001 = np.loadtxt('newFitcond_GJRKBB_a_0.84_0.0001.tab')
NewFitcond_GJRKBB_c_0_84_0_0001 = np.loadtxt('newFitcond_GJRKBB_c_0.84_0.0001.tab')
NewFitcond_WBB_a_0_84_0_0001 = np.loadtxt('newFitcond_WBB_a_0.84_0.0001.tab')
NewFitcond_WBB_c_0_84_0_0001 = np.loadtxt('newFitcond_WBB_c_0.84_0.0001.tab')


## True

NewTruecond_GJRKBB_a_0_84_0_0001 = np.loadtxt('newTruecond_GJRKBB_a_0.84_0.0001.tab')
NewTruecond_GJRKBB_c_0_84_0_0001 = np.loadtxt('newTruecond_GJRKBB_c_0.84_0.0001.tab')
NewTruecond_WBB_a_0_84_0_0001 = np.loadtxt('newTruecond_WBB_a_0.84_0.0001.tab')
NewTruecond_WBB_c_0_84_0_0001 = np.loadtxt('newTruecond_WBB_c_0.84_0.0001.tab')

NewTemp = np.loadtxt('NewTemp.tab')


plt.figure(figsize=((12,12)))
plt.plot(NewTemp,NewFitcond_WBB_a_0_84_0_0001,label = "Fit", c='black',linewidth=3)
plt.scatter(NewTemp,NewTruecond_WBB_a_0_84_0_0001,label = "Analytical Conductivity", marker = "x",color='red')

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (W/m/K)')
plt.title('New Looose Contact, Amorphous, R = 0.0001 m, Porosity = 0.84')
plt.savefig('Figures/NewLooseContact_Amorphous_R_0_0001m_P_0_84.png',dpi=300)
plt.show()


plt.figure(figsize=((12,12)))
plt.plot(NewTemp,NewFitcond_WBB_c_0_84_0_0001,label = "Fit", c='black',linewidth=3)
plt.scatter(NewTemp,NewTruecond_WBB_c_0_84_0_0001,label = "Analytical Conductivity", marker = "x",color='red')

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (W/m/K)')
plt.title('New Looose Contact, Crystalline, R = 0.0001 m, Porosity = 0.84')
plt.savefig('Figures/NewLooseContact_Crystalline_R_0_0001m_P_0_84.png',dpi=300)
plt.show()



plt.figure(figsize=((12,12)))
plt.plot(Temp,Fitcond_WBB_c_0_84_0_0001,label = "Fit", c='black',linewidth=3)
plt.scatter(Temp,Truecond_WBB_c_0_84_0_0001,label = "Analytical Conductivity", marker = "x",color='red')

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (W/m/K)')
plt.title('Looose Contact, Crystalline, R = 0.0001 m, Porosity = 0.84')
plt.savefig('Figures/LooseContact_Crystalline_R_0_0001m_P_0_84.png',dpi=300)
plt.show()




plt.figure(figsize=((12,12)))
plt.plot(NewTemp,(NewFitcond_WBB_c_0_84_0_0001-NewTruecond_WBB_c_0_84_0_0001)/NewTruecond_WBB_c_0_84_0_0001*100,label = "Fit", c='black',linewidth=3)

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Relative error on Conductivity (%)')
plt.title('Looose Contact, Crystalline, R = 0.0001 m, Porosity = 0.84')
plt.savefig('Figures/RelErrorLooseContact_Crystalline_R_0_0001m_P_0_84.png',dpi=300)
plt.show()





### Compare Specific Heat ###
T = np.arange(60,141,1)
C_True = np.loadtxt('Cice_true.tab')
x = (T-220)*0.01
C_mine = 1629.240 +582.1480*x+63.52807*x**2+79.49155*x**3
C_krc = 1704.57 +713.339*x+110.694*x**2+75.7506*x**3

plt.figure(figsize=((12,12)))
plt.plot(T,C_krc,label = "KRC", c='red',linewidth=3)
plt.plot(T,C_mine,label = "New Param", c='blue',linewidth=3)
plt.scatter(T,C_True,label = "Shulman",marker="x", c='black',linewidth=3)

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Specific Heat (J/kg/K)')
plt.title('Comparison parameterization for Specific Heat')
plt.savefig('Figures/CompParamSpecificHeat.png',dpi=300)
plt.show()




plt.figure(figsize=((12,12)))
plt.plot(T,(C_krc-C_True)/C_True*100,label = "KRC", c='red',linewidth=3)
plt.plot(T,(C_mine-C_True)/C_True*100,label = "New Param", c='blue',linewidth=3)

plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Relative error on Specific Heat (%)')
plt.title('Comparison parameterization for Specific Heat vs Shulman (2004)')
plt.savefig('Figures/RelativeCompParamSpecificHeatvsSchulman.png',dpi=300)
plt.show()



plt.figure(figsize=((12,12)))
plt.plot(T,(C_krc-C_mine)/C_mine*100, c='black',linewidth=3)

plt.xlabel('Temperature (K)')
plt.ylabel('Relative error on Specific Heat (%)')
plt.title('Comparison parameterization for Specific Heat KRC vs New')
plt.savefig('Figures/RelativeCompParamSpecificHeat_KRCvsNew.png',dpi=300)
plt.show()




### Compare Conduction  ###
T = np.arange(60,141,1)
x = (T-220)*0.01

cond_KRC =  0.03129103
T_user = 100

k_krc = cond_KRC*(1+2.7*((x-T_user)/350.)**3)


plt.figure(figsize=((12,12)))
plt.plot(Temp,Fitcond_GJRKBB_a_0_26_0_1,label = "Tight Contacts, Amorphous", linewidth=3)
plt.plot(Temp,Fitcond_GJRKBB_c_0_26_0_1,label = "Tight Contacts, Crystalline", linewidth=3)
plt.plot(Temp,Fitcond_WBB_a_0_26_0_1,label = "Loose Contacts, Amorphous", linewidth=3)
plt.plot(Temp,Fitcond_WBB_c_0_26_0_1,label = "Loose Contacts, Crystalline", linewidth=3)
plt.plot(T,k_krc, linewidth=3, label = 'KRC current parameterization')
plt.xlabel('Temperature (K)')
plt.legend()
plt.ylabel('Conductivity (W/m/K)')
plt.title('R = 0.1 m, Porosity = 0.26')
plt.savefig('Figures/Comp_currentparam_k_vsmymodels.png')







plt.figure(figsize=((12,12)))
plt.plot(T,fit2,label = "2nd order fit", linewidth=3)
plt.plot(T,fit3,label = "3rd order fit", linewidth=3)
plt.scatter(T,truth,label = "True model", linewidth=3)
plt.xlabel('Temperature (K)')
plt.legend()
plt.ylabel('Conductivity (W/m/K)')
plt.title('R = 0.1 mm, Porosity = 0.26')
plt.savefig('Error_2ndorderfit.png')






plt.figure(figsize=((12,12)))
plt.plot(T,(fit2-truth)/truth*100.,label = "True model", linewidth=3)
plt.xlabel('Temperature (K)')
plt.legend()
plt.ylabel('Relative Error on Conductivity (%)')
plt.title('R = 0.1 mm, Porosity = 0.26')
plt.savefig('Errorrel_2ndorderfit.png')

