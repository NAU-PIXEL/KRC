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

time = np.loadtxt('Time.tab')
high_c = np.loadtxt('Tsurf_GJRKBB_c_0.26_0.0001.tab')
high_a = np.loadtxt('Tsurf_GJRKBB_a_0.26_0.0001.tab')

loose_c = np.loadtxt('Tsurf_WBB_c_0.26_0.0001.tab')
loose_a = np.loadtxt('Tsurf_WBB_a_0.26_0.0001.tab')


high_c_novar = np.loadtxt('Tsurfnovar_GJRKBB_c_0.26_0.0001.tab')
high_a_novar = np.loadtxt('Tsurfnovar_GJRKBB_a_0.26_0.0001.tab')

loose_c_novar = np.loadtxt('Tsurfnovar_WBB_c_0.26_0.0001.tab')
loose_a_novar = np.loadtxt('Tsurfnovar_WBB_a_0.26_0.0001.tab')

TIhigh_c = np.loadtxt('TI_GJRKBB_c_0.26_0.0001.tab')
TIhigh_a = np.loadtxt('TI_GJRKBB_a_0.26_0.0001.tab')

TIloose_c = np.loadtxt('TI_WBB_c_0.26_0.0001.tab')
TIloose_a = np.loadtxt('TI_WBB_a_0.26_0.0001.tab')


reg = np.loadtxt('Tsurf_regular_0.26_0.0001.tab')

plt.figure(figsize=((12,12)))
plt.plot(time,reg[0,:],label='Default KRC',linewidth=3)
plt.plot(time,high_c[0,:],label='High Contact - Crystalline',linewidth=3)
plt.plot(time,high_a[0,:],label='High Contact - Amorphe',linewidth=3)
plt.plot(time,loose_c[0,:],label='Loose Contact - Crystalline',linewidth=3)
plt.plot(time,loose_a[0,:],label='Loose Contact - Amorphe',linewidth=3)
plt.legend()
plt.xlabel('Time (hr)')
plt.ylabel('Ice Temperature (K)')
plt.title('Radius = 0.1mm, porosity = 0.26')
plt.savefig('Test_newconfig_newfit.png',dpi=300)
plt.show()




plt.figure(figsize=((12,12)))
plt.subplot(2,1,1)
plt.plot(time,high_c[0,:],label='conductivity varies with temp',linewidth=3)
plt.plot(time,high_c_novar[0,:],label='constant mean conductivity',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Ice Temperature (K)')
plt.title('High Contact - Crystalline - Radius = 0.1mm, porosity = 0.26')
plt.legend()
plt.subplot(2,1,2)
plt.plot(time,high_c[0,:] - high_c_novar[0,:] ,label='conductivity varies with temp',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Delta T (K)')
plt.savefig('ErrorConductivity_highc.png')
plt.show()


plt.figure(figsize=((12,12)))
plt.subplot(2,1,1)
plt.plot(time,high_a[0,:],label='conductivity varies with temp',linewidth=3)
plt.plot(time,high_a_novar[0,:],label='constant mean conductivity',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Ice Temperature (K)')
plt.title('High Contact - Amorphous - Radius = 0.1mm, porosity = 0.26')
plt.legend()
plt.subplot(2,1,2)
plt.plot(time,high_a[0,:] - high_a_novar[0,:] ,label='conductivity varies with temp',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Delta T (K)')
plt.savefig('ErrorConductivity_higha.png')
plt.show()



plt.figure(figsize=((12,12)))
plt.subplot(2,1,1)
plt.plot(time,loose_c[0,:],label='conductivity varies with temp',linewidth=3)
plt.plot(time,loose_c_novar[0,:],label='constant mean conductivity',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Ice Temperature (K)')
plt.title('Loose Contact - Crystalline - Radius = 0.1mm, porosity = 0.26')
plt.legend()
plt.subplot(2,1,2)
plt.plot(time,loose_c[0,:] - loose_c_novar[0,:] ,label='conductivity varies with temp',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Delta T (K)')
plt.savefig('ErrorConductivity_loosec.png')
plt.show()



plt.figure(figsize=((12,12)))
plt.subplot(2,1,1)
plt.plot(time,loose_a[0,:],label='conductivity varies with temp',linewidth=3)
plt.plot(time,loose_a_novar[0,:],label='constant mean conductivity',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Ice Temperature (K)')
plt.title('Loose Contact - Crystalline - Radius = 0.1mm, porosity = 0.26')
plt.legend()
plt.subplot(2,1,2)
plt.plot(time,loose_a[0,:] - loose_a_novar[0,:] ,label='conductivity varies with temp',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Delta T (K)')
plt.savefig('ErrorConductivity_loosea.png')
plt.show()




##### Thermal Inertia

plt.figure(figsize=((12,12)))
plt.plot(time,TIhigh_c[:],label='conductivity varies with temp',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Thermal Inertia (SI)')
plt.title('High Contact - Crystalline - Radius = 0.1mm, porosity = 0.26')
plt.savefig('TI_highc.png')
plt.show()

plt.figure(figsize=((12,12)))
plt.plot(time,TIhigh_a[:],label='conductivity varies with temp',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Thermal Inertia (SI)')
plt.title('High Contact - Amorphous - Radius = 0.1mm, porosity = 0.26')
plt.savefig('TI_higha.png')
plt.show()





plt.figure(figsize=((12,12)))
plt.plot(time,TIloose_c[:],label='conductivity varies with temp',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Thermal Inertia (SI)')
plt.title('Loose Contact - Crystalline - Radius = 0.1mm, porosity = 0.26')
plt.savefig('TI_loosec.png')
plt.show()



plt.figure(figsize=((12,12)))
plt.plot(time,TIloose_a[:],label='conductivity varies with temp',linewidth=3)
plt.xlabel('Time (hr)')
plt.ylabel('Thermal Inertia (SI)')
plt.title('Loose Contact - Amorphous - Radius = 0.1mm, porosity = 0.26')
plt.savefig('TI_loosea.png')
plt.show()


##### Effect Albedo

Tsurf_highti_alb03 = np.loadtxt('Tsurf_highti_alb03.tab')
Tsurf_highti_alb067 = np.loadtxt('Tsurf_highti_alb067.tab')
Tsurf_highti_alb09 = np.loadtxt('Tsurf_highti_alb09.tab')

Tsurf_lowti_alb03 = np.loadtxt('Tsurf_lowti_alb03.tab')
Tsurf_lowti_alb067 = np.loadtxt('Tsurf_lowti_alb067.tab')
Tsurf_lowti_alb09 = np.loadtxt('Tsurf_lowti_alb09.tab')


plt.figure(figsize=((12,12)))
plt.plot(time,Tsurf_highti_alb03[:],label='Albedo = 0.3',linewidth=3)
plt.plot(time,Tsurf_highti_alb067[:],label='Albedo = 0.67',linewidth=3)
plt.plot(time,Tsurf_highti_alb09[:],label='Albedo = 0.9',linewidth=3)
plt.xlabel('Time (hr)')
plt.legend()

plt.ylabel('Temperature (K)')
plt.title('High Contact - Crystalline - Radius = 0.1mm, porosity = 0.26')
plt.savefig('Effectalb_highTI.png')
plt.show()


plt.figure(figsize=((12,12)))
plt.plot(time,Tsurf_lowti_alb03[:],label='Albedo = 0.3',linewidth=3)
plt.plot(time,Tsurf_lowti_alb067[:],label='Albedo = 0.67',linewidth=3)
plt.plot(time,Tsurf_lowti_alb09[:],label='Albedo = 0.9',linewidth=3)
plt.legend()
plt.xlabel('Time (hr)')
plt.ylabel('Temperature (K)')
plt.title('Loose Contact - Amorphous - Radius = 0.1mm, porosity = 0.26')
plt.savefig('Effectalb_lowTI.png')
plt.show()

##### Effect Layering

Tsurf_high = np.loadtxt('Tsurf_highti.tab')
Tsurf_low = np.loadtxt('Tsurf_lowti.tab')

Tsurf_high_low_5cm = np.loadtxt('Tsurf_high_low_5cm.tab')
Tsurf_high_low_5mm = np.loadtxt('Tsurf_high_low_5mm.tab')

Tsurf_low_high_5cm = np.loadtxt('Tsurf_low_high_5cm.tab')
Tsurf_low_high_5mm = np.loadtxt('Tsurf_low_high_5mm.tab')


plt.figure(figsize=((12,12)))
plt.plot(time,Tsurf_high[:],label='Uniform high TI',linewidth=3)

plt.plot(time,Tsurf_high_low_5mm[:],label='High TI // Low TI - 5 mm',linewidth=3)
plt.plot(time,Tsurf_high_low_5cm[:],label='High TI // Low TI - 5 cm',linewidth=3)
plt.legend()
plt.xlabel('Time (hr)')
plt.ylabel('Temperature (K)')
plt.savefig('Effect_strat_highlow.png')
plt.show()



plt.figure(figsize=((12,12)))
plt.plot(time,Tsurf_low[:],label='Uniform low TI',linewidth=3)

plt.plot(time,Tsurf_low_high_5mm[:],label='Low TI // High TI - 5 mm',linewidth=3)
plt.plot(time,Tsurf_low_high_5cm[:],label='Low TI // High TI - 5 cm',linewidth=3)
plt.legend()
plt.xlabel('Time (hr)')
plt.ylabel('Temperature (K)')
plt.savefig('Effect_strat_lowhigh.png')
plt.show()
