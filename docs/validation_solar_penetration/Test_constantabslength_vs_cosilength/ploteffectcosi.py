import numpy as np
import matplotlib.pyplot as plt

time = np.loadtxt('Time.tab')
Tsurf_constant = np.loadtxt('Tsurf_constantscaleabs.tab')
Tsurf_cosi = np.loadtxt('Tsurf_cosiscaleabs.tab')


plt.plot(time,Tsurf_constant,'k',label='Constant e-folding')
plt.plot(time,Tsurf_cosi,'k',linestyle='dotted',label='e-folding*cos(i)')
plt.xlabel('Time (hr)')
plt.ylabel('Surface Temperature (K)')
plt.legend()
plt.savefig('Effect_incidenceangle.png',dpi=300)
plt.show()


plt.figure(figsize=((16,8)))
plt.subplot(2,1,1)
plt.plot(time,Tsurf_constant-Tsurf_cosi,'k',label=' constant-cosi')
plt.legend()
plt.xlabel('Time (hr)')
plt.ylabel('Absolute Difference of Tsurf (K)')
plt.subplot(2,1,2)
plt.plot(time,(Tsurf_constant-Tsurf_cosi)/Tsurf_cosi*100,'k',label='(constant-cosi)/cosi')
plt.xlabel('Time (hr)')
plt.legend()
plt.ylabel('Relative Difference Tsurf (%)')
plt.savefig('Effect_incidenceangle_RelError.png',dpi=300)
plt.show()
