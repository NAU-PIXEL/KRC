import numpy as np
import matplotlib.pyplot as plt



solarpen_4cm = np.loadtxt("KRC_solarpen_eps=4cm.ascii")
bestfitti_4cm = np.loadtxt("KRC_solarpen_eps=4cm_noabs_bestfitti.ascii")
bestfittim20_4cm = np.loadtxt('KRC_solarpen_eps=4cm_noabs_bestfitti_m20.ascii')
bestfittip20_4cm = np.loadtxt('KRC_solarpen_eps=4cm_noabs_bestfitti_p20.ascii')
bestfittip20_4cm = np.loadtxt('KRC_solarpen_eps=4cm_noabs_bestfitti_p20.ascii')
bestfitti_albm01_4cm = np.loadtxt('KRC_solarpen_eps=4cm_noabs_bestfitti_albm01.ascii')
bestfitti_albp01_4cm = np.loadtxt('KRC_solarpen_eps=4cm_noabs_bestfitti_albp01.ascii')



plt.plot(time,solarpen_4cm,'k',label='e-folding = 4cm')
plt.plot(time,bestfitti_4cm,'k',linestyle='dotted',label='No Solar Absorption,TI-best fit')
plt.xlabel('Local Time (hr)')
plt.ylabel('Surface Temperature (K)')
plt.legend()
plt.savefig('CompTIbestfit_abs_4cm.png',dpi=300)
plt.show()


plt.plot(time,solarpen_4cm,'k',label='e-folding = 4cm')
plt.plot(time,bestfitti_4cm,'k',linestyle='dotted',label='No Solar Absorption,TI-best fit')
plt.plot(time,bestfittim20_4cm,'b',linestyle='dashed',label='No Solar Absorption,TI-best fit +- 20 USI')
plt.plot(time,bestfittip20_4cm,'b',linestyle='dashed')

plt.xlabel('Local Time (hr)')
plt.ylabel('Surface Temperature (K)')
plt.legend()
plt.savefig('CompTIbestfit_pm20_abs_4cm.png',dpi=300)
plt.show()


plt.plot(time,solarpen_4cm,'k',label='e-folding = 4cm')
plt.plot(time,bestfitti_4cm,'k',linestyle='dotted',label='No Solar Absorption,TI-best fit')
plt.plot(time,bestfitti_albm01_4cm,'r',linestyle='dashed',label='No Solar Absorption,TI-best fit, Albedo +- 0.1')
plt.plot(time,bestfitti_albp01_4cm,'r',linestyle='dashed')

plt.xlabel('Local Time (hr)')
plt.ylabel('Surface Temperature (K)')
plt.legend()
plt.savefig('CompTIbestfit_albpm01_abs_4cm.png',dpi=300)
plt.show()


solarpen_2cm = np.loadtxt("KRC_solarpen_eps=2cm.ascii")
bestfitti_2cm = np.loadtxt("KRC_solarpen_eps=2cm_noabs_bestfitti.ascii")


plt.plot(time,solarpen_2cm,'k',label='e-folding = 2cm')
plt.plot(time,bestfitti_2cm,'k',linestyle='dotted',label='No Solar Absorption,TI-best fit')
plt.xlabel('Local Time (hr)')
plt.ylabel('Surface Temperature (K)')
plt.legend()
plt.savefig('CompTIbestfit_abs_2cm.png',dpi=300)
plt.show()



solarpen_1cm = np.loadtxt("KRC_solarpen_eps=1cm.ascii")
bestfitti_1cm = np.loadtxt("KRC_solarpen_eps=1cm_noabs_bestfitti.ascii")


plt.plot(time,solarpen_1cm,'k',label='e-folding = 1cm')
plt.plot(time,bestfitti_1cm,'k',linestyle='dotted',label='No Solar Absorption,TI-best fit')
plt.xlabel('Local Time (hr)')
plt.ylabel('Surface Temperature (K)')
plt.legend()
plt.savefig('CompTIbestfit_abs_1cm.png',dpi=300)
plt.show()





## Heating rate


plt.plot(time[:-1], (np.diff(solarpen_4cm))/(3600*(time[2]-time[1])),'k',label='e-folding = 4cm')
plt.plot(time[:-1], (np.diff(bestfitti_4cm))/(3600*(time[2]-time[1])),'k',linestyle='dotted',label='No Solar Absorption,TI-best fit')
plt.xlabel('Local Time (hr)')
plt.ylabel('Surface Heating Rate (K/s) (K)')
plt.xlim((7,17))
plt.legend()
plt.savefig('Heatingrate_4cm.png',dpi=300)
plt.show()


plt.plot(time[:-1], (np.diff(solarpen_2cm))/(3600*(time[2]-time[1])),'k',label='e-folding = 2cm')
plt.plot(time[:-1], (np.diff(bestfitti_2cm))/(3600*(time[2]-time[1])),'k',linestyle='dotted',label='No Solar Absorption,TI-best fit')
plt.xlabel('Local Time (hr)')
plt.ylabel('Surface Heating Rate (K/s) (K)')
plt.xlim((7,17))
plt.legend()
plt.savefig('Heatingrate_2cm.png',dpi=300)
plt.show()



plt.plot(time[:-1], (np.diff(solarpen_1cm))/(3600*(time[2]-time[1])),'k',label='e-folding = 1cm')
plt.plot(time[:-1], (np.diff(bestfitti_1cm))/(3600*(time[2]-time[1])),'k',linestyle='dotted',label='No Solar Absorption,TI-best fit')
plt.xlabel('Local Time (hr)')
plt.ylabel('Surface Heating Rate (K/s) (K)')
plt.xlim((7,17))
plt.legend()
plt.savefig('Heatingrate_1cm.png',dpi=300)
plt.show()