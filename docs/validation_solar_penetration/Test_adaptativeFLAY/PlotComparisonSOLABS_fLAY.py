time = np.loadtxt('time.tab')
Tsurf_tI33_delta1cm_flay01 = np.loadtxt('Tsurf_tI33_delta1cm_flay01.tab')
Tsurf_tI33_delta1cm_flay005 = np.loadtxt('Tsurf_tI33_delta1cm_flay005.tab')
Tsurf_tI33_delta1cm_flay001 = np.loadtxt('Tsurf_tI33_delta1cm_flay001.tab')

Tsurf_tI133_delta1cm_flay01 = np.loadtxt('Tsurf_tI133_delta1cm_flay01.tab')
Tsurf_tI133_delta1cm_flay005 = np.loadtxt('Tsurf_tI133_delta1cm_flay005.tab')
Tsurf_tI133_delta1cm_flay001 = np.loadtxt('Tsurf_tI133_delta1cm_flay001.tab')


plt.plot(time,Tsurf_tI33_delta1cm_flay01,'k',label='Nominal, FLAY = 0.1')
plt.plot(time,Tsurf_tI33_delta1cm_flay005,'k',linestyle='dotted',label='FLAY = 0.05')
plt.plot(time,Tsurf_tI33_delta1cm_flay001,'k',linestyle='dashed',label='FLAY = 0.01')
plt.xlabel('Time (hr)')
plt.ylabel('Surface Temperature (K)')
plt.legend()
plt.savefig('TI33.png',dpi=300)
plt.show()


plt.plot(time,Tsurf_tI133_delta1cm_flay01,'k',label='Nominal, FLAY = 0.1')
plt.plot(time,Tsurf_tI133_delta1cm_flay005,'k',linestyle='dotted',label='FLAY = 0.05')
plt.plot(time,Tsurf_tI133_delta1cm_flay001,'k',linestyle='dashed',label='FLAY = 0.01')
plt.xlabel('Time (hr)')
plt.ylabel('Surface Temperature (K)')
plt.legend()
plt.savefig('TI133.png',dpi=300)
plt.show()


Tsurf_ada_ti33 = np.loadtxt('Tsurf_tI33_delta1cm_adaptativeFlay.tab')
Tsurf_ada_ti133 = np.loadtxt('Tsurf_tI133_delta1cm_adaptativeFlay.tab')

plt.plot(time,Tsurf_ada_ti33,'k',label='Adaptative FLAY')
plt.plot(time,Tsurf_tI33_delta1cm_flay001,'k',linestyle='dashed',label='FLAY = 0.01')
plt.xlabel('Time (hr)')
plt.ylabel('Surface Temperature (K)')
plt.legend()
plt.savefig('TI33ada.png',dpi=300)
plt.show()


plt.plot(time,Tsurf_ada_ti133,'k',label='Adaptative FLAY')
plt.plot(time,Tsurf_tI133_delta1cm_flay001,'k',linestyle='dashed',label='FLAY = 0.01')
plt.xlabel('Time (hr)')
plt.ylabel('Surface Temperature (K)')
plt.legend()
plt.savefig('TI133ada.png',dpi=300)
plt.show()