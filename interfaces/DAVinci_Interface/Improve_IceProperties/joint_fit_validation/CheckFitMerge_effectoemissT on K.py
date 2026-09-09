


dir = "~/Desktop/Europa/Ice_properties/Model_newTI/Script_Fiticeproperties4KRC/"
source(dir+"Compute_Fit_Cond_Spec_4KRC.dvrc")



R = 1e-4
porosity = 0.26

porosity_str = porosity + ""
radius_str = R + ""



Compute_Fit_ThermophysicalProperties_IcyRegolith('T','T',60,140,R,porosity,1.,"GJKRBB","Crystalline",'')
    Con0: 0.1285186	
    Con1: 0.1480212	
    Con2: 0.1000803	
    Con3: 0.000000

# Fromthe regular model 0.1285613	0.1480694	0.1000944	0.000000	


plt.plot(Temp,0.1285186+0.1480212*xt+0.1000803*xt**2+0*xt**3,label = 'With Temperature Dependant Emissivity')
plt.plot(Temp,0.1285613+0.1480694*xt+0.1000944*xt**2,label = 'Constant Emissivity of 1')
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (SI)')
plt.title('GJKRBB = Crystalline - R=1e-4m')
plt.savefig('GJKRBB_c_R1em4.png',dpi=300)
plt.show()

Compute_Fit_ThermophysicalProperties_IcyRegolith('T','T',60,140,R,porosity,1.,"GJKRBB","Amorphous",'')

    Con0: 0.01023817	
    Con1: 0.005883659	
    Con2: 0.001435268	
    Con3: 0.0004823851	


# Regular model 0.01027675	0.005932613	0.001455236	0.0004848374	

plt.plot(Temp,0.01023817+0.005883659*xt+0.001435268*xt**2+0.0004823851*xt**3,label = 'With Temperature Dependant Emissivity')
plt.plot(Temp,0.01027675+0.005932613*xt+0.001455236*xt**2+0.0004848374*xt**3,label = 'Constant Emissivity of 1')
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (SI)')
plt.title('GJKRBB - Amorphous - R=1e-4m')
plt.savefig('GJKRBB_a_R1em4.png',dpi=300)
plt.show()



Compute_Fit_ThermophysicalProperties_IcyRegolith('T','T',60,140,R,porosity,1.,"WBB","Crystalline",'')
    Con0: 0.001561740	
    Con1: 0.001911680	
    Con2: 0.001129811	
    Con3: 0.000000

# Regular model  2.308552e-05	-0.002186754	-0.002392175	-0.0009822343	


plt.plot(Temp,0.001561740+0.001911680*xt+0.001129811*xt**2,label = 'With Temperature Dependant Emissivity')
plt.plot(Temp,2.308552e-05-0.002186754*xt-0.002392175*xt**2-0.0009822343*xt**3,label = 'Constant Emissivity of 1')
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (SI)')
plt.title('WBB - Crystalline - R=1e-4m')
plt.savefig('WBB_c_R1em4.png',dpi=300)
plt.show()



Compute_Fit_ThermophysicalProperties_IcyRegolith('T','T',60,140,R,porosity,1.,"WBB","Amorphous",'')
    Con0: 0.0005826827	
    Con1: 0.0007257603	
    Con2: 0.0003231720	
    Con3: 5.258474e-05	

# Regular model  0.0006212567	0.0007747147	0.0003431401	5.503708e-05	



plt.plot(Temp,0.0005826827+0.0007257603*xt+0.0003231720*xt**2+5.258474e-05*xt**3,label = 'With Temperature Dependant Emissivity')
plt.plot(Temp,0.0006212567 +0.0007747147*xt+0.0003431401*xt**2+5.503708e-05*xt**3,label = 'Constant Emissivity of 1')
plt.legend()
plt.xlabel('Temperature (K)')
plt.ylabel('Conductivity (SI)')
plt.title('WBB - Amorphous - R=1e-4m')
plt.savefig('WBB_a_R1em4.png',dpi=300)
plt.show()

plo(1.035179+0.2213155*xt+0.2145190*xt^2+0.07158190*xt^3,xaxis = It)
    EmisT: "T"
    Emis0: 1.035179	
    Emis1: 0.2213155	
    Emis2: 0.2145190	
    Emis3: 0.07158190