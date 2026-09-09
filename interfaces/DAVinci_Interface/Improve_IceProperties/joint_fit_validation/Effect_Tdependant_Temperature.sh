#!/Applications/davinci.app/Contents/Resources/bin/davinci -f
#
# Check the Fit of specific heat and Conductivy computed in krc for icy regolith 
# Based on the work of C.Ferrari and A.Lucas
#

dir = "~/Desktop/Europa/Ice_properties/Merge_ComputeInputs_forKRC/"
source(dir+"Compute_Fit_PropIcyRegolith_4KRC.dvrc")

R = 1e-4
porosity = 0.26

porosity_str = porosity + ""
radius_str = R + ""

Out = Compute_Fit_ThermophysicalProperties_IcyRegolith('T','T',60,140,R,porosity,1.,"GJKRBB","Crystalline",'')

OUT_emissT = krc(lat=0, body="Europa",ALBEDO=.72,LKofT="F",lbound = -2/80,DENSITY=dens,EmisT = 'T',Emis0 = Out.Emis0,Emis1 = Out.Emis1,Emis2 =Out.Emis2,Emis3 =Out.Emis3)

MeanTsurf = avg(OUT_emissT.tsurf[,,1])

Xt = (MeanTsurf-220.)*0.01

Meanemis = Out.Emis0+Out.Emis1*Xt +Out.Emis2*Xt^2+Out.Emis3*Xt^3
OUT_noemissT = krc(lat=0, body="Europa",ALBEDO=.72,LKofT="F",lbound = -2/80,DENSITY=dens,EmisT = 'T',EMISS = Meanemis)

plot(OUT_emissT.tsurf[,,1] - OUT_noemissT.tsurf[,,1])

write(OUT_noemissT.tsurf[,,1],dir+"/tsurf_c_gjkrbb" + porosity_str + "_" + radius_str + "noemissT.tab",ascii,force=1)
write(OUT_emissT.tsurf[,,1],dir+"/tsurf_c_gjkrbb" + porosity_str + "_" + radius_str + "emissT.tab",ascii,force=1)
write(OUT_noemissT.time[,,1],dir+"/time.tab",ascii,force=1)



Out = Compute_Fit_ThermophysicalProperties_IcyRegolith('T','T',60,140,R,porosity,1.,"WBB","Amorphous",'')
  

OUT_emissT = krc(lat=0, body="Europa",ALBEDO=.72,LKofT="F",lbound = -2/80,DENSITY=dens,EmisT = 'T',Emis0 = Out.Emis0,Emis1 = Out.Emis1,Emis2 =Out.Emis2,Emis3 =Out.Emis3)

MeanTsurf = avg(OUT_emissT.tsurf[,,1])

Xt = (MeanTsurf-220.)*0.01

Meanemis = Out.Emis0+Out.Emis1*Xt +Out.Emis2*Xt^2+Out.Emis3*Xt^3
OUT_noemissT = krc(lat=0, body="Europa",ALBEDO=.72,LKofT="F",lbound = -2/80,DENSITY=dens,EmisT = 'T',EMISS = Meanemis)

plot(OUT_emissT.tsurf[,,1] - OUT_noemissT.tsurf[,,1])

write(OUT_noemissT.tsurf[,,1],dir+"/tsurf_a_wbb" + porosity_str + "_" + radius_str + "noemissT.tab",ascii,force=1)
write(OUT_emissT.tsurf[,,1],dir+"/tsurf_a_wbb" + porosity_str + "_" + radius_str + "emissT.tab",ascii,force=1)
write(OUT_noemissT.time[,,1],dir+"/time.tab",ascii,force=1)





## 

#!/Applications/davinci.app/Contents/Resources/bin/davinci -f
#
# Check the Fit of specific heat and Conductivy computed in krc for icy regolith 
# Based on the work of C.Ferrari and A.Lucas
#

dir = "~/Desktop/Europa/Ice_properties/Merge_ComputeInputs_forKRC/"
source(dir+"Compute_Fit_PropIcyRegolith_4KRC.dvrc")

R = 1e-4
porosity = 0.26
dens = porosity*920.
porosity_str = porosity + ""
radius_str = R + ""

Out = Compute_Fit_ThermophysicalProperties_IcyRegolith('T','T',60,140,R,porosity,1.,"GJKRBB","Crystalline",'')

OUT_emissT = krc(lat=0, body="Europa",ALBEDO=.72,LKofT="T",lbound = -2/80,DENSITY=dens,EmisT = 'T',Emis0 = Out.Emis0,Emis1 = Out.Emis1,Emis2 =Out.Emis2,Emis3 =Out.Emis3,SphUp0 =Out.Sph0,SphUp1 =Out.Sph1,SphUp2 =Out.Sph2,SphUp3 =Out.Sph3,ConUp0 = Out.Con0,ConUp1 = Out.Con1,ConUp2 = Out.Con2,ConUp3 = Out.Con3)

MeanTsurf = avg(OUT_emissT.tsurf[,,1])

Xt = (MeanTsurf-220.)*0.01

Meanemis = Out.Emis0+Out.Emis1*Xt +Out.Emis2*Xt^2+Out.Emis3*Xt^3
MeanCond = Out.Con0+Out.Con1*Xt +Out.Con2*Xt^2+Out.Con3*Xt^3
MeanC = Out.Sph0+Out.Sph1*Xt +Out.Sph2*Xt^2+Out.Sph3*Xt^3

OUT_noemissT = krc(lat=0, body="Europa",ALBEDO=.72,LKofT="F",lbound = -2/80,DENSITY=dens,EmisT = 'T',EMISS = Meanemis,COND = MeanCond,SPEC_HEAT =  MeanC)

plot(OUT_emissT.tsurf[,,1] - OUT_noemissT.tsurf[,,1])

write(OUT_noemissT.tsurf[,,1],dir+"/tsurf_c_gjkrbb" + porosity_str + "_" + radius_str + "allTdep.tab",ascii,force=1)
write(OUT_emissT.tsurf[,,1],dir+"/tsurf_c_gjkrbb" + porosity_str + "_" + radius_str + "noTdep.tab",ascii,force=1)
write(OUT_noemissT.time[,,1],dir+"/time.tab",ascii,force=1)



Out = Compute_Fit_ThermophysicalProperties_IcyRegolith('T','T',60,140,R,porosity,1.,"WBB","Amorphous",'')
  

OUT_emissT = krc(lat=0, body="Europa",ALBEDO=.72,LKofT="T",lbound = -2/80,DENSITY=dens,EmisT = 'T',Emis0 = Out.Emis0,Emis1 = Out.Emis1,Emis2 =Out.Emis2,Emis3 =Out.Emis3,SphUp0 =Out.Sph0,SphUp1 =Out.Sph1,SphUp2 =Out.Sph2,SphUp3 =Out.Sph3,ConUp0 = Out.Con0,ConUp1 = Out.Con1,ConUp2 = Out.Con2,ConUp3 = Out.Con3)

MeanTsurf = avg(OUT_emissT.tsurf[,,1])

Xt = (MeanTsurf-220.)*0.01


Meanemis = Out.Emis0+Out.Emis1*Xt +Out.Emis2*Xt^2+Out.Emis3*Xt^3
MeanCond = Out.Con0+Out.Con1*Xt +Out.Con2*Xt^2+Out.Con3*Xt^3
MeanC = Out.Sph0+Out.Sph1*Xt +Out.Sph2*Xt^2+Out.Sph3*Xt^3

OUT_noemissT = krc(lat=0, body="Europa",ALBEDO=.72,LKofT="F",lbound = -2/80,DENSITY=dens,EmisT = 'T',EMISS = Meanemis,COND = MeanCond,SPEC_HEAT =  MeanC)

plot(OUT_emissT.tsurf[,,1] , OUT_noemissT.tsurf[,,1])

write(OUT_noemissT.tsurf[,,1],dir+"/tsurf_a_wbb" + porosity_str + "_" + radius_str + "allTdep.tab",ascii,force=1)
write(OUT_emissT.tsurf[,,1],dir+"/tsurf_a_wbb" + porosity_str + "_" + radius_str + "noTdep.tab",ascii,force=1)
write(OUT_noemissT.time[,,1],dir+"/time.tab",ascii,force=1)