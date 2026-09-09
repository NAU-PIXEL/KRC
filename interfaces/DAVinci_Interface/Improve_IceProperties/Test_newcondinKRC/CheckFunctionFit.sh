#!/Applications/davinci.app/Contents/Resources/bin/davinci -f
#
# Check the Fit of specific heat and Conductivy computed in krc for icy regolith 
# Based on the work of C.Ferrari and A.Lucas
#


dir = "~/Desktop/Europa/Ice_properties/Test_newcondinKRC/"
source(dir+"Compute_Fit_Cond_Spec_4KRC.dvrc")

#cas qui bugue: R = 1e-4, 

R = 1e-4
porosity = 0.26

porosity_str = porosity + ""
radius_str = R + ""

# Fit Specific heat
fit_Cp = fit_Cp_waterice(60,140) 
fit_c_GJKRBB = fit_Cond_waterice(60,140,R,porosity,"GJKRBB","Crystalline")
fit_a_GJKRBB = fit_Cond_waterice(60,140,R,porosity,"GJKRBB","Amorphous")

# Low contact

fit_c_WBB = fit_Cond_waterice(60,140,R,porosity,"WBB","Crystalline")
fit_a_WBB = fit_Cond_waterice(60,140,R,porosity,"WBB","Amorphous")

out_regular = krc(body='Europa',lat=0,LKofT=T,Por1 = porosity)
out_regular_Cp = krc(body='Europa',lat=0,LKofT=T,SphUp0=fit_Cp[1], SphUp1=fit_Cp[2],SphUp2=fit_Cp[3],SphUp3=fit_Cp[4],Por1 = porosity)
out_c_GJKRBB = krc(body='Europa',lat=0,LKofT=T,SphUp0=fit_Cp[1], SphUp1=fit_Cp[2],SphUp2=fit_Cp[3],SphUp3=fit_Cp[4],ConUp0=fit_c_GJKRBB[1],ConUp1=fit_c_GJKRBB[2],ConUp2=fit_c_GJKRBB[3],ConUp3=fit_c_GJKRBB[4],Por1 = porosity)
out_a_GJKRBB = krc(body='Europa',lat=0,LKofT=T,SphUp0=fit_Cp[1], SphUp1=fit_Cp[2],SphUp2=fit_Cp[3],SphUp3=fit_Cp[4],ConUp0=fit_a_GJKRBB[1],ConUp1=fit_a_GJKRBB[2],ConUp2=fit_a_GJKRBB[3],ConUp3=fit_a_GJKRBB[4],Por1 = porosity)
out_c_WBB = krc(body='Europa',lat=0,LKofT=T,SphUp0=fit_Cp[1], SphUp1=fit_Cp[2],SphUp2=fit_Cp[3],SphUp3=fit_Cp[4],ConUp0=fit_c_WBB[1],ConUp1=fit_c_WBB[2],ConUp2=fit_c_WBB[3],ConUp3=fit_c_WBB[4],Por1 = porosity)
out_a_WBB = krc(body='Europa',lat=0,LKofT=T,SphUp0=fit_Cp[1], SphUp1=fit_Cp[2],SphUp2=fit_Cp[3],SphUp3=fit_Cp[4],ConUp0=fit_a_WBB[1],ConUp1=fit_a_WBB[2],ConUp2=fit_a_WBB[3],ConUp3=fit_a_WBB[4],Por1 = porosity)


plot(out_regular.tsurf[,,1], x=out_regular.time,label="",out_regular_Cp.tsurf[,,1], x=out_regular_Cp.time,label="",out_a_GJKRBB.tsurf[,,1], x=out_a_GJKRBB.time,label="",out_c_GJKRBB.tsurf[,,1], x=out_c_GJKRBB.time,label="",out_c_WBB.tsurf[,,1], x=out_c_WBB.time,label="",out_a_WBB.tsurf[,,1], x=out_a_WBB.time,label="")

write(out_regular.tsurf,dir+"/Tsurf_regular_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(out_c_GJKRBB.tsurf,dir+"/Tsurf_GJRKBB_c_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(out_a_GJKRBB.tsurf,dir+"/Tsurf_GJRKBB_a_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(out_c_WBB.tsurf,dir+"/Tsurf_WBB_c_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(out_a_WBB.tsurf,dir+"/Tsurf_WBB_a_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(out_regular.time,dir+"/Time.tab",ascii,force=1)


test_G_c_GJKRBB = ComputeConductivity(It,R,porosity,"GJKRBB","Crystalline")
test_G_c_GJKRBB = ComputeConductivity(It,R,porosity,"GJKRBB","Crystalline")

