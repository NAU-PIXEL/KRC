#!/Applications/davinci.app/Contents/Resources/bin/davinci -f
#
# Check the Fit of specific heat and Conductivy computed in krc for icy regolith 
# Based on the work of C.Ferrari and A.Lucas
#


dir = "~/Desktop/Europa/Ice_properties/Script_Fiticeproperties4KRC/"
source(dir+"Compute_Fit_Cond_Spec_4KRC.dvrc")



R = 1e-4
porosity = 0.26

porosity_str = porosity + ""
radius_str = R + ""


# 1. Fit of Cp 

It = create2(60,1,140)
C = Cp_ice_water(It)
fit_C = fit_Cp_waterice(60,140)
xt = ( It - 220 ) * 0.01
testfit = fit_C[1,1,1] + fit_C[2,1,1]*xt + fit_C[3,1,1]*xt^2 + fit_C[4,1,1]*xt^3

plot(C,x=It, testfit, x= It)
pause()

# 2. Fit of cond 
# High contact
test_G_c_GJKRBB = ComputeConductivity(It,R,porosity,"GJKRBB","Crystalline")
fit_k = fit_Cond_waterice(60,140,R,porosity,"GJKRBB","Crystalline")
fit_c_GJKRBB = fit_k[1,1,1] + fit_k[2,1,1]*xt + fit_k[3,1,1]*xt^2 + fit_k[4,1,1]*xt^3
plot(test_G_c_GJKRBB,x=It, fit_c_GJKRBB, x= It)
pause()

test_G_a_GJKRBB = ComputeConductivity(It,R,porosity,"GJKRBB","Amorphous")
fit_k = fit_Cond_waterice(60,140,R,porosity,"GJKRBB","Amorphous")
fit_a_GJKRBB = fit_k[1,1,1] + fit_k[2,1,1]*xt + fit_k[3,1,1]*xt^2 + fit_k[4,1,1]*xt^3
plot(test_G_a_GJKRBB,x=It, fit_a_GJKRBB, x= It)
pause() 

# Low contact

test_G_c_WBB = ComputeConductivity(It,R,porosity,"WBB","Crystalline")
fit_k = fit_Cond_waterice(60,140,R,porosity,"WBB","Crystalline")
fit_c_WBB = fit_k[1,1,1] + fit_k[2,1,1]*xt + fit_k[3,1,1]*xt^2 + fit_k[4,1,1]*xt^3
plot(test_G_c_WBB,x=It, testfit, x= It)
pause()

test_G_a_WBB = ComputeConductivity(It,R,porosity,"WBB","Amorphous")
fit_k = fit_Cond_waterice(60,140,R,porosity,"WBB","Amorphous")
fit_a_WBB = fit_k[1,1,1] + fit_k[2,1,1]*xt + fit_k[3,1,1]*xt^2 + fit_k[4,1,1]*xt^3
plot(test_G_a_WBB,x=It, fit_a_WBB, x= It)
pause() 

## Write for plot
write(It,dir+"/Temp.tab",ascii,force=1)
write(test_G_c_GJKRBB,dir+"/Truecond_GJRKBB_c_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(test_G_a_GJKRBB,dir+"/Truecond_GJRKBB_a_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(test_G_c_WBB,dir+"/Truecond_WBB_c_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(test_G_a_WBB,dir+"/Truecond_WBB_a_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)

write(fit_c_GJKRBB,dir+"/Fitcond_GJRKBB_c_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(fit_a_GJKRBB,dir+"/Fitcond_GJRKBB_a_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(fit_c_WBB,dir+"/Fitcond_WBB_c_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(fit_a_WBB,dir+"/Fitcond_WBB_a_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)





## Highlight sensitivity to low TI/TEMP

# 1. Fit of Cp 

It = create2(20,1,180)
C = Cp_ice_water(It)
fit_C = fit_Cp_waterice(60,160)
xt = ( It - 220 ) * 0.01
testfit = fit_C[1,1,1] + fit_C[2,1,1]*xt + fit_C[3,1,1]*xt^2 + fit_C[4,1,1]*xt^3

plot(C,x=It, testfit, x= It)
pause()

# 2. Fit of cond 
# High contact
test_G_c_GJKRBB = ComputeConductivity(It,R,porosity,"GJKRBB","Crystalline")
fit_k = fit_Cond_waterice(20,180,R,porosity,"GJKRBB","Crystalline")
fit_c_GJKRBB = fit_k[1,1,1] + fit_k[2,1,1]*xt + fit_k[3,1,1]*xt^2 + fit_k[4,1,1]*xt^3
plot(test_G_c_GJKRBB,x=It, fit_c_GJKRBB, x= It)
pause()

test_G_a_GJKRBB = ComputeConductivity(It,R,porosity,"GJKRBB","Amorphous")
fit_k = fit_Cond_waterice(20,180,R,porosity,"GJKRBB","Amorphous")
fit_a_GJKRBB = fit_k[1,1,1] + fit_k[2,1,1]*xt + fit_k[3,1,1]*xt^2 + fit_k[4,1,1]*xt^3
plot(test_G_a_GJKRBB,x=It, fit_a_GJKRBB, x= It)
pause() 

# Low contact

test_G_c_WBB = ComputeConductivity(It,R,porosity,"WBB","Crystalline")
fit_k = fit_Cond_waterice(20,180,R,porosity,"WBB","Crystalline")
fit_c_WBB = fit_k[1,1,1] + fit_k[2,1,1]*xt + fit_k[3,1,1]*xt^2 + fit_k[4,1,1]*xt^3
plot(test_G_c_WBB,x=It, testfit, x= It)
pause()

test_G_a_WBB = ComputeConductivity(It,R,porosity,"WBB","Amorphous")
fit_k = fit_Cond_waterice(20,180,R,porosity,"WBB","Amorphous")
fit_a_WBB = fit_k[1,1,1] + fit_k[2,1,1]*xt + fit_k[3,1,1]*xt^2 + fit_k[4,1,1]*xt^3
plot(test_G_a_WBB,x=It, testfit, x= It)
pause() 

## Write for plot
write(It,dir+"/NewTemp.tab",ascii,force=1)
write(test_G_c_GJKRBB,dir+"/newTruecond_GJRKBB_c_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(test_G_a_GJKRBB,dir+"/newTruecond_GJRKBB_a_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(test_G_c_WBB,dir+"/newTruecond_WBB_c_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(test_G_a_WBB,dir+"/newTruecond_WBB_a_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)

write(fit_c_GJKRBB,dir+"/newFitcond_GJRKBB_c_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(fit_a_GJKRBB,dir+"/newFitcond_GJRKBB_a_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(fit_c_WBB,dir+"/newFitcond_WBB_c_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)
write(fit_a_WBB,dir+"/newFitcond_WBB_a_" + porosity_str + "_" + radius_str + ".tab",ascii,force=1)









