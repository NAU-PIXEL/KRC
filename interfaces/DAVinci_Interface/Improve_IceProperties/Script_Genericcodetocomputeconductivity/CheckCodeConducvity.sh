#!/Applications/davinci.app/Contents/Resources/bin/davinci -f
#
# Check the correctness of the module Compute Conductivity based on 
# Low thermal inertias of icy planetary surfaces Evidence for amorphous ice
# C.Ferrari and A.Lucas
#


source("~/Desktop/Europa/Ice_properties/ComputeK.dvrc")
source("~/Desktop/Europa/Ice_properties/ComputeConductivity.dvrc")





It = create2(20,1,120)


G_c_GJKRBB_026_100_tot = Gamma_T(It,0.1e-3,0.26,0.076,0.33,0.41,1,1,2,1,1)
G_a_GJKRBB_026_100_tot = Gamma_T(It,0.1e-3,0.26,0.076,0.33,0.41,1,1,2,1,2)

G_c_WBB_026_100_tot = Gamma_T(It,0.1e-3,0.26,0.076,0.33,0.41,1,3,3,1,1)
G_a_WBB_026_100_tot = Gamma_T(It,0.1e-3,0.26,0.076,0.33,0.41,1,3,3,1,2)


test_G_c_GJKRBB = ComputeIceInertia(It,0.1e-3,0.26,"GJKRBB","Crystalline")
test_G_a_GJKRBB = ComputeIceInertia(It,0.1e-3,0.26,"GJKRBB","Amorphous")
test_G_c_WBB = ComputeIceInertia(It,0.1e-3,0.26,"WBB","Crystalline")
test_G_a_WBB = ComputeIceInertia(It,0.1e-3,0.26,"WBB","Amorphous")



plot(log10(G_a_GJKRBB_026_100_tot)- log10(test_G_a_GJKRBB), x = It,log10(G_c_GJKRBB_026_100_tot)- log10(test_G_c_GJKRBB), x = It)
pause()
plot(log10(G_a_WBB_026_100_tot)- log10(test_G_a_WBB), x = It,log10(G_c_WBB_026_100_tot)- log10(test_G_c_WBB), x = It)
pause()
