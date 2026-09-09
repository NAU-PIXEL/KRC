#!/Applications/davinci.app/Contents/Resources/bin/davinci -f
#
# compute the Figures of the article:
# Low thermal inertias of icy planetary surfaces Evidence for amorphous ice
# C.Ferrari and A.Lucas
#


source("~/Desktop/Europa/Ice_properties/ComputeK.dvrc")





#Figure 1 Left
Ip =create2(0.26,1e-2,0.84)
printf("Cas 1 \n ")
Krp_BB_0_1 = K_rad_BB_P(Ip,0.1e-3,80,1,0)
printf("Cas 2 \n ")


Krp_BB_1 = K_rad_BB_P(Ip,1e-3,80,1,0)
printf("Cas 3 \n ")

Krp_BB_10 = K_rad_BB_P(Ip,10e-3,80,1,0)
printf("Cas 4 \n ")

Krp_BB_100 = K_rad_BB_P(Ip,100e-3,80,1,0)
printf("Cas 5 \n ")


Krp_GB_0_1 = K_rad_GB_P(Ip,0.1e-3,80,1,0)
printf("Cas 6 \n ")

Krp_GB_1 = K_rad_GB_P(Ip,1e-3,80,1,0)
printf("Cas 7 \n ")

Krp_GB_10 = K_rad_GB_P(Ip,10e-3,80,1,0)
printf("Cas 8 \n ")

Krp_GB_100 = K_rad_GB_P(Ip,100e-3,80,1,0)



plot(log10(Krp_BB_0_1), x=log10(Ip),label="",c = 10,style=lines, log10(Krp_GB_0_1), x=log10(Ip),label= "",c = 10,style=dot, log10(Krp_BB_1), x=log10(Ip),label="",c = 50,style=lines, log10(Krp_GB_1), x=log10(Ip),label="",c = 50,style=dot,log10(Krp_BB_10), x=log10(Ip),label="",c = 100,style=lines, log10(Krp_GB_10), x=log10(Ip),label="",c = 100,style=dot,log10(Krp_BB_100), x=log10(Ip),label="",c = 150,style=lines, log10(Krp_GB_100), x=log10(Ip),label="",c = 150,style=dot)



pause()

#Figure 1 right

It =create2(10,1,130)

Krt_BB_1 = K_rad_BB_T(It,1e-2,0.5,1,0)
Krt_BB_05 = K_rad_BB_T(It,1e-2,0.5,0.5,0)

Krt_GB_1 = K_rad_GB_T(It,1e-2,0.5,1,0)
Krt_GB_05 = K_rad_GB_T(It,1e-2,0.5,0.5,0)

plot(log10(Krt_BB_1), x=(It),label="",c = 10,style=lines, log10(Krt_GB_1), x=(It),label= "",c = 10,style=dot, log10(Krt_BB_05), x=(It),label="",c = 50,style=lines, log10(Krt_GB_05), x=(It),label="",c = 50,style=dot)

pause()





#Figure 2 Left

Kc_c_01_W = K_contact_P(Ip,0.1e-3,80,0.076,0.33,0.41,3,3,1)
Kc_c_1_W = K_contact_P(Ip,1e-3,80,0.076,0.33,0.41,3,3,1)
Kc_c_10_W = K_contact_P(Ip,1e-2,80,0.076,0.33,0.41,3,3,1)
Kc_c_100_W = K_contact_P(Ip,1e-1,80,0.076,0.33,0.41,3,3,1)




Kc_c_01_GJKR = K_contact_P(Ip,0.1e-3,80,0.076,0.33,0.41,1,2,1)

Kc_c_1_GJKR = K_contact_P(Ip,1e-3,80,0.076,0.33,0.41,1,2,1)
Kc_c_10_GJKR = K_contact_P(Ip,1e-2,80,0.076,0.33,0.41,1,2,1)
Kc_c_100_GJKR = K_contact_P(Ip,1e-1,80,0.076,0.33,0.41,1,2,1)


Kc_c_01_GB = K_contact_P(Ip,0.1e-3,80,0.076,0.33,0.41,2,2,1)
Kc_c_1_GB = K_contact_P(Ip,1e-3,80,0.076,0.33,0.41,2,2,1)
Kc_c_10_GB = K_contact_P(Ip,1e-2,80,0.076,0.33,0.41,2,2,1)
Kc_c_100_GB = K_contact_P(Ip,1e-1,80,0.076,0.33,0.41,2,2,1)


plot(log10(Kc_c_01_W), x=(Ip),label="",c = 10,style=lines, log10(Kc_c_01_GJKR), x=(Ip),label= "",c = 10,style=linespoints,log10(Kc_c_01_GB), x =Ip,label="", c=10,style=points,log10(Kc_c_1_W), x=(Ip),label="",c = 50,style=lines, log10(Kc_c_1_GJKR), x=(Ip),label= "",c = 50,style=linespoints,log10(Kc_c_1_GB), x =Ip,label="", c=50,style=points,  log10(Kc_c_10_W), x=(Ip),label="",c = 100,style=lines, log10(Kc_c_10_GJKR), x=(Ip),label= "",c = 100,style=linespoints,log10(Kc_c_10_GB), x =Ip,label="", c=100,style=points,  log10(Kc_c_100_W), x=(Ip),label="",c = 150,style=lines, log10(Kc_c_100_GJKR), x=(Ip),label= "",c = 150,style=linespoints,log10(Kc_c_100_GB), x =Ip,label="", c=150,style=points)  



pause()


#Figure 2 Right

Kc_a_01_W = K_contact_P(Ip,0.1e-3,80,0.076,0.33,0.41,3,3,2)
Kc_a_1_W = K_contact_P(Ip,1e-3,80,0.076,0.33,0.41,3,3,2)
Kc_a_10_W = K_contact_P(Ip,1e-2,80,0.076,0.33,0.41,3,3,2)
Kc_a_100_W = K_contact_P(Ip,1e-1,80,0.076,0.33,0.41,3,3,2)




Kc_a_01_GJKR = K_contact_P(Ip,0.1e-3,80,0.076,0.33,0.41,1,2,2)

Kc_a_1_GJKR = K_contact_P(Ip,1e-3,80,0.076,0.33,0.41,1,2,2)
Kc_a_10_GJKR = K_contact_P(Ip,1e-2,80,0.076,0.33,0.41,1,2,2)
Kc_a_100_GJKR = K_contact_P(Ip,1e-1,80,0.076,0.33,0.41,1,2,2)


Kc_a_01_GB = K_contact_P(Ip,0.1e-3,80,0.076,0.33,0.41,2,2,2)
Kc_a_1_GB = K_contact_P(Ip,1e-3,80,0.076,0.33,0.41,2,2,2)
Kc_a_10_GB = K_contact_P(Ip,1e-2,80,0.076,0.33,0.41,2,2,2)
Kc_a_100_GB = K_contact_P(Ip,1e-1,80,0.076,0.33,0.41,2,2,2)


plot(log10(Kc_a_01_W), x=(Ip),label="",c = 10,style=lines, log10(Kc_a_01_GJKR), x=(Ip),label= "",c = 10,style=linespoints,log10(Kc_a_01_GB), x =Ip,label="", c=10,style=points,log10(Kc_a_1_W), x=(Ip),label="",c = 50,style=lines, log10(Kc_a_1_GJKR), x=(Ip),label= "",c = 50,style=linespoints,log10(Kc_a_1_GB), x =Ip,label="", c=50,style=points,  log10(Kc_a_10_W), x=(Ip),label="",c = 100,style=lines, log10(Kc_a_10_GJKR), x=(Ip),label= "",c = 100,style=linespoints,log10(Kc_a_10_GB), x =Ip,label="", c=100,style=points,  log10(Kc_a_100_W), x=(Ip),label="",c = 150,style=lines, log10(Kc_a_100_GJKR), x=(Ip),label= "",c = 150,style=linespoints,log10(Kc_a_100_GB), x =Ip,label="", c=150,style=points)  



pause()

#Figure droite, sens horaire


Ir = create( int((100e-3-0.01e-3)/(0.01e-3)), 1,1,format = double,  start = 0.01e-3, step = 0.01e-3)



Ke_c_JRKGBB_26 = K_E_R(Ir,80,0.26,0.076,0.33,0.41,1,1,2,1,1)
Ke_a_JRKGBB_26 = K_E_R(Ir,80,0.26,0.076,0.33,0.41,1,1,2,1,2)
Krad_c_BB_26 = K_rad_BB_R(Ir,0.26,80,1,1)

Ke_c_JRKGBB_50 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,1,2,1,1)
Ke_a_JRKGBB_50 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,1,2,1,2)
Krad_c_BB_50 = K_rad_BB_R(Ir,0.5,80,1,1)

Ke_c_JRKGBB_84 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,1,2,1,1)
Ke_a_JRKGBB_84 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,1,2,1,2)
Krad_c_BB_84 = K_rad_BB_R(Ir,0.5,80,1,1)

plot(log10(Ke_c_JRKGBB_26), x=log10(Ir),label="",c = 10,style=lines, log10(Ke_a_JRKGBB_26), x=log10(Ir),label= "",c = 10,style=points,log10(Krad_c_BB_26), x =log10(Ir),label="", c=10,style=linespoints,log10(Ke_c_JRKGBB_50), x=log10(Ir),label="",c = 50,style=lines, log10(Ke_a_JRKGBB_50), x=log10(Ir),label= "",c = 50,style=points,log10(Krad_c_BB_50), x =log10(Ir),label="", c=50,style=linespoints,log10(Ke_c_JRKGBB_84), x=log10(Ir),label="",c = 100,style=lines, log10(Ke_a_JRKGBB_84), x=log10(Ir),label= "",c = 100,style=points,log10(Krad_c_BB_84), x =log10(Ir),label="", c=100,style=linespoints)

pause()


Ke_c_GB_26 = K_E_R(Ir,80,0.26,0.076,0.33,0.41,1,2,1,2,1)
Ke_a_GB_26 = K_E_R(Ir,80,0.26,0.076,0.33,0.41,1,2,1,2,2)
Krad_c_GB_26 = K_rad_GB_R(Ir,0.26,80,1,1)

Ke_c_GB_50 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,2,1,2,1)
Ke_a_GB_50 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,2,1,2,2)
Krad_c_GB_50 = K_rad_GB_R(Ir,0.5,80,1,1)

Ke_c_GB_84 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,2,1,2,1)
Ke_a_GB_84 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,2,1,2,2)
Krad_c_GB_84 = K_rad_GB_R(Ir,0.84,80,1,1)

plot(log10(Ke_c_GB_26), x=log10(Ir),label="",c = 10,style=lines, log10(Ke_a_GB_26), x=log10(Ir),label= "",c = 10,style=points,log10(Krad_c_GB_26), x =log10(Ir),label="", c=10,style=linespoints,log10(Ke_c_GB_50), x=log10(Ir),label="",c = 50,style=lines, log10(Ke_a_GB_50), x=log10(Ir),label= "",c = 50,style=points,log10(Krad_c_GB_50), x =log10(Ir),label="", c=50,style=linespoints,log10(Ke_c_GB_84), x=log10(Ir),label="",c = 100,style=lines, log10(Ke_a_GB_84), x=log10(Ir),label= "",c = 100,style=points,log10(Krad_c_GB_84), x =log10(Ir),label="", c=100,style=linespoints)

pause()



Ke_c_WBB_26 = K_E_R(Ir,80,0.26,0.076,0.33,0.41,1,3,3,1,1)
Ke_a_WBB_26 = K_E_R(Ir,80,0.26,0.076,0.33,0.41,1,3,3,1,2)
Krad_c_WBB_26 = K_rad_BB_R(Ir,0.26,80,1,1)

Ke_c_WBB_50 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,3,3,1,1)
Ke_a_WBB_50 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,3,3,1,2)
Krad_c_WBB_50 = K_rad_BB_R(Ir,0.5,80,1,1)

Ke_c_WBB_84 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,3,3,1,1)
Ke_a_WBB_84 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,3,3,1,2)
Krad_c_WBB_84 = K_rad_BB_R(Ir,0.84,80,1,1)

plot(log10(Ke_c_WBB_26), x=log10(Ir),label="",c = 10,style=lines, log10(Ke_a_WBB_26), x=log10(Ir),label= "",c = 10,style=points,log10(Krad_c_WBB_26), x =log10(Ir),label="", c=10,style=linespoints,log10(Ke_c_WBB_50), x=log10(Ir),label="",c = 50,style=lines, log10(Ke_a_WBB_50), x=log10(Ir),label= "",c = 50,style=points,log10(Krad_c_WBB_50), x =log10(Ir),label="", c=50,style=linespoints,log10(Ke_c_WBB_84), x=log10(Ir),label="",c = 100,style=lines, log10(Ke_a_WBB_84), x=log10(Ir),label= "",c = 100,style=points,log10(Krad_c_WBB_84), x =log10(Ir),label="", c=100,style=linespoints)

pause()


Ke_c_WGB_26 = K_E_R(Ir,80,0.26,0.076,0.33,0.41,1,3,3,2,1)
Ke_a_WGB_26 = K_E_R(Ir,80,0.26,0.076,0.33,0.41,1,3,3,2,2)
Krad_c_WGB_26 = K_rad_GB_R(Ir,0.26,80,1,1)

Ke_c_WGB_50 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,3,3,2,1)
Ke_a_WGB_50 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,3,3,2,2)
Krad_c_WGB_50 = K_rad_GB_R(Ir,0.5,80,1,1)

Ke_c_WGB_84 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,3,3,2,1)
Ke_a_WGB_84 = K_E_R(Ir,80,0.5,0.076,0.33,0.41,1,3,3,2,2)
Krad_c_WGB_84 = K_rad_GB_R(Ir,0.84,80,1,1)

plot(log10(Ke_c_WGB_26), x=log10(Ir),label="",c = 10,style=lines, log10(Ke_a_WGB_26), x=log10(Ir),label= "",c = 10,style=points,log10(Krad_c_WGB_26), x =log10(Ir),label="", c=10,style=linespoints,log10(Ke_c_WGB_50), x=log10(Ir),label="",c = 50,style=lines, log10(Ke_a_WGB_50), x=log10(Ir),label= "",c = 50,style=points,log10(Krad_c_WGB_50), x =log10(Ir),label="", c=50,style=linespoints,log10(Ke_c_WGB_84), x=log10(Ir),label="",c = 100,style=lines, log10(Ke_a_WGB_84), x=log10(Ir),label= "",c = 100,style=points,log10(Krad_c_WGB_84), x =log10(Ir),label="", c=100,style=linespoints)

pause()



#Figure 4 Top

G_c_GJKRBB_026 = Gamma_R(Ir,80,0.26,0.076,0.33,0.41,1,1,2,1,1)
G_a_GJKRBB_026 = Gamma_R(Ir,80,0.26,0.076,0.33,0.41,1,1,2,1,2)

G_c_GJKRBB_05 = Gamma_R(Ir,80,0.5,0.076,0.33,0.41,1,1,2,1,1)
G_a_GJKRBB_05 = Gamma_R(Ir,80,0.5,0.076,0.33,0.41,1,1,2,1,2)

G_c_GJKRBB_084 = Gamma_R(Ir,80,0.84,0.076,0.33,0.41,1,1,2,1,1)
G_a_GJKRBB_084 = Gamma_R(Ir,80,0.84,0.076,0.33,0.41,1,1,2,1,2)




G1 =  16*create(length(Ir),1,1, start =1,step = 0)
G2min  = (66-23)*create(length(Ir),1,1, start =1,step = 0)
G2max  = (66+23)*create(length(Ir),1,1, start =1,step = 0)



plot(log10(G_c_GJKRBB_026), x = log10(Ir),label = "",c = 10,style =lines,log10(G_a_GJKRBB_026), x = log10(Ir),label = "",c = 10,style =points,log10(G_c_GJKRBB_05), x = log10(Ir),label = "",c = 10,style =lines,log10(G_a_GJKRBB_05), x = log10(Ir),label = "",c = 50,style =points,log10(G_c_GJKRBB_084), x = log10(Ir),label = "",c = 100,style =lines,log10(G_a_GJKRBB_084), x = log10(Ir),label = "",c = 100,style =points,log10(G1), x = log10(Ir),label = "",c = 1,style =points,log10(G2min), x = log10(Ir),label = "",c = 1,style =points,log10(G2max), x = log10(Ir),label = "",c = 1,style =points)


pause()



#Figure 4 Bottom

G_c_WBB_026 = Gamma_R(Ir,80,0.26,0.076,0.33,0.41,1,3,3,1,1)
G_a_WBB_026 = Gamma_R(Ir,80,0.26,0.076,0.33,0.41,1,3,3,1,1)

G_c_WBB_05 = Gamma_R(Ir,80,0.26,0.076,0.33,0.41,1,3,3,1,1)
G_a_WBB_05 = Gamma_R(Ir,80,0.26,0.076,0.33,0.41,1,3,3,1,1)

G_c_WBB_084 = Gamma_R(Ir,80,0.26,0.076,0.33,0.41,1,3,3,1,1)
G_a_WBB_084 = Gamma_R(Ir,80,0.26,0.076,0.33,0.41,1,3,3,1,1)







plot(log10(G_c_WBB_026), x = log10(Ir),label = "",c = 10,style =lines,log10(G_a_WBB_026), x = log10(Ir),label = "",c = 10,style =points,log10(G_c_WBB_05), x = log10(Ir),label = "",c = 10,style =lines,log10(G_a_WBB_05), x = log10(Ir),label = "",c = 50,style =points,log10(G_c_WBB_084), x = log10(Ir),label = "",c = 100,style =lines,log10(G_a_WBB_084), x = log10(Ir),label = "",c = 100,style =points,log10(G1), x = log10(Ir),label = "",c = 1,style =points,log10(G2min), x = log10(Ir),label = "",c = 1,style =points,log10(G2max), x = log10(Ir),label = "",c = 1,style =points)


pause()






# Figure 5a


It = create2(20,1,120)


G_c_GJKRBB_026_100_tot = Gamma_T(It,0.1e-3,0.26,0.076,0.33,0.41,1,1,2,1,1)
G_a_GJKRBB_026_100_tot = Gamma_T(It,0.1e-3,0.26,0.076,0.33,0.41,1,1,2,1,2)
G_c_GJKRBB_026_100_bulk = Gamma_bulk_T(It,0.26,1)
G_a_GJKRBB_026_100_bulk = Gamma_bulk_T(It,0.26,2)
G_c_GJKRBB_026_100_rad = Gamma_rad_T(It,0.1e-3,0.26,1,1,1)


G_c_GJKRBB_05_100_tot = Gamma_T(It,0.1e-3,0.5,0.076,0.33,0.41,1,1,2,1,1)
G_a_GJKRBB_05_100_tot = Gamma_T(It,0.1e-3,0.5,0.076,0.33,0.41,1,1,2,1,2)
G_c_GJKRBB_05_100_bulk = Gamma_bulk_T(It,0.5,1)
G_a_GJKRBB_05_100_bulk = Gamma_bulk_T(It,0.5,2)
G_c_GJKRBB_05_100_rad = Gamma_rad_T(It,0.1e-3,0.5,1,1,1)


G_c_GJKRBB_084_100_tot = Gamma_T(It,0.1e-3,0.84,0.076,0.33,0.41,1,1,2,1,1)
G_a_GJKRBB_084_100_tot = Gamma_T(It,0.1e-3,0.84,0.076,0.33,0.41,1,1,2,1,2)
G_c_GJKRBB_084_100_bulk = Gamma_bulk_T(It,0.84,1)
G_a_GJKRBB_084_100_bulk = Gamma_bulk_T(It,0.84,2)
G_c_GJKRBB_084_100_rad = Gamma_rad_T(It,0.1e-3,0.84,1,1,1)


plot(log10(G_c_GJKRBB_026_100_tot), x = It,label ="",c=10,style=lines,log10(G_c_GJKRBB_05_100_tot), x = It,label ="",c=50,style=lines,log10(G_c_GJKRBB_084_100_tot), x = It,label ="",c=100,style=lines,log10(G_a_GJKRBB_026_100_tot), x = It, label = "", c=10, style = points,log10(G_a_GJKRBB_05_100_tot), x = It, label = "", c=50, style = points,log10(G_a_GJKRBB_084_100_tot), x = It, label = "", c=100, style = points,log10(G_c_GJKRBB_026_100_bulk),x=It,label = "", c=10, style=linespoints,log10(G_c_GJKRBB_05_100_bulk),x=It,label = "", c=50, style=linespoints,log10(G_c_GJKRBB_084_100_bulk),x=It,label = "", c=100, style=linespoints,log10(G_a_GJKRBB_026_100_bulk),x=It,label = "", c=15, style=linespoints,log10(G_a_GJKRBB_05_100_bulk),x=It,label = "", c=55, style=linespoints,log10(G_a_GJKRBB_084_100_bulk),x=It,label = "", c=105, style=linespoints,log10(G_c_GJKRBB_026_100_rad),x=It,label = "", c=10, style=dots,log10(G_c_GJKRBB_05_100_rad),x=It,label = "", c=50, style=dots,log10(G_c_GJKRBB_05_100_rad),x=It,label = "", c=100, style=dots)



pause()


#Figure 5b


It = create2(20,1,120)


G_c_GJKRBB_026_1_tot = Gamma_T(It,1e-2,0.26,0.076,0.33,0.41,1,1,2,1,1)
G_a_GJKRBB_026_1_tot = Gamma_T(It,1e-2,0.26,0.076,0.33,0.41,1,1,2,1,2)
G_c_GJKRBB_026_1_bulk = Gamma_bulk_T(It,0.26,1)
G_a_GJKRBB_026_1_bulk = Gamma_bulk_T(It,0.26,2)
G_c_GJKRBB_026_1_rad = Gamma_rad_T(It,1e-2,0.26,1,1,1)


G_c_GJKRBB_05_1_tot = Gamma_T(It,1e-2,0.5,0.076,0.33,0.41,1,1,2,1,1)
G_a_GJKRBB_05_1_tot = Gamma_T(It,0.1e-2,0.5,0.076,0.33,0.41,1,1,2,1,2)
G_c_GJKRBB_05_1_bulk = Gamma_bulk_T(It,0.5,1)
G_a_GJKRBB_05_1_bulk = Gamma_bulk_T(It,0.5,2)
G_c_GJKRBB_05_1_rad = Gamma_rad_T(It,0.1e-1,0.5,1,1,1)


G_c_GJKRBB_084_1_tot = Gamma_T(It,0.1e-1,0.84,0.076,0.33,0.41,1,1,2,1,1)
G_a_GJKRBB_084_1_tot = Gamma_T(It,0.1e-1,0.84,0.076,0.33,0.41,1,1,2,1,2)
G_c_GJKRBB_084_1_bulk = Gamma_bulk_T(It,0.84,1)
G_a_GJKRBB_084_1_bulk = Gamma_bulk_T(It,0.84,2)
G_c_GJKRBB_084_1_rad = Gamma_rad_T(It,0.1e-1,0.84,1,1,1)


plot(log10(G_c_GJKRBB_026_1_tot), x = It,label ="",c=10,style=lines,log10(G_c_GJKRBB_05_1_tot), x = It,label ="",c=50,style=lines,log10(G_c_GJKRBB_084_1_tot), x = It,label ="",c=100,style=lines,log10(G_a_GJKRBB_026_1_tot), x = It, label = "", c=10, style = points,log10(G_a_GJKRBB_05_1_tot), x = It, label = "", c=50, style = points,log10(G_a_GJKRBB_084_1_tot), x = It, label = "", c=100, style = points,log10(G_c_GJKRBB_026_1_bulk),x=It,label = "", c=10, style=linespoints,log10(G_c_GJKRBB_05_1_bulk),x=It,label = "", c=50, style=linespoints,log10(G_c_GJKRBB_084_1_bulk),x=It,label = "", c=100, style=linespoints,log10(G_a_GJKRBB_026_1_bulk),x=It,label = "", c=15, style=linespoints,log10(G_a_GJKRBB_05_1_bulk),x=It,label = "", c=55, style=linespoints,log10(G_a_GJKRBB_084_1_bulk),x=It,label = "", c=105, style=linespoints,log10(G_c_GJKRBB_026_1_rad),x=It,label = "", c=10, style=dots,log10(G_c_GJKRBB_05_1_rad),x=It,label = "", c=50, style=dots,log10(G_c_GJKRBB_05_1_rad),x=It,label = "", c=100, style=dots)
pause()
#Figure 5c


G_c_WBB_026_100_tot = Gamma_T(It,0.1e-3,0.26,0.076,0.33,0.41,1,3,3,1,1)
G_a_WBB_026_100_tot = Gamma_T(It,0.1e-3,0.26,0.076,0.33,0.41,1,3,3,1,2)
G_c_WBB_026_100_bulk = Gamma_bulk_T(It,0.26,1)
G_a_WBB_026_100_bulk = Gamma_bulk_T(It,0.26,2)
G_c_WBB_026_100_rad = Gamma_rad_T(It,0.1e-3,0.26,1,2,1)


G_c_WBB_05_100_tot = Gamma_T(It,0.1e-3,0.5,0.076,0.33,0.41,1,3,3,1,1)
G_a_WBB_05_100_tot = Gamma_T(It,0.1e-3,0.5,0.076,0.33,0.41,1,3,3,1,2)
G_c_WBB_05_100_bulk = Gamma_bulk_T(It,0.5,1)
G_a_WBB_05_100_bulk = Gamma_bulk_T(It,0.5,2)
G_c_WBB_05_100_rad = Gamma_rad_T(It,0.1e-3,0.5,1,2,1)


G_c_WBB_084_100_tot = Gamma_T(It,0.1e-3,0.84,0.076,0.33,0.41,1,3,3,1,1)
G_a_WBB_084_100_tot = Gamma_T(It,0.1e-3,0.84,0.076,0.33,0.41,1,3,3,1,2)
G_c_WBB_084_100_bulk = Gamma_bulk_T(It,0.84,1)
G_a_WBB_084_100_bulk = Gamma_bulk_T(It,0.84,2)
G_c_WBB_084_100_rad = Gamma_rad_T(It,0.1e-3,0.84,1,2,1)


plot(log10(G_c_WBB_026_100_tot), x = It,label ="",c=10,style=lines,log10(G_c_WBB_05_100_tot), x = It,label ="",c=50,style=lines,log10(G_c_WBB_084_100_tot), x = It,label ="",c=100,style=lines,log10(G_a_WBB_026_100_tot), x = It, label = "", c=10, style = points,log10(G_a_WBB_05_100_tot), x = It, label = "", c=50, style = points,log10(G_a_WBB_084_100_tot), x = It, label = "", c=100, style = points,log10(G_c_WBB_026_100_bulk),x=It,label = "", c=10, style=linespoints,log10(G_c_WBB_05_100_bulk),x=It,label = "", c=50, style=linespoints,log10(G_c_WBB_084_100_bulk),x=It,label = "", c=100, style=linespoints,log10(G_a_WBB_026_100_bulk),x=It,label = "", c=15, style=linespoints,log10(G_a_WBB_05_100_bulk),x=It,label = "", c=55, style=linespoints,log10(G_a_WBB_084_100_bulk),x=It,label = "", c=105, style=linespoints,log10(G_c_WBB_026_100_rad),x=It,label = "", c=10, style=dots,log10(G_c_WBB_05_100_rad),x=It,label = "", c=50, style=dots,log10(G_c_WBB_05_100_rad),x=It,label = "", c=100, style=dots)

pause()
#Figure 5d




G_c_WBB_026_1_tot = Gamma_T(It,1e-2,0.26,0.076,0.33,0.41,1,3,3,1,1)
G_a_WBB_026_1_tot = Gamma_T(It,1e-2,0.26,0.076,0.33,0.41,1,3,3,1,1)
G_c_WBB_026_1_bulk = Gamma_bulk_T(It,0.26,1)
G_a_WBB_026_1_bulk = Gamma_bulk_T(It,0.26,2)
G_c_WBB_026_1_rad = Gamma_rad_T(It,1e-2,0.26,1,2,1)


G_c_WBB_05_1_tot = Gamma_T(It,1e-2,0.5,0.076,0.33,0.41,1,3,3,1,1)
G_a_WBB_05_1_tot = Gamma_T(It,0.1e-2,0.5,0.076,0.33,0.41,1,3,3,1,1)
G_c_WBB_05_1_bulk = Gamma_bulk_T(It,0.5,1)
G_a_WBB_05_1_bulk = Gamma_bulk_T(It,0.5,2)
G_c_WBB_05_1_rad = Gamma_rad_T(It,0.1e-1,0.5,1,2,1)


G_c_WBB_084_1_tot = Gamma_T(It,0.1e-1,0.84,0.076,0.33,0.41,1,3,3,1,1)
G_a_WBB_084_1_tot = Gamma_T(It,0.1e-1,0.84,0.076,0.33,0.41,1,3,3,1,1)
G_c_WBB_084_1_bulk = Gamma_bulk_T(It,0.84,1)
G_a_WBB_084_1_bulk = Gamma_bulk_T(It,0.84,2)
G_c_WBB_084_1_rad = Gamma_rad_T(It,0.1e-1,0.84,1,2,1)


plot(log10(G_c_WBB_026_1_tot), x = It,label ="",c=10,style=lines,log10(G_c_WBB_05_1_tot), x = It,label ="",c=50,style=lines,log10(G_c_WBB_084_1_tot), x = It,label ="",c=100,style=lines,log10(G_a_WBB_026_1_tot), x = It, label = "", c=10, style = points,log10(G_a_WBB_05_1_tot), x = It, label = "", c=50, style = points,log10(G_a_WBB_084_1_tot), x = It, label = "", c=100, style = points,log10(G_c_WBB_026_1_bulk),x=It,label = "", c=10, style=linespoints,log10(G_c_WBB_05_1_bulk),x=It,label = "", c=50, style=linespoints,log10(G_c_WBB_084_1_bulk),x=It,label = "", c=100, style=linespoints,log10(G_a_WBB_026_1_bulk),x=It,label = "", c=15, style=linespoints,log10(G_a_WBB_05_1_bulk),x=It,label = "", c=55, style=linespoints,log10(G_a_WBB_084_1_bulk),x=It,label = "", c=105, style=linespoints,log10(G_c_WBB_026_1_rad),x=It,label = "", c=10, style=dots,log10(G_c_WBB_05_1_rad),x=It,label = "", c=50, style=dots,log10(G_c_WBB_05_1_rad),x=It,label = "", c=100, style=dots)



##

R_c_1 = inverse_Gamma_porosity(Ip,70,90,1,0.076,0.33,0.41,1.0e-9,1,1)
R_c_2 = inverse_Gamma_porosity(Ip,70,90,1,0.076,0.33,0.41,1.0e-9,2,1)
R_c_3 = inverse_Gamma_porosity(Ip,70,90,1,0.076,0.33,0.41,1.0e-9,3,1)
R_c_4 = inverse_Gamma_porosity(Ip,70,90,1,0.076,0.33,0.41,1.0e-9,4,1)


R_a_1 = inverse_Gamma_porosity(Ip,70,90,1,0.076,0.33,0.41,1.0e-9,1,2)
R_a_2 = inverse_Gamma_porosity(Ip,70,90,1,0.076,0.33,0.41,1.0e-9,2,2)
R_a_3 = inverse_Gamma_porosity(Ip,70,90,1,0.076,0.33,0.41,1.0e-9,3,2)
R_a_4 = inverse_Gamma_porosity(Ip,70,90,1,0.076,0.33,0.41,1.0e-9,4,2)





plot(log10(R_c_1), x = Ip, label = " Crystalline, Model 1",ignore = 0, style = points,log10(R_c_2), x = Ip, label = " Crystalline, Model 2",ignore = 0, style = points, log10(R_c_3), x = Ip, label = " Crystalline, Model 3",ignore = 0, style = points,log10(R_c_4), x = Ip, label = " Crystalline, Model 4",ignore = 0, style = points,log10(R_a_1), x = Ip, label = " Amorphous, Model 1",ignore = 0, style = points,log10(R_a_2), x = Ip, label = " Amorphous, Model 2",ignore = 0, style = points,log10(R_a_3), x = Ip, label = " Amorphous, Model 3",ignore = 0, style = points,log10(R_a_4), x = Ip, label = " Amorphous, Model 4",ignore = 0, style = points)

labelxy("Porosity", "log10(grain size) [m]")

