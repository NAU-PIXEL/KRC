#!/Applications/davinci.app/Contents/Resources/bin/davinci -f
#
# Check the Fit of Emissivity computed in krc for icy regolith 
# Based on the work of C.Ferrari and A.Lucas
#


dir = "~/Desktop/Europa/Ice_properties/Emissivity_functionTemp/Fit_emissivitywaterice_temperature/"
source(dir+"Compute_Fit_Emissivity_4KRC.dvrc")

Tmin = 50
Tmax = 150
T_step    = 1.
T_num     = int((Tmax - Tmin)/T_step + 1)
T         = create(T_num,1,1,start=Tmin,step=T_step,format=float)
xt = ( T - 220 ) * 0.01
  

Form = 'Amorphous'
Radius = 1e-6
radius_str = Radius + ""
true_emiss_vs_t = compute_temperaturedependance_purewaterice(Tmin,Tmax,Radius,Form,dir) 
fit = compute_fit_emissivitywaterice_vstemperature(Tmin,Tmax,Radius,Form,dir) 
fit_eval  = fit[1,1,1] + fit[2,1,1]*xt + fit[3,1,1]*xt^2 + fit[4,1,1]*xt^3
plot(true_emiss_vs_t.emissivity,x=T, fit_eval, x= T)
pause()
write(T,dir+"/Temp.tab",ascii,force=1)
write(true_emiss_vs_t.emissivity,dir+"/True_emiss_a_"+ radius_str + ".tab",ascii,force=1)
write(fit_eval,dir+"/Fit_emiss_a_"+ radius_str + ".tab",ascii,force=1)

Form = 'Amorphous'
Radius = 1e-4
radius_str = Radius + ""
true_emiss_vs_t = compute_temperaturedependance_purewaterice(Tmin,Tmax,Radius,Form,dir) 
fit = compute_fit_emissivitywaterice_vstemperature(Tmin,Tmax,Radius,Form,dir) 
fit_eval  = fit[1,1,1] + fit[2,1,1]*xt + fit[3,1,1]*xt^2 + fit[4,1,1]*xt^3
plot(true_emiss_vs_t.emissivity,x=T, fit_eval, x= T)
pause()
write(true_emiss_vs_t.emissivity,dir+"/True_emiss_a_"+ radius_str + ".tab",ascii,force=1)
write(fit_eval,dir+"/Fit_emiss_a_"+ radius_str + ".tab",ascii,force=1)

Form = 'Amorphous'
Radius = 1e-2
radius_str = Radius + ""
true_emiss_vs_t = compute_temperaturedependance_purewaterice(Tmin,Tmax,Radius,Form,dir) 
fit = compute_fit_emissivitywaterice_vstemperature(Tmin,Tmax,Radius,Form,dir) 
fit_eval  = fit[1,1,1] + fit[2,1,1]*xt + fit[3,1,1]*xt^2 + fit[4,1,1]*xt^3
plot(true_emiss_vs_t.emissivity,x=T, fit_eval, x= T)
pause()
write(true_emiss_vs_t.emissivity,dir+"/True_emiss_a_"+ radius_str + ".tab",ascii,force=1)
write(fit_eval,dir+"/Fit_emiss_a_"+ radius_str + ".tab",ascii,force=1)

Form = 'Amorphous'
Radius = 1
radius_str = Radius + ""
true_emiss_vs_t = compute_temperaturedependance_purewaterice(Tmin,Tmax,Radius,Form,dir) 
fit = compute_fit_emissivitywaterice_vstemperature(Tmin,Tmax,Radius,Form,dir) 
fit_eval  = fit[1,1,1] + fit[2,1,1]*xt + fit[3,1,1]*xt^2 + fit[4,1,1]*xt^3
plot(true_emiss_vs_t.emissivity,x=T, fit_eval, x= T)
pause()
write(true_emiss_vs_t.emissivity,dir+"/True_emiss_a_"+ radius_str + ".tab",ascii,force=1)
write(fit_eval,dir+"/Fit_emiss_a_"+ radius_str + ".tab",ascii,force=1)



Form = 'Crystalline'
Radius = 1e-6
radius_str = Radius + ""
true_emiss_vs_t = compute_temperaturedependance_purewaterice(Tmin,Tmax,Radius,Form,dir) 
fit = compute_fit_emissivitywaterice_vstemperature(Tmin,Tmax,Radius,Form,dir) 
fit_eval  = fit[1,1,1] + fit[2,1,1]*xt + fit[3,1,1]*xt^2 + fit[4,1,1]*xt^3
plot(true_emiss_vs_t.emissivity,x=T, fit_eval, x= T)
pause()
write(T,dir+"/Temp.tab",ascii,force=1)
write(true_emiss_vs_t.emissivity,dir+"/True_emiss_c_"+ radius_str + ".tab",ascii,force=1)
write(fit_eval,dir+"/Fit_emiss_c_"+ radius_str + ".tab",ascii,force=1)

Form = 'Crystalline'
Radius = 1e-4
radius_str = Radius + ""
true_emiss_vs_t = compute_temperaturedependance_purewaterice(Tmin,Tmax,Radius,Form,dir) 
fit = compute_fit_emissivitywaterice_vstemperature(Tmin,Tmax,Radius,Form,dir) 
fit_eval  = fit[1,1,1] + fit[2,1,1]*xt + fit[3,1,1]*xt^2 + fit[4,1,1]*xt^3
plot(true_emiss_vs_t.emissivity,x=T, fit_eval, x= T)
pause()
write(true_emiss_vs_t.emissivity,dir+"/True_emiss_c_"+ radius_str + ".tab",ascii,force=1)
write(fit_eval,dir+"/Fit_emiss_c_"+ radius_str + ".tab",ascii,force=1)

Form = 'Crystalline'
Radius = 1e-2
radius_str = Radius + ""
true_emiss_vs_t = compute_temperaturedependance_purewaterice(Tmin,Tmax,Radius,Form,dir) 
fit = compute_fit_emissivitywaterice_vstemperature(Tmin,Tmax,Radius,Form,dir) 
fit_eval  = fit[1,1,1] + fit[2,1,1]*xt + fit[3,1,1]*xt^2 + fit[4,1,1]*xt^3
plot(true_emiss_vs_t.emissivity,x=T, fit_eval, x= T)
pause()
write(true_emiss_vs_t.emissivity,dir+"/True_emiss_c_"+ radius_str + ".tab",ascii,force=1)
write(fit_eval,dir+"/Fit_emiss_c_"+ radius_str + ".tab",ascii,force=1)

Form = 'Crystalline'
Radius = 1
radius_str = Radius + ""
true_emiss_vs_t = compute_temperaturedependance_purewaterice(Tmin,Tmax,Radius,Form,dir) 
fit = compute_fit_emissivitywaterice_vstemperature(Tmin,Tmax,Radius,Form,dir) 
fit_eval  = fit[1,1,1] + fit[2,1,1]*xt + fit[3,1,1]*xt^2 + fit[4,1,1]*xt^3
plot(true_emiss_vs_t.emissivity,x=T, fit_eval, x= T)
pause()
write(true_emiss_vs_t.emissivity,dir+"/True_emiss_c_"+ radius_str + ".tab",ascii,force=1)
write(fit_eval,dir+"/Fit_emiss_c_"+ radius_str + ".tab",ascii,force=1)



