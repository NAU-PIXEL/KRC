#Program to determine how one can differenciate solar absorption vs high ti material

OUT_pen1cm = krc(lat=0,INERTIA=33.76,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95,RADGND='T', EFOLD_RADGND = 1e-2)
OUT_pen2cm = krc(lat=0,INERTIA=33.76,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1, lbound = -2//95,RADGND='T', EFOLD_RADGND = 2e-2)
OUT_pen4cm = krc(lat=0,INERTIA=33.76,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95,RADGND='T', EFOLD_RADGND = 4e-2)

TI_Table       = create2(33.,1.,150.)
RMS_Table       =0*TI_Table
for(h=1;h<dim(TI_Table)[2];h+=1){
current_TI = TI_Table[1,h,1]
OUT_nopen_higherti = krc(lat=0,INERTIA=current_TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
rms = sqrt(avg((OUT_pen4cm.tsurf[,1,143]-OUT_nopen_higherti.tsurf[,1,143])*(OUT_pen4cm.tsurf[,1,143]-OUT_nopen_higherti.tsurf[,1,143])))
RMS_Table[1,h,1] = rms
}

OUT_nopen_higherti = krc(lat=0,INERTIA=80,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrmsp20 = krc(lat=0,INERTIA=60,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrmsm20 = krc(lat=0,INERTIA=100,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrms_loweralb = krc(lat=0,INERTIA=80,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.62,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrms_higheralb = krc(lat=0,INERTIA=100,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.82,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)


write(OUT_pen4cm.tsurf[,1,143],"KRC_solarpen_eps=4cm.ascii",ascii,force=1)
write(OUT_nopen_higherti.tsurf[,1,143],"KRC_solarpen_eps=4cm_noabs_bestfitti.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrmsp20.tsurf[,1,143],"KRC_solarpen_eps=4cm_noabs_bestfitti_p20.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrmsm20.tsurf[,1,143],"KRC_solarpen_eps=4cm_noabs_bestfitti_m20.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrms_loweralb.tsurf[,1,143],"KRC_solarpen_eps=4cm_noabs_bestfitti_albm01.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrms_higheralb.tsurf[,1,143],"KRC_solarpen_eps=4cm_noabs_bestfitti_albp01.ascii",ascii,force=1)
write(OUT_pen4cm.time,"time.ascii",ascii,force=1)


TI_Table       = create2(33.,1.,150.)
RMS_Table       =0*TI_Table
for(h=1;h<dim(TI_Table)[2];h+=1){
current_TI = TI_Table[1,h,1]
OUT_nopen_higherti = krc(lat=0,INERTIA=current_TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
rms = sqrt(avg((OUT_pen2cm.tsurf[,1,143]-OUT_nopen_higherti.tsurf[,1,143])*(OUT_pen2cm.tsurf[,1,143]-OUT_nopen_higherti.tsurf[,1,143])))
RMS_Table[1,h,1] = rms
}

OUT_nopen_higherti = krc(lat=0,INERTIA=56,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrmsp20 = krc(lat=0,INERTIA=26,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrmsm20 = krc(lat=0,INERTIA=76,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrms_loweralb = krc(lat=0,INERTIA=56,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.62,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrms_higheralb = krc(lat=0,INERTIA=56,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.82,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)

write(OUT_pen2cm.tsurf[,1,143],"KRC_solarpen_eps=2cm.ascii",ascii,force=1)
write(OUT_nopen_higherti.tsurf[,1,143],"KRC_solarpen_eps=2cm_noabs_bestfitti.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrmsp20.tsurf[,1,143],"KRC_solarpen_eps=2cm_noabs_bestfitti_p20.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrmsm20.tsurf[,1,143],"KRC_solarpen_eps=2cm_noabs_bestfitti_m20.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrms_loweralb.tsurf[,1,143],"KRC_solarpen_eps=2cm_noabs_bestfitti_albm01.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrms_higheralb.tsurf[,1,143],"KRC_solarpen_eps=2cm_noabs_bestfitti_albp01.ascii",ascii,force=1)
write(OUT_pen4cm.time,"time.ascii",ascii,force=1)



TI_Table       = create2(33.,1.,150.)
RMS_Table       =0*TI_Table
for(h=1;h<dim(TI_Table)[2];h+=1){
current_TI = TI_Table[1,h,1]
OUT_nopen_higherti = krc(lat=0,INERTIA=current_TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
rms = sqrt(avg((OUT_pen1cm.tsurf[,1,143]-OUT_nopen_higherti.tsurf[,1,143])*(OUT_pen1cm.tsurf[,1,143]-OUT_nopen_higherti.tsurf[,1,143])))
RMS_Table[1,h,1] = rms
}


OUT_nopen_higherti = krc(lat=0,INERTIA=43,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrmsp20 = krc(lat=0,INERTIA=23,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrmsm20 = krc(lat=0,INERTIA=63,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrms_loweralb = krc(lat=0,INERTIA=43,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.62,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_nopen_higherti_bestrms_higheralb = krc(lat=0,INERTIA=43,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.82,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)



write(OUT_pen1cm.tsurf[,1,143],"KRC_solarpen_eps=1cm.ascii",ascii,force=1)
write(OUT_nopen_higherti.tsurf[,1,143],"KRC_solarpen_eps=1cm_noabs_bestfitti.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrmsp20.tsurf[,1,143],"KRC_solarpen_eps=1cm_noabs_bestfitti_p20.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrmsm20.tsurf[,1,143],"KRC_solarpen_eps=1cm_noabs_bestfitti_m20.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrms_loweralb.tsurf[,1,143],"KRC_solarpen_eps=1cm_noabs_bestfitti_albm01.ascii",ascii,force=1)
write(OUT_nopen_higherti_bestrms_higheralb.tsurf[,1,143],"KRC_solarpen_eps=1cm_noabs_bestfitti_albp01.ascii",ascii,force=1)
write(OUT_pen4cm.time,"time.ascii",ascii,force=1)












bestfittip20 = np.loadtxt('KRC_solarpen_eps=4cm_noabs_bestfitti_p20.ascii')
bestfittim20 = np.loadtxt('KRC_solarpen_eps=4cm_noabs_bestfitti_m20.ascii')
bestfittip20 = np.loadtxt('KRC_solarpen_eps=4cm_noabs_bestfitti_p20.ascii')
import numpy as np
import matplotlib.pyplot as plt
plt.plot(time,solarpen,'k',label='e-folding = 4cm')
plt.plot(time,bestfitti,'k',linestyle='dotted',label='No Solar Absorption,TI-best fit')
plt.plot(time,bestfittip20,'k',linestyle='dashed',label='No Solar Absorption,TI-best fit + 20SI')
plt.plot(time,bestfittim20,'k',linest