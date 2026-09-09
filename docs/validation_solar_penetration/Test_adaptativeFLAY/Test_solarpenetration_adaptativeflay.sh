#!/Applications/davinci.app/Contents/Resources/bin/davinci -f
#
# Check the implementation of the adaptative FLAY for the solar aborption in the ground
#


#33.76 for low ti case, 133.76 else
TI = 33.76
rhoc = 150*1000
k = TI*TI/rhoc
P = 3.55*86400
delta = sqrt(P*k/(pi*rhoc))


OUT = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95,RADGND='T', EFOLD_RADGND = 1e-2,KEEP='T')



OUT2 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.01,lbound = -2//95,RADGND='T', EFOLD_RADGND = 1e-2)

plot(OUT.tsurf[,1,1],xaxis= OUT.time,"Penetration in the first centimer, ADAPTATIVE FLAY = 0.1",w=2,color=2, OUT2.tsurf[,1,1],xaxis= OUT2.time,"Penetration in the first centimer, FLAY = 0.01",w=2,color=3,OUT3.tsurf[,1,1],xaxis= OUT3.time,"Penetration in the first centimer, FLAY = 0.01",w=2,color=4)

write(OUT.tsurf[,1,1],"Tsurf_tI133_delta1cm_flay01.tab",ascii,force=1)
write(OUT2.tsurf[,1,1],"Tsurf_tI133_delta1cm_flay005.tab",ascii,force=1)
write(OUT3.tsurf[,1,1],"Tsurf_tI133_delta1cm_flay001.tab",ascii,force=1)
write(OUT.time,"time.tab",ascii,force=1)






OUT = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95,RADGND='T', EFOLD_RADGND = 1e-2)

OUT2 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.01,lbound = -2//95,RADGND='T', EFOLD_RADGND = 1e-2,KEEP='T')


write(OUT.tsurf[,1,1],"Tsurf_tI33_delta1cm_adaptativeFlay.tab",ascii,force=1)

plot(OUT.tsurf[,1,1],xaxis= OUT.time,"Penetration in the first centimer, ADAPTATIVE FLAY = 0.1",w=2,color=2, OUT2.tsurf[,1,1],xaxis= OUT2.time,"Penetration in the first centimer, FLAY = 0.01",w=2,color=3)




























































































































































































































































































OUT_pen009 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.09,lbound = -2//95)
OUT_pen008 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.08,lbound = -2//95)
OUT_pen007 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.07,lbound = -2//95)
OUT_pen006 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.06,lbound = -2//95)
OUT_pen005 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.05,lbound = -2//95)
OUT_pen005 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.05,lbound = -2//95)
OUT_pen004 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.04,lbound = -2//95)

OUT_pen003 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.03,lbound = -2//95)
OUT_pen002 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.02,lbound = -2//95)
OUT_pen001 = krc(lat=0,INERTIA=TI,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.01,lbound = -2//95)

plot(OUT_pen01.tsurf[,1,143],xaxis=OUT_pen01.time,"PENE 01",w=2,color=2, OUT_pen005.tsurf[,1,143],xaxis = OUT_pen005.time,"PENE 05",color= 3,w=2,  OUT_pen003.tsurf[,1,143],xaxis = OUT_pen003.time,"PENE 03",color= 5,w=2,OUT_pen002.tsurf[,1,143],xaxis = OUT_pen003.time,"PENE 02",color= 6,w=2,OUT_pen001.tsurf[,1,143],xaxis = OUT_pen001.time,"PENE 01",color= 7,w=2)


array = [4e-2/0.1265676, 6e-2/1265676,5e-2/0.1265676,2e-2/0.1265676, 3e-2/0.1265676,1e-2/0.1265676,1e-2/0.2098906,2e-2/0.2098906,4e-2/0.2098906,5e-3/0.2098906,1e-2/0.3140444,4e-2/0.3140444,2e-2/0.07449075,5e-3/0.07449075]


flaygood = [0.05, 0.1, 0.1, 0.03, 0.03, 0.03, 0.02,0.05,0.05,0.01,0.01,0.03,0.1,0.03]
write(OUT_pen01.tsurf[,1,143],"KRC_solarpen_eps=1_rlay=01_ti133.ascii",ascii,force=1)
write(OUT_pen005.tsurf[,1,143],"KRC_solarpen_eps=1_rlay=005_ti133.ascii",ascii,force=1)
write(OUT_pen003.tsurf[,1,143],"KRC_solarpen_eps=1_rlay=003_ti133.ascii",ascii,force=1)
write(OUT_pen001.tsurf[,1,143],"KRC_solarpen_eps=1_rlay=001_ti133.ascii",ascii,force=1)


## Comparison with bruce


OUT_nopen = krc(lat=0,INERTIA=33.76,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,lbound = -2//95)
OUT_pen1cm = krc(lat=0,INERTIA=33.76,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.01,lbound = -2//95)
OUT_pen2cm = krc(lat=0,INERTIA=33.76,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.01, lbound = -2//95)
OUT_pen4cm = krc(lat=0,INERTIA=33.76,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.01,lbound = -2//95)


write(OUT_nopen.tsurf[,1,143],"KRC_solarpen_eps=0.ascii",ascii,force=1)
write(OUT_pen1cm.tsurf[,1,143],"KRC_solarpen_eps=1cm.ascii",ascii,force=1)
write(OUT_pen2cm.tsurf[,1,143],"KRC_solarpen_eps=2cm.ascii",ascii,force=1)
write(OUT_pen4cm.tsurf[,1,143],"KRC_solarpen_eps=4cm.ascii",ascii,force=1)
write(OUT_nopen.tsurf[,1,143],"KRC_solarpen_eps=0.ascii",ascii,force=1)
write(OUT_pen1cm.time,"KRC_solarpen_time.ascii",ascii,force=1)




plot(OUT_nopen.tsurf[,1,143],xaxis = OUT_nopen.time,"No penetration in the ground",color= 1,w=2,OUT_pen1cm.tsurf[,1,143],xaxis=OUT_pen1cm.time,"e-folding = 1cm",w=2,color=2,OUT_pen2cm.tsurf[,1,143],xaxis=OUT_pen2cm.time,"e-folding = 2cm",w=2,color=3,OUT_pen4cm.tsurf[,1,143],xaxis=OUT_pen4cm.time,"e-folding = 4cm",w=2,color=4)
plot(OUT_nopen.tsurf[,1,1],xaxis = OUT_nopen.time,"No penetration in the ground",color= 1,w=2,OUT_pen1cm.tsurf[,1,1],xaxis=OUT_pen1cm.time,"e-folding = 1cm",w=2,color=2)


plot(OUT_pen4cm.tsurf[,1,1],xaxis = OUT_pen4cm.time)\




ESSAIE EN METTANT LE HEAT FLOW A 0 ON VERRA CE QUE CA DONNE