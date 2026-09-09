OUT_pen_constant = krc(lat=0,INERTIA=33.76,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,RADGND='T', EFOLD_RADGND = 2e-2,lbound = -2/80)
OUT_pen_cosi = krc(lat=0,INERTIA=33.76,DENSITY=150,SPEC_HEAT = 1000, body="Europa",ALBEDO=.72,EMISS=0.95,LKofT="F",FLAY = 0.1,RADGND='T', EFOLD_RADGND = 2e-2,lbound = -2/80)

write(OUT_pen_constant.tsurf[,1,1],"Tsurf_constantscaleabs.tab",ascii,force=1)
write(OUT_pen_cosi.tsurf[,1,1],"Tsurf_cosiscaleabs.tab",ascii,force=1)
write(OUT_pen_cosi.time,"Time.tab",ascii,force=1)

