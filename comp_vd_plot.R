# -----------------------------------------------------------------------------
# PLOTTING
#
# Evaluate simulated Vd with observed Vd
# shsun 13/2/2019
# shsun 14/2/add wu18 to deciduous
# shsun 3/17/2019 evaluation with ra, rb, rs
# -----------------------------------------------------------------------------
setwd(dir = '~/Dropbox/Ranalysis/')
# load functions
source('functionssh.R')
library(ggpubr)
# load obsevations
load('obsVdHrly.RData')
load('obsVdMonth.RData')
# load simulations, read in readSilvaMod.R
# load('silva_mod_decid.RData')
# load('silva_mod_conif.RData')
# load('silva_mod_grass.RData')
# load('silva_mod_crop.RData')
# load('silva_mod_rainf.RData')

# load simulated results, read in readMod.R
load('simAllAvgHrly.RData')

load('bordenf_obsmod.RData')
#load('blodgett_312.RData')

# -----------------------------------------------------------------------------
# Plots
#
# Data sources: 
# 1. Sam Silva's Dataset: hourly/daily/monthly vd average as seasonal average
# 2. Harvard Forest Data: hourly o3 flux and o3 conc
# 3. Blodgett Forest Data: hourly vd
# 4. Hyytiala Forest Data: hourly vd
# 5. Borden Forest Data: hourly vd
#
#
# ----------------------------------------------------------------------------------------------------------------------------------------------------------
#
# sesonal average vd scatter plots 
#
# ----------------------------------------------------------------------------------------------------------------------------------------------------------

# all PFTs use same plotting method, loading different sessions accordingly
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = F, title.w98, bootstrap = T) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = F, title.w98fbb, bootstrap = T) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = F, title.w98med, bootstrap = T) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = F, title.zh, bootstrap = T) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = F, title.zhfbb, bootstrap = T) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = F, title.zhmed, bootstrap = T) 
# plot combined plots 
#quartz()
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")

# daytime -----------------------------------
# deciduous forest
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = T, title.w98, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16)
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = T, title.w98fbb, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = T, title.w98med, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = T, title.zh, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = T, title.zhfbb, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = T, title.zhmed, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# coniferous forest
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = T, title.w98, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = T, title.w98fbb, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = T, title.w98med, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = T, title.zh, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = T, title.zhfbb, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = T, title.zhmed, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# grass
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = T, title.w98, bootstrap = F, errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 10:16) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = T, title.w98fbb, bootstrap = F,errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 10:16) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = T, title.w98med, bootstrap = F, errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 10:16) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = T, title.zh, bootstrap = F, errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 10:16) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = T, title.zhfbb, bootstrap = F,errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 10:16) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = T, title.zhmed, bootstrap = F,errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 10:16) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# crops
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = T, title.w98, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = T, title.w98fbb, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = T, title.w98med, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = T, title.zh, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = T, title.zhfbb, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = T, title.zhmed, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 10:16) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# rainforest
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = T, title.w98, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = T, title.w98fbb, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = T, title.w98med, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = T, title.zh, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = T, title.zhfbb, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = T, title.zhmed, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")

# 24hr ------------------
# deciduous forest
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = F, title.w98, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = F, title.w98fbb, bootstrap = T,errorbar = F, point_size = 6, text_size = 5) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = F, title.w98med, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = F, title.zh, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = F, title.zhfbb, bootstrap = T,errorbar = F, point_size = 6, text_size = 5) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = F, title.zhmed, bootstrap = T,errorbar = F, point_size = 6, text_size = 5) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# coniferous forest
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = F, title.w98, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = F, title.w98fbb, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = F, title.w98med, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = F, title.zh, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = F, title.zhfbb, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = F, title.zhmed, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# grass
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = F, title.w98, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = F, title.w98fbb, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = F, title.w98med, bootstrap = T,  errorbar = F, point_size = 6, text_size = 5) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = F, title.zh, bootstrap = T,  errorbar = F, point_size = 6, text_size = 5) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = F, title.zhfbb, bootstrap = T,errorbar = F, point_size = 6, text_size = 5) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = F, title.zhmed, bootstrap = T,errorbar = F, point_size = 6, text_size = 5) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# crops
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = F, title.w98, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = F, title.w98fbb, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = F, title.w98med, bootstrap = T,  errorbar = F, point_size = 6, text_size = 5) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = F, title.zh, bootstrap = T,  errorbar = F, point_size = 6, text_size = 5) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = F, title.zhfbb, bootstrap = T,errorbar = F, point_size = 6, text_size = 5) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = F, title.zhmed, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# rainforest
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, daytime = F, title.w98, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, daytime = F, title.w98fbb, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, daytime = F, title.w98med, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, daytime = F, title.zh, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, daytime = F, title.zhfbb, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, daytime = F, title.zhmed, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")


# nighttime ---------------------
# deciduous forest
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, night = T, title = title.w98, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, night = T, title = title.w98fbb, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, night = T, title = title.w98med, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, night = T, title = title.zh, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, night = T, title = title.zhfbb, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, night = T, title = title.zhmed, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# coniferous forest
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, night = T, title = title.w98, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, night = T, title = title.w98fbb, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, night = T, title = title.w98med, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, night = T, title = title.zh, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, night = T, title = title.zhfbb, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, night = T, title = title.zhmed, bootstrap = T, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# grass
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, night = T, title = title.w98, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, night = T, title = title.w98fbb, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, night = T, title = title.w98med, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, night = T, title = title.zh, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, night = T, title = title.zhfbb, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, night = T, title = title.zhmed, bootstrap = T, errorbar = F, point_size = 6, text_size = 5) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# crops
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, night = T, title = title.w98, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, night = T, title = title.w98fbb, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, night = T, title = title.w98med, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, night = T, title = title.zh, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, night = T, title = title.zhfbb, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, night = T, title = title.zhmed, bootstrap = T, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# rainforest
p1 = plotModObs(seasonl, modlist.w98, obslist, avg = T, night = T, title = title.w98, bootstrap = T, errorbar = F, point_size = 5, text_size = 5) 
p2 = plotModObs(seasonl, modlist.w98fbb, obslist, avg = T, night = T, title = title.w98fbb, bootstrap = T, errorbar = F, point_size = 5, text_size = 5) 
p3 = plotModObs(seasonl, modlist.w98med, obslist, avg = T, night = T, title = title.w98med, bootstrap = T, errorbar = F, point_size = 5, text_size = 5) 
p4 = plotModObs(seasonl, modlist.zh, obslist, avg = T, night = T, title = title.zh, bootstrap = T, errorbar = F,point_size = 5, text_size = 5) 
p5 = plotModObs(seasonl, modlist.zhfbb, obslist, avg = T, night = T, title = title.zhfbb, bootstrap = T,errorbar = F, point_size = 5, text_size = 5) 
p6 = plotModObs(seasonl, modlist.zhmed, obslist, avg = T, night = T, title = title.zhmed, bootstrap = T, errorbar = F, point_size = 5, text_size = 5) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")




# ----------------------------------------------------------------------------------------------------------------------------------------------------------
#
# average vd diurnal cycle 
#
# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# diurnal cycle: envelope plots
# short-term observations + long-term observations
# -----------------------------------------------------------------------------
# deciduous 
# JJA
obslist = list(obs.hf.summer$o3vd, wu.june.2011.decid$o3vd, munger.decid.summer.hrly$o3vd, fink.2000.decid$o3vd, padro.summer.1992.decid$o3vd, wu.summer.2018.decid$o3vd)
# padro.spring.1992.decid$o3vd removed 
modlist.w98 = list(hf9901.summer.hrly$vd.w98, wu2011.summer.hrly$vd.w98,munger96.summer.hrly$vd.w98,fink00.summer.hrly$vd.w98,padro91.summer.hrly$vd.w98, bf.summer.hrly$vd.w98)
modlist.w98fbb = list(hf9901.summer.hrly$vd.w98fbb, wu2011.summer.hrly$vd.w98fbb,munger96.summer.hrly$vd.w98fbb, fink00.summer.hrly$vd.w98fbb,padro91.summer.hrly$vd.w98fbb, bf.summer.hrly$vd.w98fbb)
modlist.w98med = list(hf9901.summer.hrly$vd.w98med, wu2011.summer.hrly$vd.w98med,munger96.summer.hrly$vd.w98med, fink00.summer.hrly$vd.w98med,padro91.summer.hrly$vd.w98med, bf.summer.hrly$vd.w98med)
modlist.zh = list(hf9901.summer.hrly$vd.zh, wu2011.summer.hrly$vd.zh,munger96.summer.hrly$vd.zh, fink00.summer.hrly$vd.zh,padro91.summer.hrly$vd.zh, bf.summer.hrly$vd.zh)
modlist.zhfbb = list(hf9901.summer.hrly$vd.zhfbb, wu2011.summer.hrly$vd.zhfbb,munger96.summer.hrly$vd.zhfbb, fink00.summer.hrly$vd.zhfbb,padro91.summer.hrly$vd.zhfbb, bf.summer.hrly$vd.zhfbb)
modlist.zhmed = list(hf9901.summer.hrly$vd.zhmed, wu2011.summer.hrly$vd.zhmed,munger96.summer.hrly$vd.zhmed, fink00.summer.hrly$vd.zhmed,padro91.summer.hrly$vd.zhmed, bf.summer.hrly$vd.zhmed)
# DJF
obslist = list(munger.decid.winter.hrly$o3vd, padro.winter.1992.decid$o3vd, wu.winter.2018.decid$o3vd)
# padro.spring.1992.decid$o3vd removed 
modlist.w98 = list(munger96.winter.hrly$vd.w98, padro91.winter.hrly$vd.w98, bf.winter.hrly$vd.w98)
modlist.w98fbb = list(munger96.winter.hrly$vd.w98fbb, padro91.winter.hrly$vd.w98fbb, bf.winter.hrly$vd.w98fbb)
modlist.w98med = list(munger96.winter.hrly$vd.w98med, padro91.winter.hrly$vd.w98med, bf.winter.hrly$vd.w98med)
modlist.zh = list(munger96.winter.hrly$vd.zh, padro91.winter.hrly$vd.zh, bf.winter.hrly$vd.zh)
modlist.zhfbb = list(munger96.winter.hrly$vd.zhfbb, padro91.winter.hrly$vd.zhfbb, bf.winter.hrly$vd.zhfbb)
modlist.zhmed = list(munger96.winter.hrly$vd.zhmed, padro91.winter.hrly$vd.zhmed, bf.winter.hrly$vd.zhmed)
# plot combined plots 
# envelope plots ------------
p1 = plotEnvTimeSeri(modlist.w98, obslist, title.w98, ylim = 1)
p2 = plotEnvTimeSeri(modlist.w98fbb, obslist, title.w98fbb, ylim = 1)
p3 = plotEnvTimeSeri(modlist.w98med, obslist, title.w98med, ylim = 1)
p4 = plotEnvTimeSeri(modlist.zh, obslist, title.zh, ylim = 1)
p5 = plotEnvTimeSeri(modlist.zhfbb, obslist, title.zhfbb, ylim = 1)
p6 = plotEnvTimeSeri(modlist.zhmed, obslist, title.zhmed, ylim = 1)
#quartz()
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")

# -----------------------------------------------------------------------------
# diurnal cycle: error bar plots
# short-term observations + long-term observations
# using bootstrapping average
# -----------------------------------------------------------------------------
# deciduous 
# summer
obslist = list(obs.hf.summer$o3vd, wu.june.2011.decid$o3vd, munger.decid.summer.hrly$o3vd, fink.2000.decid$o3vd, padro.summer.1992.decid$o3vd,  wu.summer.2018.decid$o3vd)
modlist = list(hf9901.summer.hrly, wu2011.summer.hrly, munger96.summer.hrly, fink00.summer.hrly,padro91.summer.hrly, bf.summer.hrly)
p1 = plotTimeSeri(obs = obslist, mod = modlist, title = "JJA")
#spring
obslist = list(obs.hf.spring$o3vd)
modlist = list(hf9901.spring.hrly$vd)
p2 = plotTimeSeri(obs = obslist, mod = modlist, title = "MAM")
#fall
obslist = list(obs.hf.fall$o3vd, wu.sept.2011.decid$o3vd)
modlist = list(hf9901.fall.hrly$vd, wu2011.fall.hrly$vd)
p3 = plotTimeSeri(obs = obslist, mod = modlist, title = "SON")
#winter
obslist = list(munger.decid.winter.hrly$o3vd, padro.winter.1992.decid$o3vd, wu.winter.2018.decid$o3vd)
modlist = list(munger96.winter.hrly, padro91.winter.hrly, bf.winter.hrly)
p4 = plotTimeSeri(obs = obslist, mod = modlist, title = "DJF", ylim_high = 0.6)
ggarrange(p1, p4, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")
# ---------------------------
# coniferous
obslist = list(mikk.summer.2004.conif$o3vd, mikk.spring.2004.conif$o3vd, mikk.fall.2004.conif$o3vd, mikk.winter.2004.conif$o3vd, 
               kurp.summer.2002.conif$o3vd, kurp.spring.2002.conif$o3vd, kurp.fall.2002.conif$o3vd, kurp.winter.2002.conif$o3vd, 
               kumar.hrly.2008.conif$o3vd, turn.apr.2009.conif$o3vd, turn.may.1.2009.conif$o3vd, turn.may.2.2009.conif$o3vd,
               mikk.june.2000.conif$o3vd, 0.1*mikk.sep.2000.conif$o3vd, schef96.summer.conif$o3vd, 0.1*coe.spring.conif$o3vd, 
               fares.summer.2010.conif$o3vd, fares.spring.2010.conif$o3vd, fares.fall.2010.conif$o3vd, fares.winter.2010.conif$o3vd)
modlist = list(mikk04.summer.hrly,mikk04.spring.hrly, mikk04.fall.hrly,mikk04.winter.hrly,kurp02.summer.hrly,kurp02.spring.hrly,kurp02.fall.hrly,kurp02.winter.hrly,
                   kumar08.june.hrly, turn09.apr.hrly, turn09.may.1.hrly, turn09.may.2.hrly,mikk00.summer.hrly, mikk00.fall.hrly, schef96.summer.hrly, coe95.spring.hrly,
                   fare10.summer.hrly, fare10.spring.hrly, fare10.fall.hrly, fare10.winter.hrly)

p1 = plotTimeSeri(obs = obslist, mod = modlist, title = "Coniferous", ylim_high = 1)
# ---------------------------
# grass

obslist = list(fowl.2001.summer.grass$o3vd, fowl.2001.spring.grass$o3vd, fowl.2001.fall.grass$o3vd,fowl.2001.winter.grass$o3vd, 
               pio.2000.aug.grass$o3vd, pio.2000.june.grass$o3vd, pio.2000.feb.grass$o3vd, pio.2000.apr.grass$o3vd,
               padro.1994.grass$o3vd, cies.2004.klippen$o3vd)
modlist = list(fowl01.summer.hrly, fowl01.spring.hrly, fowl01.fall.hrly, fowl01.winter.hrly, 
                   pio00.aug.hrly, pio00.june.hrly, pio00.feb.hrly, pio00.apr.hrly,
                   padro94.grass.hrly, cies04klippen.sep.hrly)
p2 = plotTimeSeri(obs = obslist, mod = modlist, title = "Grass", ylim_high = 1)
# ---------------------------
# Crop
obslist = list(padro.1994.crop$o3vd, stella.2011.gringon.crop$o3vd, stella.2011.capesud.crops$o3vd, stella.2011.lamasquere.crop$o3vd,
               meyers.1998.bond.crop$o3vd, meyers.1998.nash.crop$o3vd, coyle.2009.crop$o3vd)
modlist = list(padro94.crop.jul.hrly, stella11LaCape.hrly, stella11Lama.hrly, stella11Grig.hrly, mey98bond.hrly, 
                   mey98nash.hrly, coyle09.hrly)
p3 = plotTimeSeri(obs = obslist, mod = modlist, title = "Crops", ylim_high = 1.2)
# ---------------------------
# rainforest
obslist = list(0.1*fowl.2011.july.rainf$o3vd, 0.1*fowl.2011.apr.rainf$o3vd, rum.wet.2007.rainf$o3vd, rum.dry.2007.rainf$o3vd)
modlist = list(danum.summer.hrly, danum.spring.hrly, rum07.spring.hrly, rum07.fall.hrly)
p4 = plotTimeSeri(obs = obslist, mod = modlist, title = "Rainforest", ylim_high = 1.8, ylim_low = -0.3)


ggarrange(p1, p2, p3, p4,nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")



# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Long-term measurements
#
# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# load data from each long-term site
load('hyytiala_312.RData') # Hyytiala Forest
load('hf9301_312.RData') # Harvard Forest
load('bordenf_obsmod.RData') # Borden Forest, 2008~2013
load('blodgett_312.RData')  # Blodgett Forest, 2007~2010 

# hf9301 =  readFiles(casename = 'hf9301', dirname = 'shsun/', startdate = '19930101', startyr = 1993, endyr = 2001, startmonth = 1, endmonth = 12, utc.adjust = -4)
# wu18 = readFiles(casename = 'wu18', dirname = 'shsun/',startdate = '20080101', startyr = 2008, endyr = 2013, startmonth = 1, endmonth = 12, utc.adjust = -5)
# rannik12 = readFiles(casename = 'rannik12', dirname = 'shsun/', startdate = 20070101, startyr = 2007, endyr = 2010, startmonth = 1, endmonth = 12, utc.adjust = 2)
# fare10 = readFiles(casename = 'fare10', dirname = 'shsun/', startdate = 20010101, startyr = 2001, endyr = 2007, startmonth = 1, endmonth = 12, utc.adjust = -8)
# save(hf9301, wu18, rannik12, fare10, file = 'longterm.RData')

# -----------------------------------------------------------------------------
# monthly time series plot 

#### replace with sam silva's average monthly values
tmp = read.csv('~/Dropbox/DryDepEval/dryDepData/dryDepObs/harvard_samsilva.csv', header = F)
colnames(tmp) = c('month','o3vd')
tmp2 = array(NA, dim = c(24,3))
tmp2[,1] = 0.01*tmp$o3vd
#### replace with wu zy's average monthly vd
tmp = read.csv('~/Dropbox/DryDepEval/Wu2018/hf9301_wu15.csv', header = F)
colnames(tmp) = c('month','o3vd')
tmp$month = round(tmp$month)
tmp$o3vd = 0.01*tmp$o3vd

# daytime 9:00~15:00
# replace with clifton's daytime Vd
test = read.delim('~/Dropbox/DryDepEval/HarvardForest/Re%3a_Question_about_calculating_ozone_EC_flux/hf_mm_9am3pm_vd_26_cms_bootstrap_03202018.txt', sep = ',', header = F)
test = t(test)
colnames(test) = 1990:2000
hf.clf = data.frame(month = 1:12, test)
colnames(hf.clf) = c('month',1990:2000)
hf.clf = melt(hf.clf, id = 'month')
colnames(hf.clf) = c('month','year','o3vd')
test = aggregate(hf.clf$o3vd, by = list(hf.clf$month), mean, na.rm = T)
test.sd = aggregate(hf.clf$o3vd, by = list(hf.clf$month), sd, na.rm = T)
colnames(test) = c('month','o3vd')
colnames(test.sd) = c('month','sd')
test = merge(test, test.sd, by = c('month'))
hf.btstrp.day.avgmly[1,1] = test$o3vd[1]/100; hf.btstrp.day.avgmly[1,2] = test$sd[2]/100
hf.btstrp.day.avgmly[2,1] = test$o3vd[2]/100; hf.btstrp.day.avgmly[2,2] = test$sd[2]/100


p1 = plotBootMonthlyComp(modmonthly = hf9301.btstrp.monthly, obsmonthly = tmp, ylim = 0.0105, title = "",boot = F) #Harvard Forest
p2 = plotBootMonthlyComp(modmonthly = rannik12.btstrp.monthly, obsmonthly = hyy.btstrp.avgmonthly, ylim = 0.0105,title = "") #Hyytiala Forest
p3 = plotBootMonthlyComp(modmonthly = wu18.btstrp.monthly, obsmonthly = 0.01*bf.btstrp.avgmonthly,ylim = 0.0105, title = "") #Borden Forest
p4 = plotBootMonthlyComp(modmonthly = fare10.btstrp.monthly ,obsmonthly = blo.btstrp.avgmonthly, ylim = 0.0105,title = "") #Blodgett Forest
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# ===================================================================================================================================================
## daytime monthly
p1 = plotBootMonthlyComp(modmonthly = hf9301.btstrp.day.monthly, obsmonthly = hf.btstrp.day.avgmly, ylim = 0.012, title = "", boot = F) #Harvard Forest
p2 = plotBootMonthlyComp(modmonthly = rannik12.btstrp.day.monthly, obsmonthly = hyy.btstrp.daytime.avgmonthly, ylim = 0.0105,title = "", boot = F) #Hyytiala Forest
p3 = plotBootMonthlyComp(modmonthly = wu18.btstrp.day.monthly, obsmonthly = 0.01*bf.btstrp.day.avgmonthly,ylim = 0.0105, title = "", boot = F) #Borden Forest
p4 = plotBootMonthlyComp(modmonthly = fare10.btstrp.day.monthly ,obsmonthly = blo.btstrp.day.avgmonthly, ylim = 0.018,title = "", boot = F) #Blodgett Forest
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")
# ===================================================================================================================================================

#
max(abs(hf9301.btstrp.day.monthly$zhmed[,1] - mly[,1]))
max(abs(rannik12.btstrp.day.monthly$zhfbb[,1] - hyy.btstrp.daytime.avgmonthly[,1]))
max(abs(wu18.btstrp.day.monthly$w98[,1]- 0.01*bf.btstrp.day.avgmonthly[,1]))


# -----------------------------------------------------------------------------
# daily scatter plot 

# load data


scat_modobs_vd(hf.dly.vd, hf9301.dly, title = "") # Harvard Forest
scat_modobs_vd(hyy.dly.vd, rannik12.dly, title = "") # Hyytiala Forst
scat_modobs_vd(bf.vd, bf.mod.daily.bdt, title = "") # Borden Forest
scat_modobs_vd(blo.dly.vd, blo.dly, text_ylim = 0.3, title = "") #Blodgett Forest

# JJA 
scat_modobs_vd(hf.dly.vd, hf9301.dly, to_comp_month = 6:8, title = "", abline = F) # Harvard Forest
scat_modobs_vd(hyy.dly.vd, rannik12.dly, to_comp_month = 6:8, title = "", abline = F) # Hyytiala Forst
scat_modobs_vd(bf.vd, bf.mod.daily.bdt, to_comp_month = 6:8, title = "", abline = F) # Borden Forest
scat_modobs_vd(blo.dly.vd, blo.dly, text_ylim = 0.3, to_comp_month = 6:8, title = "", abline = F) #Blodgett Forest

# daytime 6am - 6pm
scat_modobs_vd(hyy.day.dly.vd, rannik12.day.dly, title = "Hyytiala Forst")
scat_modobs_vd(hf.day.dly.vd, hf9301.day.dly, title = "Harvard Forest")
scat_modobs_vd(bf.day.vd, bf.mod.day.daily.bdt, title = "Borden Forest")
scat_modobs_vd(blo.day.dly.vd, blo.day.dly, text_x = 0.2, text_ylim = 0.5, title_x = 0.2, title = "Blodgett Forest")

# daytime 6am - 6pm  JJA
scat_modobs_vd(hyy.day.dly.vd, rannik12.day.dly, to_comp_month = 6:8, title = "") # Hyytiala Forst
scat_modobs_vd(hf.day.dly.vd, hf9301.day.dly, to_comp_month = 6:8, title = "") # Harvard Forest
scat_modobs_vd(bf.day.vd, bf.mod.day.daily.bdt,to_comp_month = 6:8, title = "") # Borden Forest
scat_modobs_vd(blo.day.dly.vd, blo.day.dly, to_comp_month = 6:8, text_x = 0.2, text_ylim = 0.5, title_x = 0.2, title = "") # Blodgett Forest


# annotate_figure(figure,
#                 top = text_grob("Visualizing Tooth Growth", color = "red", face = "bold", size = 14),
#                 bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue",
#                                    hjust = 1, x = 1, face = "italic", size = 10),
#                 left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
#                 right = "I'm done, thanks :-)!",
#                 fig.lab = "Figure 1", fig.lab.face = "bold"
# )



# -----------------------------------------------------------------------------
# average diurnal
# -----------------------------------------------------------------------------
# plot average diurnal cycles with hourly observations
# use bootstrapping average hourly? 
# standard deviation
# input hourly data for each season

# mean + sd for plot 
# btstrp 95% CI for plot!

# mod avg vd,gs,ra,rb,gc,gs/gc
rannik12.jja.avghrly = f_mod_mean_sd(casename = rannik12.tmp, pft = 3, to_avg_month = c(6:8), trim = 0.1)
wu18.jjas.avghrly = f_mod_mean_sd(casename = wu18, pft = 8, to_avg_month = 6:9, trim = 0.1)
hf9301.jja.avghrly = f_mod_mean_sd(casename = hf9301, pft = 8, to_avg_month = 6:8, trim = 0.1)
fare10.jja.avghrly = f_mod_mean_sd(casename = fare10, pft = 2, to_avg_day = 6:8, trim = 0.1)

## test wrf w98
# hf9301.wrfw98.avghrly = f_mod_mean_sd(casename = hf9301wrfw98, pft = 8, to_avg_month = 6:8, trim = 0.1, caselist = 'w98')
# hf9301.jja.avghrly$vd.w98 = hf9301.wrfw98.avghrly$vd.w98
wu18.wrfw98.avghrly = f_mod_mean_sd(casename = wu18wrfw98, pft = 8, to_avg_month = 6:9, trim = 0.1, caselist = 'w98')
wu18.jjas.avghrly$vd.w98 = wu18.wrfw98.avghrly$vd.w98


# monthly
#hf.btstrp.avgmly[1:2,1] = hf.btstrp.avgmly[12,1]
rannik12.mean.mly = f_mod_mean_sd(casename = rannik12.tmp, pft = 3, monthly = T, obsVdMonthly = hyy.btstrp.avgmonthly[,1], obsRsMonthly = rannik12.rs.mly, trim = 0.1)
wu18.mean.mly = f_mod_mean_sd(casename = wu18, pft = 8, monthly = T, obsVdMonthly = bf.btstrp.avgmonthly[,1]*0.01, obsRsMonthly = bf08.rs.mly$x, trim = 0.1)
hf9301.mean.mly = f_mod_mean_sd(casename = hf9301, pft = 8, monthly = T, obsVdMonthly = hf.btstrp.avgmly[,1], obsRsMonthly = hf9301.rs.mly, trim = 0.1)
fare10.mean.mly = f_mod_mean_sd(casename = fare10, pft = 2, monthly = T, obsVdMonthly = blo.btstrp.avgmonthly[,1], obsRsMonthly = rannik12.rs.mly, trim = 0.1)



rannik12.mean.day.mly = f_mod_mean_sd(casename = rannik12.tmp, pft = 3, monthly = T, to_avg_hour = 10:16, obsVdMonthly = hyy.btstrp.daytime.avgmonthly[,1], obsRsMonthly = rannik12.rs.day.mly, trim = 0.1)
wu18.mean.day.mly = f_mod_mean_sd(casename = wu18, pft = 8, monthly = T, to_avg_hour = 10:16, obsVdMonthly = bf.btstrp.day.avgmonthly[,1]*0.01, obsRsMonthly = bf08.rs.day.mly$x, trim = 0.1)
hf9301.mean.day.mly = f_mod_mean_sd(casename = hf9301, pft = 8, monthly = T, to_avg_hour = 10:16, obsVdMonthly = hf.btstrp.day.avgmly[,1], obsRsMonthly = hf9301.rs.day.mly, trim = 0.1)
fare10.mean.day.mly = f_mod_mean_sd(casename = fare10, pft = 2, monthly = T, to_avg_hour = 10:16, obsVdMonthly = blo.btstrp.day.avgmonthly[,1], obsRsMonthly = fare10.rs.day.mly, trim = 0.1)

# ----------------------------
# p2 = plot_diurnal_obsmod(obs = hyy.summer.avghrly, mod = rannik12.jja.avghrly, ylim_high = 0.015, title = "") #Hyytiala JJA
# p3 = plot_diurnal_obsmod(obs = 0.01*bf.summer.avghrly, mod = wu18.jjas.avghrly, ylim_high = 0.015, title = "") #Borden JJAS
# p1 = plot_diurnal_obsmod(obs = hf.summer.avghrly, mod = hf9301.jja.avghrly, ylim_high = 0.015, title = "") #Harvard JJA
# p4 = plot_diurnal_obsmod(obs = blo.summer.avghrly, mod = fare10.jja.avghrly, ylim_high = 0.015, title = "") #Blodgett JJA
# ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# ===================================================================================================================================================
# diurnal vdo3 
p1 = plot_diurnal_obsmod(obs = hf.btstrp.summer.avghrly, mod = hf9301.jja.avghrly, ylim_high = 0.015, ylim_low = -0.002,title = "") #Harvard JJA
p2 = plot_diurnal_obsmod(obs = hyy.btstrp.summer.avghrly, mod = rannik12.jja.avghrly, ylim_high = 0.015, title = "") #Hyytiala JJA
p3 = plot_diurnal_obsmod(obs = 0.01*bf.summer.avghrly, mod = wu18.jjas.avghrly, ylim_high = 0.015, title = "") #Borden JJAS
p4 = plot_diurnal_obsmod(obs = blo.btstrp.summer.avghrly, mod = fare10.jja.avghrly, ylim_high = 0.018, title = "") #Blodgett JJA
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")
# ===================================================================================================================================================


p1 = f_plot_ra(to_plot_var = 'ra.w98', yaxis_name = bquote(R[a]~ s ~m^-1))
p2 = f_plot_ra(to_plot_var = 'rb.w98', yaxis_name = bquote(R[b]~ s ~m^-1))
#p1 = f_plot_ra(to_plot_var = 'rs.w98', yaxis_name = 'Gs s^m-1')
ggarrange(p1, p2,nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")

p1 = f_plot_frac(to_plot_var = 'ra.zh', to_plot_var2 = 'gc.zhfbb',yaxis_name = 'Ra/Rc')
p2 = f_plot_frac(to_plot_var = 'ra.zh', to_plot_var2 = 'gc.zhfbb',yaxis_name = 'Ra/Rc', monthly = T, hf = hf9301.mean.day.mly, blo = fare10.mean.day.mly, bf = wu18.mean.day.mly, hyy = rannik12.mean.day.mly)
ggarrange(p1, p2,nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")

p1 = f_plot_ra(to_plot_var = 'ra.w98', yaxis_name = bquote(R[a]~ s ~m^-1), hf = hf9301.mean.mly, blo = fare10.mean.mly, bf = wu18.mean.mly, hyy = rannik12.mean.mly, monthly = T)
p2 = f_plot_ra(to_plot_var = 'rb.w98', yaxis_name = bquote(R[b]~ s ~m^-1), hf = hf9301.mean.mly, blo = fare10.mean.mly, bf = wu18.mean.mly, hyy = rannik12.mean.mly, monthly = T)
ggarrange(p1, p2,nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")

p1 = f_plot_ra(to_plot_var = 'ra.w98', yaxis_name = bquote(R[a]~ s ~m^-1), hf = hf9301.mean.day.mly, blo = fare10.mean.day.mly, bf = wu18.mean.day.mly, hyy = rannik12.mean.day.mly, monthly = T)
p2 = f_plot_ra(to_plot_var = 'rb.w98', yaxis_name = bquote(R[b]~ s ~m^-1), hf = hf9301.mean.day.mly, blo = fare10.mean.day.mly, bf = wu18.mean.day.mly, hyy = rannik12.mean.day.mly, monthly = T)
#p1 = f_plot_ra(to_plot_var = 'rs.w98', yaxis_name = 'Gs s^m-1')
ggarrange(p1, p2,nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")

p1 = f_plot_ra(to_plot_var = 'gc.w98', yaxis_name = bquote(G[c]~ m ~s^-1), hf = hf9301.mean.mly, blo = fare10.mean.mly, bf = wu18.mean.mly, hyy = rannik12.mean.mly, monthly = T)
p2 = f_plot_ra(to_plot_var = 'fgs.w98', yaxis_name = 'Gs/Gc', hf = hf9301.mean.mly, blo = fare10.mean.mly, bf = wu18.mean.mly, hyy = rannik12.mean.mly, monthly = T)
ggarrange(p1, p2,nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")

## ra, rb, rs, rns
## daytime monthly vd
p1 = plotBootMonthlyComp(modmonthly = hf9301.btstrp.day.monthly, obsmonthly = hf.btstrp.day.avgmly, ylim = 0.012, title = "") #Harvard Forest
p2 = plotBootMonthlyComp(modmonthly = rannik12.btstrp.day.monthly, obsmonthly = hyy.btstrp.daytime.avgmonthly, ylim = 0.0105,title = "") #Hyytiala Forest
p3 = plotBootMonthlyComp(modmonthly = wu18.btstrp.day.monthly, obsmonthly = 0.01*bf.btstrp.day.avgmonthly,ylim = 0.0105, title = "") #Borden Forest
p4 = plotBootMonthlyComp(modmonthly = fare10.btstrp.day.monthly ,obsmonthly = blo.btstrp.day.avgmonthly, ylim = 0.015,title = "") #Blodgett Forest
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

## daytime monthly gs 
p1 = f_plot_monthly_rs(case = hf9301.mean.day.mly, to_plot_var = 'rs', to_plot_obs = hf9301.mean.day.mly$obsGs[,1],  yaxis_name = bquote(G[s]~ m ~s^-1))
p2 = f_plot_monthly_rs(case = rannik12.mean.day.mly, to_plot_var = 'rs', to_plot_obs = rannik12.mean.day.mly$obsGs[,1],  yaxis_name = bquote(G[s]~ m ~s^-1))
p3 = f_plot_monthly_rs(case = wu18.mean.day.mly, to_plot_var = 'rs', to_plot_obs = wu18.mean.day.mly$obsGs[,1],  yaxis_name = bquote(G[s]~ m ~s^-1))
p4 = f_plot_monthly_rs(case = fare10.mean.day.mly, to_plot_var = 'rs', to_plot_obs = fare10.mean.day.mly$obsGs[,1],  yaxis_name = bquote(G[s]~ m ~s^-1))
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")


# ===================================================================================================================================================
# diurnal cycle gs 
p1 = f_plot_diurnal_rs(case = hf9301.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  hf9301.rs.jja.diurnal, ylim_high = 0.02, obs_sd = hf9301.rs.jja.diurnal.sd)
p2 = f_plot_diurnal_rs(case = rannik12.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  rannik12.rs.jja.diurnal, ylim_high = 0.015, ylim_low = -0.002, obs_sd = rannik12.rs.jja.diurnal.sd)
p3 = f_plot_diurnal_rs(case = wu18.jjas.avghrly, to_plot_var = 'rs',  to_plot_obs =  bf08_jjas_avghrly$rs, ylim_high = 0.015, ylim_low = -0.002, obs_sd = bf08_jjas_avghrly_sd)
p4 = f_plot_diurnal_rs(case = fare10.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  fare10.rs.jja.diurnal, ylim_high = 0.015, obs_sd = fare10.rs.jja.diurnal.sd)
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")
# ===================================================================================================================================================

##  test
p1 = f_plot_diurnal_rs(case = hf9301.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  0.61/hf9301.rs.jja.diurnal, ylim_high = 0.015)
p2 = f_plot_diurnal_rs(case = rannik12.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  0.61/rannik12.rs.jja.diurnal, ylim_high = 0.015)
p3 = f_plot_diurnal_rs(case = wu18.jjas.avghrly, to_plot_var = 'rs',  to_plot_obs =  0.61/bf08_jjas_avghrly$rs, ylim_high = 0.015)
p4 = f_plot_diurnal_rs(case = fare10.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  0.61/fare10.rs.jja.diurnal, ylim_high = 0.015)
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")




# diurnal cycle gs/gc
p1 = f_plot_diurnal_frac(case = hf9301.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  1/hf9301.rs.jja.diurnal, ylim_high = 0.015)
p2 = f_plot_diurnal_frac(case = rannik12.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  1/rannik12.rs.jja.diurnal, ylim_high = 0.015)
p3 = f_plot_diurnal_frac(case = wu18.jjas.avghrly, to_plot_var = 'rs',  to_plot_obs =  1/bf08_jjas_avghrly$rs, ylim_high = 0.015)
p4 = f_plot_diurnal_frac(case = fare10.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  1/fare10.rs.jja.diurnal, ylim_high = 0.015)
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")


# gs/gc
# daytime
p1 = f_plot_monthly_lt(case = hf9301.mean.day.mly, to_plot_var = 'fgs', to_plot_obs = hf9301.mean.day.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
p2 = f_plot_monthly_lt(case = rannik12.mean.day.mly, to_plot_var = 'fgs', to_plot_obs = rannik12.mean.day.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
p3 = f_plot_monthly_lt(case = wu18.mean.day.mly, to_plot_var = 'fgs', to_plot_obs = wu18.mean.day.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
p4 = f_plot_monthly_lt(case = fare10.mean.day.mly, to_plot_var = 'fgs', to_plot_obs = fare10.mean.day.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# 24hr
p1 = f_plot_monthly_lt(case = hf9301.mean.mly, to_plot_var = 'fgs', to_plot_obs = hf9301.mean.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
p2 = f_plot_monthly_lt(case = rannik12.mean.mly, to_plot_var = 'fgs', to_plot_obs = rannik12.mean.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
p3 = f_plot_monthly_lt(case = wu18.mean.mly, to_plot_var = 'fgs', to_plot_obs = wu18.mean.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
p4 = f_plot_monthly_lt(case = fare10.mean.mly, to_plot_var = 'fgs', to_plot_obs = fare10.mean.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")



#### individual comparison

## mae moh
df = data.frame(hour = 0:23, W89 = 100*matsuda05.spring.hrly$vd.w98, W89FBB = 100*matsuda05.spring.hrly$vd.w98fbb, W89MED = 100*matsuda05.spring.hrly$vd.w98med,
                Z03 = 100*matsuda05.spring.hrly$vd.zh, Z03FBB = 100*matsuda05.spring.hrly$vd.zhfbb, Z03MED = 100*matsuda05.spring.hrly$vd.zhmed)
gg = melt(df, id = 'hour')
colnames(gg) = c('hour','scheme','val')
df.obs = data.frame(hour = 0:23, obs = matsuda.spring.2002.decid$o3vd)

## niwot ridge 
df = data.frame(hour = 0:23, W89 = 100*turn09.may.2.hrly$vd.w98, W89FBB = 100*turn09.may.2.hrly$vd.w98fbb, W89MED = 100*turn09.may.2.hrly$vd.w98med,
                Z03 = 100*turn09.may.2.hrly$vd.zh, Z03FBB = 100*turn09.may.2.hrly$vd.zhfbb, Z03MED = 100*turn09.may.2.hrly$vd.zhmed)
gg = melt(df, id = 'hour')
colnames(gg) = c('hour','scheme','val')
df.obs = data.frame(hour = 0:23, obs = turn.may.2.2009.conif$o3vd)

p <- ggplot()+
  geom_line(data=gg, aes(x = hour, y = val, group = scheme, colour = scheme), size = 1.2)+
  geom_point(data=gg, aes(x = hour, y = val, group = scheme, colour = scheme), size = 1.8)+
  geom_line(data = df.obs, aes(x = hour, y = obs), size = 2)+
  geom_point(data = df.obs, aes(x = hour, y = obs), size = 2)+
  scale_x_continuous(breaks = seq(0,23,by=4))+
  labs(x = "hour", y = bquote(V[d,O3]~ m ~s^-1))+
  scale_colour_brewer(palette = "Set1")+
  scale_y_continuous(limits = c(0,1.2))+
  theme_bw()+
  theme(axis.text.x = element_text(colour = "grey20",size=15),
        axis.text.y = element_text(colour = "grey20",size=15))+
  theme(axis.title = element_text(face = "bold", size = 16))+
  theme(plot.title = element_text(face = "bold", size = 16))+
  theme(legend.text = element_text(size = 15), legend.title = element_blank())
p







# long-term -- function --------------------------------------- ---------------------------------------
f_plot_monthly_rs = function(case, to_plot_var, to_plot_obs,  yaxis_name = bquote(G[s]~ m ~s^-1), ylim_low = 0, ylim_high = 0.01){
  
  caselist = c('w98','zh','fbb','med')
  df = data.frame(month = 1:12, W89 = case[[paste0(to_plot_var,'.w98')]][,1], Z03 = case[[paste0(to_plot_var,'.zh')]][,1], FBB = case[[paste0(to_plot_var,'.zhfbb')]][,1], MED = case[[paste0(to_plot_var,'.zhmed')]][,1])

  gg = melt(df, id = 'month')
  colnames(gg) = c('month','scheme','val')
  
  df.obs = data.frame(month = 1:12, obs = to_plot_obs)
  
  p <- ggplot()+
    geom_line(data=gg, aes(x = month, y = val, group = scheme, colour = scheme), size = 1.2)+
    #geom_point(data=gg, aes(x = month, y = val, group = scheme, colour = scheme), size = 1.8)+
    geom_line(data = df.obs, aes(x = month, y = obs), size = 2)+
    scale_x_continuous(breaks = seq(1,12,by=1))+
    scale_y_continuous(limits = c(ylim_low,ylim_high))+
    labs(x = "Month", y = yaxis_name)+
    scale_colour_brewer(palette = "Set1")+
    theme_bw()+
    theme(axis.text.x = element_text(colour = "grey20",size=15),
          axis.text.y = element_text(colour = "grey20",size=15))+
    theme(axis.title = element_text(face = "bold", size = 16))+
    theme(plot.title = element_text(face = "bold", size = 16))+
    theme(legend.text = element_text(size = 15), legend.title = element_blank())
  return(p)
}

f_plot_diurnal_lt = function(case, to_plot_var, to_plot_obs, ylim_low = 0, ylim_high = 0.012){
  caselist = c('w98','w98fbb','w98med','zh','zhfbb','zhmed')
  df = data.frame(hour = 0:23, W89 = case[[paste0(to_plot_var,'.',caselist[1])]][,1], W89FBB = case[[paste0(to_plot_var,'.',caselist[2])]][,1], W89MED = case[[paste0(to_plot_var,'.',caselist[3])]][,1],
                  Z03 = case[[paste0(to_plot_var,'.',caselist[4])]][,1], Z03FBB = case[[paste0(to_plot_var,'.',caselist[5])]][,1], Z03MED = case[[paste0(to_plot_var,'.',caselist[6])]][,1])
  
  gg = melt(df, id = 'hour')
  colnames(gg) = c('hour','scheme','val')
  
  df.obs = data.frame(hour = 0:23, obs = to_plot_obs)
  
  p <- ggplot()+
    geom_line(data=gg, aes(x = hour, y = val, group = scheme, colour = scheme), size = 1.2)+
    geom_point(data=gg, aes(x = hour, y = val, group = scheme, colour = scheme), size = 1.8)+
    geom_line(data = df.obs, aes(x = hour, y = obs), size = 2)+
    geom_point(data = df.obs, aes(x = hour, y = obs), size = 2)+
    scale_x_continuous(breaks = seq(0,23,by=4))+
    labs(x = "hour", y = yaxis_name)+
    scale_colour_brewer(palette = "Set1")+
    scale_y_continuous(limits = c(ylim_low,ylim_high))+
    theme_bw()+
    theme(axis.text.x = element_text(colour = "grey20",size=15),
          axis.text.y = element_text(colour = "grey20",size=15))+
    theme(axis.title = element_text(face = "bold", size = 16))+
    theme(plot.title = element_text(face = "bold", size = 16))+
    theme(legend.text = element_text(size = 15), legend.title = element_blank())
  return(p)
}

f_plot_diurnal_rs = function(case, to_plot_var = 'rs', to_plot_obs, yaxis_name = bquote(G[s]~ m ~s^-1), ylim_high = 0.01, obs_sd = F, ylim_low = 0){
  caselist = c('w98','zh','fbb','med')
  df = data.frame(hour = 0:23, W89 = case[[paste0(to_plot_var,'.w98')]][,1], 
                  Z03 = case[[paste0(to_plot_var,'.zh')]][,1], FBB = case[[paste0(to_plot_var,'.zhfbb')]][,1], MED = case[[paste0(to_plot_var,'.zhmed')]][,1])
  gg = melt(df, id = 'hour')
  colnames(gg) = c('hour','scheme','val')
  
  df.obs = data.frame(hour = 0:23, obs = to_plot_obs)
  
  p <- ggplot()+
    geom_line(data=gg, aes(x = hour, y = val, group = scheme, colour = scheme), size = 1.2)+
    #geom_point(data=gg, aes(x = hour, y = val, group = scheme, colour = scheme), size = 1.8)+
    geom_line(data = df.obs, aes(x = hour, y = obs), size = 2)+
    #geom_errorbar(data = obs_sd, aes(x = hour, ymin=rs-sd, ymax=rs+sd), width = .5, size = 1.2)+
    geom_ribbon(data = obs_sd, aes(x = hour, ymin=rs-sd, ymax=rs+sd), alpha = 0.3)+
    #geom_point(data = df.obs, aes(x = hour, y = obs), size = 3)+
    scale_x_continuous(breaks = seq(0,23,by=4))+
    scale_y_continuous(limits = c(ylim_low,ylim_high))+
    labs(x = "hour", y = yaxis_name)+
    scale_colour_brewer(palette = "Set1")+
    theme_bw()+
    theme(axis.text.x = element_text(colour = "grey20",size=15),
          axis.text.y = element_text(colour = "grey20",size=15))+
    theme(axis.title = element_text(face = "bold", size = 16))+
    theme(plot.title = element_text(face = "bold", size = 16))+
    theme(legend.text = element_text(size = 15), legend.title = element_blank())
  return(p)
}


f_plot_diurnal_frac = function(case, to_plot_var = 'rs', to_plto_var2 = 'gc', to_plot_obs, yaxis_name = '', ylim_high = 0.01){
  caselist = c('w98','zh','fbb','med')
  df = data.frame(hour = 0:23, W89 = case[[paste0(to_plot_var,'.w98')]][,1]/case[[paste0(to_plot_var2,'.w98')]][,1], W89FBB = case[[paste0(to_plot_var,'.w98fbb')]][,1]/case[[paste0(to_plot_var2,'.w98fbb')]][,1], W89MED = case[[paste0(to_plot_var,'.w98med')]][,1]/case[[paste0(to_plot_var2,'.w98med')]][,1], 
                  Z03 = case[[paste0(to_plot_var,'.zh')]][,1]/case[[paste0(to_plot_var2,'.zh')]][,1], Z03FBB = case[[paste0(to_plot_var,'.zhfbb')]][,1]/case[[paste0(to_plot_var2,'.zhfbb')]][,1], Z03MED = case[[paste0(to_plot_var,'.zhmed')]][,1]/case[[paste0(to_plot_var2,'.zhmed')]][,1])
  gg = melt(df, id = 'hour')
  colnames(gg) = c('hour','scheme','val')
  
  df.obs = data.frame(hour = 0:23, obs = to_plot_obs)
  
  p <- ggplot()+
    geom_line(data=gg, aes(x = hour, y = val, group = scheme, colour = scheme), size = 1.2)+
    geom_point(data=gg, aes(x = hour, y = val, group = scheme, colour = scheme), size = 1.8)+
    geom_line(data = df.obs, aes(x = hour, y = obs), size = 2)+
    geom_point(data = df.obs, aes(x = hour, y = obs), size = 3)+
    scale_x_continuous(breaks = seq(0,23,by=4))+
    scale_y_continuous(limits = c(0,ylim_high))+
    labs(x = "hour", y = yaxis_name)+
    scale_colour_brewer(palette = "Set1")+
    theme_bw()+
    theme(axis.text.x = element_text(colour = "grey20",size=15),
          axis.text.y = element_text(colour = "grey20",size=15))+
    theme(axis.title = element_text(face = "bold", size = 16))+
    theme(plot.title = element_text(face = "bold", size = 16))+
    theme(legend.text = element_text(size = 15), legend.title = element_blank())
  return(p)
}

f_plot_monthly_lt = function(case, to_plot_var, to_plot_obs,  yaxis_name = bquote(G[s]~ m ~s^-1), ylim_low = 0, ylim_high = 1){
  
  caselist = c('w98','w98fbb','w98med','zh','zhfbb','zhmed')
  df = data.frame(month = 1:12, W89 = case[[paste0(to_plot_var,'.',caselist[1])]][,1], W89FBB = case[[paste0(to_plot_var,'.',caselist[2])]][,1], W89MED = case[[paste0(to_plot_var,'.',caselist[3])]][,1],
                  Z03 = case[[paste0(to_plot_var,'.',caselist[4])]][,1], Z03FBB = case[[paste0(to_plot_var,'.',caselist[5])]][,1], Z03MED = case[[paste0(to_plot_var,'.',caselist[6])]][,1])
  
  gg = melt(df, id = 'month')
  colnames(gg) = c('month','scheme','val')
  
  df.obs = data.frame(month = 1:12, obs = to_plot_obs)
  
  p <- ggplot()+
    geom_line(data=gg, aes(x = month, y = val, group = scheme, colour = scheme), size = 1.2)+
    geom_point(data=gg, aes(x = month, y = val, group = scheme, colour = scheme), size = 1.8)+
    geom_line(data = df.obs, aes(x = month, y = obs), size = 2)+
    scale_x_continuous(breaks = seq(1,12,by=1))+
    scale_y_continuous(limits = c(ylim_low,ylim_high))+
    labs(x = "Month", y = yaxis_name)+
    scale_colour_brewer(palette = "Set1")+
    theme_bw()+
    theme(axis.text.x = element_text(colour = "grey20",size=15),
          axis.text.y = element_text(colour = "grey20",size=15))+
    theme(axis.title = element_text(face = "bold", size = 16))+
    theme(plot.title = element_text(face = "bold", size = 16))+
    theme(legend.text = element_text(size = 15), legend.title = element_blank())
  return(p)
}

f_plot_ra = function(to_plot_var, yaxis_name, title = "", hf = hf9301.jja.avghrly, blo = fare10.jja.avghrly, bf = wu18.jjas.avghrly, hyy = rannik12.jja.avghrly, monthly = F){
  if(monthly){
    df = data.frame(cbind(month = 1:12, Harvard = hf[[to_plot_var]][,1], Blodgett = blo[[to_plot_var]][,1], 
                          Borden = bf[[to_plot_var]][,1], Hyytiala = hyy[[to_plot_var]][,1]) )
    gg = melt(df, id = 'month')
    colnames(gg) = c('month','site','val')
    
    p <- ggplot()+
      geom_line(data=gg, aes(x = month, y = val, group = site, colour = site), size = 1.2)+
      geom_point(data=gg, aes(x = month, y = val, group = site, colour = site), size = 1.8)+
      scale_x_continuous(breaks = seq(1,12,by=1))+
      labs(x = "Month", y = yaxis_name)+
      scale_colour_brewer(palette = "Set1")+
      theme_bw()+
      ggtitle(paste0(title))+
      theme(axis.text.x = element_text(colour = "grey20",size=15),
            axis.text.y = element_text(colour = "grey20",size=15))+
      theme(axis.title = element_text(face = "bold", size = 16))+
      theme(plot.title = element_text(face = "bold", size = 16))+
      theme(legend.text = element_text(size = 15), legend.title = element_blank())
  }else{
    df = data.frame(cbind(hour = 0:23, Harvard = hf[[to_plot_var]][,1], Blodgett = blo[[to_plot_var]][,1], 
                          Borden = bf[[to_plot_var]][,1], Hyytiala = hyy[[to_plot_var]][,1]) )
    gg = melt(df, id = 'hour')
    colnames(gg) = c('hour','site','val')
    
    p <- ggplot()+
      geom_line(data=gg, aes(x = hour, y = val, group = site, colour = site), size = 1.2)+
      geom_point(data=gg, aes(x = hour, y = val, group = site, colour = site), size = 1.8)+
      scale_x_continuous(breaks = seq(0,23,by=4))+
      labs(x = "Hour", y = yaxis_name)+
      scale_colour_brewer(palette = "Set1")+
      theme_bw()+
      ggtitle(paste0(title))+
      theme(axis.text.x = element_text(colour = "grey20",size=15),
            axis.text.y = element_text(colour = "grey20",size=15))+
      theme(axis.title = element_text(face = "bold", size = 16))+
      theme(plot.title = element_text(face = "bold", size = 16))+
      theme(legend.text = element_text(size = 15), legend.title = element_blank())
  }
  return(p)
}

f_plot_frac = function(to_plot_var, to_plot_var2, yaxis_name, title = "", hf = hf9301.jja.avghrly, blo = fare10.jja.avghrly, bf = wu18.jjas.avghrly, hyy = rannik12.jja.avghrly, monthly = F){
  if(monthly){
    df = data.frame(cbind(month = 1:12, Harvard = hf[[to_plot_var]][,1]*hf[[to_plot_var2]][,1], Blodgett = blo[[to_plot_var]][,1]*blo[[to_plot_var2]][,1], 
                          Borden = bf[[to_plot_var]][,1]*bf[[to_plot_var2]][,1], Hyytiala = hyy[[to_plot_var]][,1]*hyy[[to_plot_var2]][,1]) )
    gg = melt(df, id = 'month')
    colnames(gg) = c('month','site','val')
    
    p <- ggplot()+
      geom_line(data=gg, aes(x = month, y = val, group = site, colour = site), size = 1.2)+
      geom_point(data=gg, aes(x = month, y = val, group = site, colour = site), size = 1.8)+
      scale_x_continuous(breaks = seq(1,12,by=1))+
      labs(x = "Month", y = yaxis_name)+
      scale_colour_brewer(palette = "Set1")+
      theme_bw()+
      ggtitle(paste0(title))+
      theme(axis.text.x = element_text(colour = "grey20",size=15),
            axis.text.y = element_text(colour = "grey20",size=15))+
      theme(axis.title = element_text(face = "bold", size = 16))+
      theme(plot.title = element_text(face = "bold", size = 16))+
      theme(legend.text = element_text(size = 15), legend.title = element_blank())
  }else{
    df = data.frame(cbind(hour = 0:23, Harvard = hf[[to_plot_var]][,1]*hf[[to_plot_var2]][,1], Blodgett = blo[[to_plot_var]][,1]*blo[[to_plot_var2]][,1], 
                          Borden = bf[[to_plot_var]][,1]*bf[[to_plot_var2]][,1], Hyytiala = hyy[[to_plot_var]][,1]*hyy[[to_plot_var2]][,1]) )
    gg = melt(df, id = 'hour')
    colnames(gg) = c('hour','site','val')
    
    p <- ggplot()+
      geom_line(data=gg, aes(x = hour, y = val, group = site, colour = site), size = 1.2)+
      geom_point(data=gg, aes(x = hour, y = val, group = site, colour = site), size = 1.8)+
      scale_x_continuous(breaks = seq(0,23,by=4))+
      labs(x = "Hour", y = yaxis_name)+
      scale_colour_brewer(palette = "Set1")+
      #scale_y_continuous(limits = c(0,1))+
      theme_bw()+
      ggtitle(paste0(title))+
      theme(axis.text.x = element_text(colour = "grey20",size=15),
            axis.text.y = element_text(colour = "grey20",size=15))+
      theme(axis.title = element_text(face = "bold", size = 16))+
      theme(plot.title = element_text(face = "bold", size = 16))+
      theme(legend.text = element_text(size = 15), legend.title = element_blank())
  }
  return(p)
}









