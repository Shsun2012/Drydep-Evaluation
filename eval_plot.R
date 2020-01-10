# -----------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------
source('~/Dropbox/GitHub/Drydep-Evaluation/functions.R')
load('obsmod_data.RData')
# sesonal average vd scatter plots by PFTs 
# -----plot daytime obs vs mod vd (5 PFTs)  -----------------------------------
title.w98 = "W89";title.w98fbb = "W89FBB";title.w98med = "W89MED";title.zh = "Z03";title.zhfbb = "Z03FBB";title.zhmed = "Z03MED"
# deciduous forest
p1 = plotModObs(decid.seasonl, decid.w98, decid.obslist, avg = T, daytime = T, title.w98, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19)
p2 = plotModObs(decid.seasonl, decid.w98fbb, decid.obslist, avg = T, daytime = T, title.w98fbb, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p3 = plotModObs(decid.seasonl, decid.w98med, decid.obslist, avg = T, daytime = T, title.w98med, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p4 = plotModObs(decid.seasonl, decid.zh, decid.obslist, avg = T, daytime = T, title.zh, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p5 = plotModObs(decid.seasonl, decid.zhfbb, decid.obslist, avg = T, daytime = T, title.zhfbb, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p6 = plotModObs(decid.seasonl, decid.zhmed, decid.obslist, avg = T, daytime = T, title.zhmed, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# coniferous forest
p1 = plotModObs(conif.seasonl, conif.w98, conif.obslist, avg = T, daytime = T, title.w98, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p2 = plotModObs(conif.seasonl, conif.w98fbb, conif.obslist, avg = T, daytime = T, title.w98fbb, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p3 = plotModObs(conif.seasonl, conif.w98med, conif.obslist, avg = T, daytime = T, title.w98med, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p4 = plotModObs(conif.seasonl, conif.zh, conif.obslist, avg = T, daytime = T, title.zh, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p5 = plotModObs(conif.seasonl, conif.zhfbb, conif.obslist, avg = T, daytime = T, title.zhfbb, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p6 = plotModObs(conif.seasonl, conif.zhmed, conif.obslist, avg = T, daytime = T, title.zhmed, bootstrap = F, y_lim = 1.2, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
# grass
p1 = plotModObs(grass.seasonl, grass.w98, grass.obslist, avg = T, daytime = T, title.w98, bootstrap = F, errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 7:19) 
p2 = plotModObs(grass.seasonl, grass.w98fbb, grass.obslist, avg = T, daytime = T, title.w98fbb, bootstrap = F,errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 7:19) 
p3 = plotModObs(grass.seasonl, grass.w98med, grass.obslist, avg = T, daytime = T, title.w98med, bootstrap = F, errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 7:19) 
p4 = plotModObs(grass.seasonl, grass.zh, grass.obslist, avg = T, daytime = T, title.zh, bootstrap = F, errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 7:19) 
p5 = plotModObs(grass.seasonl, grass.zhfbb, grass.obslist, avg = T, daytime = T, title.zhfbb, bootstrap = F,errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 7:19) 
p6 = plotModObs(grass.seasonl, grass.zhmed, grass.obslist, avg = T, daytime = T, title.zhmed, bootstrap = F,errorbar = F, point_size = 6, y_lim = 1.4, text_size = 5, toAvgHour = 7:19) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")

# crops
p1 = plotModObs(crop.seasonl, crop.w98, crop.obslist, avg = T, daytime = T, title.w98, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p2 = plotModObs(crop.seasonl, crop.w98fbb, crop.obslist, avg = T, daytime = T, title.w98fbb, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p3 = plotModObs(crop.seasonl, crop.w98med, crop.obslist, avg = T, daytime = T, title.w98med, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p4 = plotModObs(crop.seasonl, crop.zh, crop.obslist, avg = T, daytime = T, title.zh, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p5 = plotModObs(crop.seasonl, crop.zhfbb, crop.obslist, avg = T, daytime = T, title.zhfbb, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
p6 = plotModObs(crop.seasonl, crop.zhmed, crop.obslist, avg = T, daytime = T, title.zhmed, bootstrap = F, y_lim = 1.4, errorbar = F, point_size = 6, text_size = 5, toAvgHour = 7:19) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")

# rainforest
p1 = plotModObs(rainf.seasonl, rainf.w98, rainf.obslist, avg = T, daytime = T, title.w98, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
p2 = plotModObs(rainf.seasonl, rainf.w98fbb, rainf.obslist, avg = T, daytime = T, title.w98fbb, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
p3 = plotModObs(rainf.seasonl, rainf.w98med, rainf.obslist, avg = T, daytime = T, title.w98med, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
p4 = plotModObs(rainf.seasonl, rainf.zh, rainf.obslist, avg = T, daytime = T, title.zh, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
p5 = plotModObs(rainf.seasonl, rainf.zhfbb, rainf.obslist, avg = T, daytime = T, title.zhfbb, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
p6 = plotModObs(rainf.seasonl, rainf.zhmed, rainf.obslist, avg = T, daytime = T, title.zhmed, bootstrap = F, y_lim = 2.5, axis_break = c(0,0.4,0.8,1.2,1.6,2,2.4), title_x = 0.4, errorbar = F, y_adjust = 0.6, point_size = 5, text_size = 5, toAvgHour = 7:19) 
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")

# ------------------------------------------------------------ average vd diurnal cycle  ----------------------------------------------------------------------------------------------
# diurnal cycle: envelope plots
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
modlist.w98 = list(munger96.winter.hrly$vd.w98, padro91.winter.hrly$vd.w98, bf.winter.hrly$vd.w98)
modlist.w98fbb = list(munger96.winter.hrly$vd.w98fbb, padro91.winter.hrly$vd.w98fbb, bf.winter.hrly$vd.w98fbb)
modlist.w98med = list(munger96.winter.hrly$vd.w98med, padro91.winter.hrly$vd.w98med, bf.winter.hrly$vd.w98med)
modlist.zh = list(munger96.winter.hrly$vd.zh, padro91.winter.hrly$vd.zh, bf.winter.hrly$vd.zh)
modlist.zhfbb = list(munger96.winter.hrly$vd.zhfbb, padro91.winter.hrly$vd.zhfbb, bf.winter.hrly$vd.zhfbb)
modlist.zhmed = list(munger96.winter.hrly$vd.zhmed, padro91.winter.hrly$vd.zhmed, bf.winter.hrly$vd.zhmed)
# envelope plots ------------
p1 = plotEnvTimeSeri(modlist.w98, obslist, title.w98, ylim = 1.6)
p2 = plotEnvTimeSeri(modlist.w98fbb, obslist, title.w98fbb, ylim = 1.6)
p3 = plotEnvTimeSeri(modlist.w98med, obslist, title.w98med, ylim = 1.6)
p4 = plotEnvTimeSeri(modlist.zh, obslist, title.zh, ylim = 1.6)
p5 = plotEnvTimeSeri(modlist.zhfbb, obslist, title.zhfbb, ylim = 1.6)
p6 = plotEnvTimeSeri(modlist.zhmed, obslist, title.zhmed, ylim = 1.6)
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")

# ===================================================================================================================================================
# four long-term sites: harvard forest, borden forest, hyytiala forest, blodgett forest

# daytime monthly vd
p1 = plotBootMonthlyComp(modmonthly = hf9301.btstrp.day.monthly, obsmonthly = hf.btstrp.day.avgmly, ylim = 0.012, title = "", boot = T) #Harvard Forest
p2 = plotBootMonthlyComp(modmonthly = rannik12.btstrp.day.monthly, obsmonthly = hyy.btstrp.daytime.avgmonthly, ylim = 0.0105,title = "", boot = F) #Hyytiala Forest
p3 = plotBootMonthlyComp(modmonthly = wu18.btstrp.day.monthly, obsmonthly = 0.01*bf.btstrp.day.avgmonthly,ylim = 0.0105, title = "", boot = T) #Borden Forest
p4 = plotBootMonthlyComp(modmonthly = fare10.btstrp.day.monthly ,obsmonthly = blo.btstrp.day.avgmonthly, ylim = 0.018,title = "", boot = F) #Blodgett Forest
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# diurnal vdo3 
p1 = plot_diurnal_obsmod(obs = hf.btstrp.summer.avghrly, mod = hf9301.jja.avghrly, ylim_high = 0.015, ylim_low = -0.002,title = "") #Harvard JJA
p2 = plot_diurnal_obsmod(obs = hyy.btstrp.summer.avghrly, mod = rannik12.jja.avghrly, ylim_high = 0.015, title = "") #Hyytiala JJA
p3 = plot_diurnal_obsmod(obs = 0.01*bf.summer.avghrly, mod = wu18.jjas.avghrly, ylim_high = 0.015, title = "") #Borden JJAS
p4 = plot_diurnal_obsmod(obs = blo.btstrp.summer.avghrly, mod = fare10.jja.avghrly, ylim_high = 0.018, title = "") #Blodgett JJA
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# daily scatter plot
scat_modobs_vd(hf.dly.vd, hf9301.dly, to_comp_month = 1:12, title = "", abline = F) # Harvard Forest
scat_modobs_vd(hyy.dly.vd, rannik12.dly, to_comp_month = 1:12, title = "", abline = F) # Hyytiala Forst
scat_modobs_vd(bf.vd, bf.mod.daily.bdt, to_comp_month = 1:12, title = "", abline = F) # Borden Forest
scat_modobs_vd(blo.dly.vd, blo.dly, text_ylim = 0.3, to_comp_month = 1:12, title = "", abline = F) #Blodgett Forest

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

# diurnal cycle gs 
p1 = f_plot_diurnal_rs(case = hf9301.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  hf9301.rs.jja.diurnal, ylim_high = 0.02, obs_sd = hf9301.rs.jja.diurnal.sd)
p2 = f_plot_diurnal_rs(case = rannik12.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  rannik12.rs.jja.diurnal, ylim_high = 0.015, ylim_low = -0.002, obs_sd = rannik12.rs.jja.diurnal.sd)
p3 = f_plot_diurnal_rs(case = wu18.jjas.avghrly, to_plot_var = 'rs',  to_plot_obs =  bf08_jjas_avghrly$rs, ylim_high = 0.015, ylim_low = -0.002, obs_sd = bf08_jjas_avghrly_sd)
p4 = f_plot_diurnal_rs(case = fare10.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  fare10.rs.jja.diurnal, ylim_high = 0.015, obs_sd = fare10.rs.jja.diurnal.sd)
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# diurnal cycle gs/gc
p1 = f_plot_diurnal_frac(case = hf9301.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  1/hf9301.rs.jja.diurnal, ylim_high =1)
p2 = f_plot_diurnal_frac(case = rannik12.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  1/rannik12.rs.jja.diurnal, ylim_high = 1)
p3 = f_plot_diurnal_frac(case = wu18.jjas.avghrly, to_plot_var = 'rs',  to_plot_obs =  1/bf08_jjas_avghrly$rs, ylim_high = 1)
p4 = f_plot_diurnal_frac(case = fare10.jja.avghrly, to_plot_var = 'rs',  to_plot_obs =  1/fare10.rs.jja.diurnal, ylim_high = 1)
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# daytime gs/gc
p1 = f_plot_monthly_lt(case = hf9301.mean.day.mly, to_plot_var = 'fgs', to_plot_obs = hf9301.mean.day.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
p2 = f_plot_monthly_lt(case = rannik12.mean.day.mly, to_plot_var = 'fgs', to_plot_obs = rannik12.mean.day.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
p3 = f_plot_monthly_lt(case = wu18.mean.day.mly, to_plot_var = 'fgs', to_plot_obs = wu18.mean.day.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
p4 = f_plot_monthly_lt(case = fare10.mean.day.mly, to_plot_var = 'fgs', to_plot_obs = fare10.mean.day.mly$obsfgs[,1],  yaxis_name = bquote(G[s]~ '/' ~G[c]))
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# daytime monthly gs 
p1 = f_plot_monthly_rs(case = hf9301.mean.day.mly, to_plot_var = 'rs', to_plot_obs = hf9301.mean.day.mly$obsGs[,1],  yaxis_name = bquote(G[s]~ m ~s^-1))
p2 = f_plot_monthly_rs(case = rannik12.mean.day.mly, to_plot_var = 'rs', to_plot_obs = rannik12.mean.day.mly$obsGs[,1],  yaxis_name = bquote(G[s]~ m ~s^-1))
p3 = f_plot_monthly_rs(case = wu18.mean.day.mly, to_plot_var = 'rs', to_plot_obs = wu18.mean.day.mly$obsGs[,1],  yaxis_name = bquote(G[s]~ m ~s^-1))
p4 = f_plot_monthly_rs(case = fare10.mean.day.mly, to_plot_var = 'rs', to_plot_obs = fare10.mean.day.mly$obsGs[,1],  yaxis_name = bquote(G[s]~ m ~s^-1))
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# ===================================================================================================================================================
# PFT-average diurnal cycles with error bars

# deciduous 
# summer
obslist = list(obs.hf.summer$o3vd, wu.june.2011.decid$o3vd, munger.decid.summer.hrly$o3vd, fink.2000.decid$o3vd, padro.summer.1992.decid$o3vd,  wu.summer.2018.decid$o3vd)
modlist = list(hf9901.summer.hrly, wu2011.summer.hrly, munger96.summer.hrly, fink00.summer.hrly,padro91.summer.hrly, bf.summer.hrly)
p1 = plotTimeSeri(obs = obslist, mod = modlist, title = "JJA", bootstrap = F)
#spring
obslist = list(obs.hf.spring$o3vd)
modlist = list(hf9901.spring.hrly)
p2 = plotTimeSeri(obs = obslist, mod = modlist, title = "MAM", bootstrap = F)
#fall
obslist = list(obs.hf.fall$o3vd, wu.sept.2011.decid$o3vd)
modlist = list(hf9901.fall.hrly, wu2011.fall.hrly)
p3 = plotTimeSeri(obs = obslist, mod = modlist, title = "SON", bootstrap = F)
#winter
obslist = list(munger.decid.winter.hrly$o3vd, padro.winter.1992.decid$o3vd, wu.winter.2018.decid$o3vd)
modlist = list(munger96.winter.hrly, padro91.winter.hrly, bf.winter.hrly)
p4 = plotTimeSeri(obs = obslist, mod = modlist, title = "DJF", ylim_high = 0.6, bootstrap = F)
ggarrange(p1, p4, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")
# --------------------------- coniferous ---------------------------
obslist = list(mikk.summer.2004.conif$o3vd, mikk.spring.2004.conif$o3vd, mikk.fall.2004.conif$o3vd, mikk.winter.2004.conif$o3vd, 
               kurp.summer.2002.conif$o3vd, kurp.spring.2002.conif$o3vd, kurp.fall.2002.conif$o3vd, kurp.winter.2002.conif$o3vd, 
               turn.apr.2009.conif$o3vd, turn.may.1.2009.conif$o3vd, turn.may.2.2009.conif$o3vd,
               mikk.june.2000.conif$o3vd, 0.1*mikk.sep.2000.conif$o3vd, schef96.summer.conif$o3vd, 0.1*coe.spring.conif$o3vd, 
               fares.summer.2010.conif$o3vd, fares.spring.2010.conif$o3vd, fares.fall.2010.conif$o3vd, fares.winter.2010.conif$o3vd)
modlist = list(mikk04.summer.hrly,mikk04.spring.hrly, mikk04.fall.hrly,mikk04.winter.hrly,kurp02.summer.hrly,kurp02.spring.hrly,kurp02.fall.hrly,kurp02.winter.hrly,
                   turn09.apr.hrly, turn09.may.1.hrly, turn09.may.2.hrly,mikk00.summer.hrly, mikk00.fall.hrly, schef96.summer.hrly, coe95.spring.hrly,
                   fare10.summer.hrly, fare10.spring.hrly, fare10.fall.hrly, fare10.winter.hrly)
p1 = plotTimeSeri(obs = obslist, mod = modlist, title = "Coniferous", ylim_high = 1)
# --------------------------- grass ---------------------------
obslist = list(fowl.2001.summer.grass$o3vd, fowl.2001.spring.grass$o3vd, fowl.2001.fall.grass$o3vd,fowl.2001.winter.grass$o3vd, 
               pio.2000.aug.grass$o3vd, pio.2000.june.grass$o3vd, pio.2000.feb.grass$o3vd, pio.2000.apr.grass$o3vd,
               padro.1994.grass$o3vd, cies.2004.klippen$o3vd)
modlist = list(fowl01.summer.hrly, fowl01.spring.hrly, fowl01.fall.hrly, fowl01.winter.hrly, 
                   pio00.aug.hrly, pio00.june.hrly, pio00.feb.hrly, pio00.apr.hrly,
                   padro94.grass.hrly, cies04klippen.sep.hrly)
p2 = plotTimeSeri(obs = obslist, mod = modlist, title = "Grass", ylim_high = 1)
# --------------------------- crop ---------------------------
obslist = list(padro.1994.crop$o3vd, stella.2011.gringon.crop$o3vd, stella.2011.capesud.crops$o3vd, stella.2011.lamasquere.crop$o3vd,
               meyers.1998.bond.crop$o3vd, meyers.1998.nash.crop$o3vd, coyle.2009.crop$o3vd)
modlist = list(padro94.crop.jul.hrly, stella11LaCape.hrly, stella11Lama.hrly, stella11Grig.hrly, mey98bond.hrly, 
                   mey98nash.hrly, coyle09.hrly)
p3 = plotTimeSeri(obs = obslist, mod = modlist, title = "Crops", ylim_high = 1.2)
# --------------------------- rainforest ---------------------------
obslist = list(0.1*fowl.2011.july.rainf$o3vd, 0.1*fowl.2011.apr.rainf$o3vd, rum.wet.2007.rainf$o3vd, rum.dry.2007.rainf$o3vd)
modlist = list(danum.summer.hrly, danum.spring.hrly, rum07.spring.hrly, rum07.fall.hrly)
p4 = plotTimeSeri(obs = obslist, mod = modlist, title = "Rainforest", ylim_high = 1.8, ylim_low = -0.3)

ggarrange(p1, p2, p3, p4,nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")


# ----------- other site comparison -----------
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





