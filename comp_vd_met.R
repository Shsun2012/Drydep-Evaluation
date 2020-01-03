# -----------------------------------------------------------------------------
# Evaluate simulated Vd with observed Vd
# -----------------------------------------------------------------------------
setwd(dir = '~/Dropbox/Ranalysis/')
# load functions
source('functionssh.R')
# load obsevations
# load('obsVdHrly.RData')
# load('obsVdMonth.RData')
# # load simulations, read in readSilvaMod.R
# load('silva_mod_decid.RData')
# load('silva_mod_conif.RData')
# load('silva_mod_grass.RData')
# load('silva_mod_crop.RData')
# load('silva_mod_rainf.RData')

fare10met = read_meteo(casename = 'fare10met', dirname = 'shsun/', startyr = 2001, endyr = 2007, utc.adjust = -8)
hf9301met = read_meteo(casename = 'hf9301met', dirname = 'shsun/', startyr = 1993, endyr = 2001, utc.adjust = -4)
rannik12met = read_meteo(casename = 'rannik12met', dirname = 'shsun/', startyr = 2007, endyr = 2010, utc.adjust = 2)
wu18met = read_meteo(casename = 'wu18met', dirname = 'shsun/', startyr = 2008, endyr = 2013, utc.adjust = -5)  # MERRA2

fare10met.avghrly = avg_met_avghrly(fare10met, to_avg_month = 6:8, to_avg_dim = c(4))
hf9301met.avghrly = avg_met_avghrly(hf9301met, to_avg_month = 6:8, to_avg_dim = c(4))
rannik12met.avghrly = avg_met_avghrly(rannik12met, to_avg_month = 6:8, to_avg_dim = c(4))
wu18met.avghrly = avg_met_avghrly(wu18met, to_avg_month = 6:8, to_avg_dim = c(4))



gg = data.frame(cbind(hour = 0:23, fare10 = fare10met.avghrly$qv2m, hf9301 = hf9301met.avghrly$qv2m, rannik12 = rannik12met.avghrly$qv2m, wu18 = wu18met.avghrly$qv2m))
gg = data.frame(cbind(hour = 0:23, fare10 = fare10met.avghrly$t2m, hf9301 = hf9301met.avghrly$t2m, rannik12 = rannik12met.avghrly$t2m, wu18 = wu18met.avghrly$t2m))
gg = data.frame(cbind(hour = 0:23, fare10 = fare10met.avghrly$gwetroot, hf9301 = hf9301met.avghrly$gwetroot, rannik12 = rannik12met.avghrly$gwetroot, wu18 = wu18met.avghrly$gwetroot))
gg = data.frame(cbind(hour = 0:23, fare10 = fare10met.avghrly$pardf, hf9301 = hf9301met.avghrly$pardf, rannik12 = rannik12met.avghrly$pardf, wu18 = wu18met.avghrly$pardf))
#
gg = data.frame(cbind(hour = 0:23, fare10 = blogwetmet.avghrly$gwetroot, hf9301 = hf9301met.avghrly$gwetroot, rannik12 = rannik12met.avghrly$gwetroot, wu18 = wu18met.avghrly$gwetroot))

## plot met var test
gg = melt(gg, id = 'hour')
p = ggplot()+
  geom_line(data = gg, aes(x = hour, y = value, group = variable, colour = variable), size = 2)+
  labs(x = "hour", y = 'PARDF')+
  theme_bw()
p

### ----------------------------------------------------------------------------------------------------
# sensitivity test

blogwet = readFiles(casename = 'blogwet2', dirname = 'shsun/', startdate = 20010101, startyr = 2001, endyr = 2007, startmonth = 1, endmonth = 12, utc.adjust = -8)
bloqv2m = readFiles(casename = 'bloqv2m', dirname = 'shsun/', startdate = 20010101, startyr = 2001, endyr = 2007, startmonth = 1, endmonth = 12, utc.adjust = -8)
blopardf = readFiles(casename = 'blopardf', dirname = 'shsun/', startdate = 20010101, startyr = 2001, endyr = 2001, startmonth = 1, endmonth = 12, utc.adjust = -8)
blolai = readFiles(casename = 'fare10lai', dirname = 'shsun/', startdate = 20010101, startyr = 2001, endyr = 2007, startmonth = 1, endmonth = 12, utc.adjust = -8)
hfmodis = readFiles(casename = 'hf9301modis', dirname = 'shsun/', startdate = 19930101, startyr = 1993, endyr = 2001,startmonth = 1,endmonth = 12, utc.adjust = -4 )
 
wu18t2m = readFiles(casename = 'wu18t2m', dirname = 'shsun/', startdate = '20080101', startyr = 2008, endyr = 2013, startmonth = 1, endmonth = 12, utc.adjust = -5)
fare10hyy = readFiles(casename = 'fare10hyy', dirname = 'shsun/', startdate = 20010101, startyr = 2001, endyr = 2007, startmonth = 1, endmonth = 12, utc.adjust = -8)

blogwetmet = read_meteo(casename = 'blogwet2w9820010101',dirname = 'shsun/', startyr = 2001, endyr = 2007, utc.adjust = -8)

blogwetmet.avghrly = avg_met_avghrly(blogwetmet, to_avg_month = 6:8, to_avg_dim = c(4))

blogwet.jja.avghrly = f_mod_mean_sd(casename = blogwet, pft = 2, to_avg_month = 6:8)
bloqv2m.jja.avghrly = f_mod_mean_sd(casename = bloqv2m, pft = 2, to_avg_month = 6:8)
blopardf.jja.avghrly = f_mod_mean_sd(casename = blopardf, pft = 2, to_avg_month = 6:8)
blolai.jja.avghrly = f_mod_mean_sd(casename = blolai, pft = 2, to_avg_month = 6:8)
hfmodis.jja.avghrly = f_mod_mean_sd(casename = hfmodis, pft = 8, to_avg_month = 6:8)
wu18t2m.jja.avghrly = f_mod_mean_sd(casename = wu18t2m, pft = 8, to_avg_month = 6:9)
blohyy.jja.avghrly = f_mod_mean_sd(casename = fare10hyy, pft = 2, to_avg_month = 6:8)

plot_diurnal_obsmod(obs = blo.summer.avghrly, mod = blohyy.jja.avghrly, ylim_high = 0.015, title = "") #Blodgett JJA



plot_diurnal_obsmod(obs = blo.summer.avghrly, mod = blohyy.jja.avghrly, ylim_high = 0.02, title = "Blodgett", to_plot_var = 'rs') #Blodgett JJA

plot_diurnal_obsmod(obs = blo.summer.avghrly, mod = fare10.jja.avghrly, ylim_high = 0.02, title = "Blodgett", to_plot_var = 'rs') #Blodgett JJA
plot_diurnal_obsmod(obs = blo.summer.avghrly, mod = blogwet.jja.avghrly, ylim_high = 0.02, title = "Blodgett", to_plot_var = 'rs') #Blodgett JJA
plot_diurnal_obsmod(obs = blo.summer.avghrly, mod = bloqv2m.jja.avghrly, ylim_high = 0.02, title = "Blodgett", to_plot_var = 'rs') #Blodgett JJA
plot_diurnal_obsmod(obs = blo.summer.avghrly, mod = blopardf.jja.avghrly, ylim_high = 0.02, title = "Blodgett", to_plot_var = 'rs') #Blodgett JJA
plot_diurnal_obsmod(obs = blo.summer.avghrly, mod = blolai.jja.avghrly, ylim_high = 0.02, title = "Blodgett", to_plot_var = 'rs') #Blodgett JJA
plot_diurnal_obsmod(obs = hf.summer.avghrly, mod = hfmodis.jja.avghrly, ylim_high = 0.02, title = "Harvard", to_plot_var = 'rs') #Harvard JJA
plot_diurnal_obsmod(obs = hf.summer.avghrly, mod = hfmodis.jja.avghrly, ylim_high = 0.016, title = "") #Harvard JJA
plot_diurnal_obsmod(obs = 0.01*bf.summer.avghrly, mod = wu18t2m.jja.avghrly, ylim_high = 0.02, title = "Borden", to_plot_var = 'rs') #Borden JJAS



### ------------------ T2M Harvard Forest July 2001
hfn5 = readFiles(casename = 'hfn5', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfn5.hrly = f_mod_mean_sd(casename = hfn5, pft = 8, to_avg_month = 7)

hfn4 = readFiles(casename = 'hfn4', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfn4.hrly = f_mod_mean_sd(casename = hfn4, pft = 8, to_avg_month = 7)

hfn3 = readFiles(casename = 'hfn3', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfn3.hrly = f_mod_mean_sd(casename = hfn3, pft = 8, to_avg_month = 7)

hfn2 = readFiles(casename = 'hfn2', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfn2.hrly = f_mod_mean_sd(casename = hfn2, pft = 8, to_avg_month = 7)

hfn1 = readFiles(casename = 'hfn1', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfn1.hrly = f_mod_mean_sd(casename = hfn1, pft = 8, to_avg_month = 7)

hf0 = readFiles(casename = 'hfp0', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hf0.hrly = f_mod_mean_sd(casename = hf0, pft = 8, to_avg_month = 7)

hfp1 = readFiles(casename = 'hfp1', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfp1.hrly = f_mod_mean_sd(casename = hfp1, pft = 8, to_avg_month = 7)

hfp2 = readFiles(casename = 'hfp2', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfp2.hrly = f_mod_mean_sd(casename = hfp2, pft = 8, to_avg_month = 7)

hfp3 = readFiles(casename = 'hfp3', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfp3.hrly = f_mod_mean_sd(casename = hfp3, pft = 8, to_avg_month = 7)

hfp4 = readFiles(casename = 'hfp4', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfp4.hrly = f_mod_mean_sd(casename = hfp4, pft = 8, to_avg_month = 7)

hfp5 = readFiles(casename = 'hfp5', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfp5.hrly = f_mod_mean_sd(casename = hfp5, pft = 8, to_avg_month = 7)

# -------------------------
tmp = data.frame(t2m = -5, w89 = hfn5.hrly[['vd.w98']][,1][9], w89fbb = hfn5.hrly[['vd.w98fbb']][,1][9], w89med = hfn5.hrly[['vd.w98med']][,1][9], zh = hfn5.hrly[['vd.zh']][,1][9], zhfbb = hfn5.hrly[['vd.zhfbb']][,1][9], zhmed = hfn5.hrly[['vd.zhmed']][,1][9])

f_merge = function(case, to_merge_hour, test_sen,to_merge_var = 'vd'){
  tmp = data.frame(t2m = test_sen, w89 = case[[paste0(to_merge_var,'.w98')]][,1][to_merge_hour], w89fbb = case[[paste0(to_merge_var,'.w98fbb')]][,1][to_merge_hour], w89med = case[[paste0(to_merge_var,'.w98med')]][,1][to_merge_hour], zh = case[[paste0(to_merge_var,'.zh')]][,1][to_merge_hour], zhfbb = case[[paste0(to_merge_var,'.zhfbb')]][,1][to_merge_hour], zhmed = case[[paste0(to_merge_var,'.zhmed')]][,1][to_merge_hour])
  return(tmp)
}


to_test_hour = 15
tmp = data.frame(rbind(f_merge(hfn5.hrly, to_test_hour, -5), f_merge(hfn4.hrly, to_test_hour, -4), f_merge(hfn4.hrly, to_test_hour, -4),f_merge(hfn3.hrly, to_test_hour, -3),f_merge(hfn2.hrly, to_test_hour, -2),f_merge(hfn1.hrly, to_test_hour, -1),
                       f_merge(hf0.hrly, to_test_hour, 0),f_merge(hfp1.hrly, to_test_hour, 1),f_merge(hfp2.hrly, to_test_hour, 2),f_merge(hfp3.hrly, to_test_hour, 3),f_merge(hfp4.hrly, to_test_hour, 4), f_merge(hfp5.hrly, to_test_hour, 5)))
gghf = melt(tmp, id = 't2m')

p = ggplot()+
  geom_line(data = gghf, aes(x = t2m, y = value, group = variable, colour = variable), size = 1.2)+
  labs(x = 'T2M', y = bquote('Modelled'~O[3]~ ~V[d]~ m ~s^-1))+
  scale_x_continuous(breaks = seq(-5,5,by = 1))+
  scale_y_continuous(limits = c(0,0.02), breaks = c(0,0.005,0.01,0.015,0.02))+
  theme_bw()+
  theme(axis.text.x = element_text(colour = "grey20",size=15),
        axis.text.y = element_text(colour = "grey20",size=15))+
  theme(axis.title = element_text(face = "bold", size = 16))+
  theme(legend.text = element_text(size = 15), legend.title = element_blank())
p

## Gs
tmp = data.frame(rbind(f_merge(hfn5.hrly, to_test_hour, -5, to_merge_var = 'rs'), f_merge(hfn4.hrly, to_test_hour, -4, to_merge_var = 'rs'), f_merge(hfn4.hrly, to_test_hour, -4, to_merge_var = 'rs'),f_merge(hfn3.hrly, to_test_hour, -3, to_merge_var = 'rs'),f_merge(hfn2.hrly, to_test_hour, -2, to_merge_var = 'rs'),f_merge(hfn1.hrly, to_test_hour, -1, to_merge_var = 'rs'),
                       f_merge(hf0.hrly, to_test_hour, 0, to_merge_var = 'rs'),f_merge(hfp1.hrly, to_test_hour, 1, to_merge_var = 'rs'),f_merge(hfp2.hrly, to_test_hour, 2, to_merge_var = 'rs'),f_merge(hfp3.hrly, to_test_hour, 3, to_merge_var = 'rs'),f_merge(hfp4.hrly, to_test_hour, 4, to_merge_var = 'rs'), f_merge(hfp5.hrly, to_test_hour, 5, to_merge_var = 'rs')))


gghf = melt(tmp, id = 't2m')

p = ggplot()+
  geom_line(data = gghf, aes(x = t2m, y = value, group = variable, colour = variable), size = 1.2)+
  labs(x = 'T2M', y = bquote('Modelled'~O[3]~ ~G[s]~ m ~s^-1))+
  scale_x_continuous(breaks = seq(-5,5,by = 1))+
  scale_y_continuous(limits = c(0,0.02), breaks = c(0,0.005,0.01,0.015,0.02))+
  theme_bw()+
  theme(axis.text.x = element_text(colour = "grey20",size=15),
        axis.text.y = element_text(colour = "grey20",size=15))+
  theme(axis.title = element_text(face = "bold", size = 16))+
  theme(legend.text = element_text(size = 15), legend.title = element_blank())
p




# ---------------------------------------
hfgn3 = readFiles(casename = 'hfgn0.3', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfgn3.hrly = f_mod_mean_sd(casename = hfgn3, pft = 8, to_avg_month = 7)

hfgn2 = readFiles(casename = 'hfgn0.2', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfgn2.hrly = f_mod_mean_sd(casename = hfgn2, pft = 8, to_avg_month = 7)

hfgn1 = readFiles(casename = 'hfgn0.1', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfgn1.hrly = f_mod_mean_sd(casename = hfgn1, pft = 8, to_avg_month = 7)

hfg0 = readFiles(casename = 'hfg0', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfg0.hrly = f_mod_mean_sd(casename = hfg0, pft = 8, to_avg_month = 7)

hfg1 = readFiles(casename = 'hfg0.1', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfg1.hrly = f_mod_mean_sd(casename = hfg1, pft = 8, to_avg_month = 7)

hfg2 = readFiles(casename = 'hfg0.2', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfg2.hrly = f_mod_mean_sd(casename = hfg2, pft = 8, to_avg_month = 7)

hfg3 = readFiles(casename = 'hfg0.3', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfg3.hrly = f_mod_mean_sd(casename = hfg3, pft = 8, to_avg_month = 7)



f_merge = function(case, to_merge_hour, test_sen,to_merge_var = 'vd'){
  tmp = data.frame(gwetroot = test_sen, w89 = case[[paste0(to_merge_var,'.w98')]][,1][to_merge_hour], w89fbb = case[[paste0(to_merge_var,'.w98fbb')]][,1][to_merge_hour], w89med = case[[paste0(to_merge_var,'.w98med')]][,1][to_merge_hour], zh = case[[paste0(to_merge_var,'.zh')]][,1][to_merge_hour], zhfbb = case[[paste0(to_merge_var,'.zhfbb')]][,1][to_merge_hour], zhmed = case[[paste0(to_merge_var,'.zhmed')]][,1][to_merge_hour])
  return(tmp)
}


to_test_hour = 15
tmp = data.frame(rbind(f_merge(hfgn3.hrly, to_test_hour, -0.3, to_merge_var = 'vd'), f_merge(hfgn2.hrly, to_test_hour, -0.2, to_merge_var = 'vd'),f_merge(hfn2.hrly, to_test_hour, -0.2, to_merge_var = 'vd'),f_merge(hfn1.hrly, to_test_hour, -0.1, to_merge_var = 'vd'),
                       f_merge(hfg0.hrly, to_test_hour, 0, to_merge_var = 'vd'),f_merge(hfg1.hrly, to_test_hour, 0.1, to_merge_var = 'vd'),f_merge(hfg2.hrly, to_test_hour, 0.2, to_merge_var = 'vd'),f_merge(hfg3.hrly, to_test_hour, 0.3, to_merge_var = 'vd')))

gghf = melt(tmp, id = 'gwetroot')

p = ggplot()+
  geom_line(data = gghf, aes(x = gwetroot, y = value, group = variable, colour = variable), size = 1.2)+
  labs(x = 'GWETROOT', y = bquote('Modelled'~O[3]~ ~V[d]~ m ~s^-1))+
  scale_x_continuous(limits = c(-0.3,0.3), breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))+
  scale_y_continuous(limits = c(0,0.022), breaks = c(0,0.005,0.01,0.015,0.02))+
  theme_bw()+
  theme(axis.text.x = element_text(colour = "grey20",size=15),
        axis.text.y = element_text(colour = "grey20",size=15))+
  theme(axis.title = element_text(face = "bold", size = 16))+
  theme(legend.text = element_text(size = 15), legend.title = element_blank())
p





# ---------------------------------------
hfqn4 = readFiles(casename = 'hfqn4', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfqn4.hrly = f_mod_mean_sd(casename = hfqn4, pft = 8, to_avg_month = 7)

hfqn3 = readFiles(casename = 'hfqn3', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfqn3.hrly = f_mod_mean_sd(casename = hfqn3, pft = 8, to_avg_month = 7)

hfqn2 = readFiles(casename = 'hfqn2', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfqn2.hrly = f_mod_mean_sd(casename = hfqn2, pft = 8, to_avg_month = 7)

hfqn1 = readFiles(casename = 'hfqn1', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfqn1.hrly = f_mod_mean_sd(casename = hfqn1, pft = 8, to_avg_month = 7)

hfq0 = readFiles(casename = 'hfq0', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfq0.hrly = f_mod_mean_sd(casename = hfq0, pft = 8, to_avg_month = 7)

hfq1 = readFiles(casename = 'hfq1', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfq1.hrly = f_mod_mean_sd(casename = hfq1, pft = 8, to_avg_month = 7)

hfq2 = readFiles(casename = 'hfq2', dirname = 'shsun/', startdate = 20010701, startyr = 2001, endyr = 2001,startmonth = 7,endmonth = 7, utc.adjust = -4 )
hfq2.hrly = f_mod_mean_sd(casename = hfq2, pft = 8, to_avg_month = 7)


f_merge = function(case, to_merge_hour, test_sen,to_merge_var = 'vd'){
  tmp = data.frame(qv2m = test_sen, w89 = case[[paste0(to_merge_var,'.w98')]][,1][to_merge_hour], w89fbb = case[[paste0(to_merge_var,'.w98fbb')]][,1][to_merge_hour], w89med = case[[paste0(to_merge_var,'.w98med')]][,1][to_merge_hour], zh = case[[paste0(to_merge_var,'.zh')]][,1][to_merge_hour], zhfbb = case[[paste0(to_merge_var,'.zhfbb')]][,1][to_merge_hour], zhmed = case[[paste0(to_merge_var,'.zhmed')]][,1][to_merge_hour])
  return(tmp)
}


to_test_hour =15
tmp = data.frame(rbind(f_merge(hfqn4.hrly, to_test_hour, -0.004, to_merge_var = 'vd'), f_merge(hfqn3.hrly, to_test_hour, -0.003, to_merge_var = 'vd'), f_merge(hfqn2.hrly, to_test_hour, -0.002, to_merge_var = 'vd'),f_merge(hfqn1.hrly, to_test_hour, -0.001, to_merge_var = 'vd'),
                       f_merge(hfq0.hrly, to_test_hour, 0, to_merge_var = 'vd'),f_merge(hfq1.hrly, to_test_hour, 0.001, to_merge_var = 'vd'),f_merge(hfq2.hrly, to_test_hour, 0.002, to_merge_var = 'vd')))

gghf = melt(tmp, id = 'qv2m')

p = ggplot()+
  geom_line(data = gghf, aes(x = qv2m, y = value, group = variable, colour = variable), size = 1.2)+
  labs(x = 'QV2M', y = bquote('Modelled'~O[3]~ ~V[d]~ m ~s^-1))+
  scale_x_continuous(limits = c(-0.004,0.002), breaks = c(-0.004, -0.003, -0.002, -0.001, 0, 0.001, 0.002))+
  scale_y_continuous(limits = c(0,0.023), breaks = c(0,0.005,0.01,0.015,0.02))+
  theme_bw()+
  theme(axis.text.x = element_text(colour = "grey20",size=15),
        axis.text.y = element_text(colour = "grey20",size=15))+
  theme(axis.title = element_text(face = "bold", size = 16))+
  theme(legend.text = element_text(size = 15), legend.title = element_blank())
p












