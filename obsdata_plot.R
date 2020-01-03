# Supplementary code for "Comparison and evaluation of different ozone dry deposition schemes in a terrestrial biospheric model"
# This script is used to: 
# 1. Process o3 flux data from Harvard forest, borden forest, hyytiala forest, blodgett forest
# 2. Generate diurnal, daily, monthly (daytime) average vd,o3 and gs
# 3. Plot 
#

# ===================================================================================================================================================
# 1. Process data 
# ===================================================================================================================================================
source('obsdata_func.R')
#
# --------------------------------- harvard forest ------------------------------------
#

# load o3 flux and lai data 
# o3flux data: http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf004
# lai data: http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf069
fl.01 = read.csv('~/Dropbox/DryDepEval/HarvardForest/flux/hf004-01-final.csv')
fl.02 = read.csv('~/Dropbox/DryDepEval/HarvardForest/flux/hf004-02-filled.csv') 
fl.lai = read.csv('~/Dropbox/DryDepEval/HarvardForest/hf069-02-LAI-site.csv') 
fl.lai = fl.lai[which(fl.lai$site == 'ems'), c("year", "doy", "decid.mean")] # use ems site

lai.inter.daily = interp_lai(1998, fl.lai)
for (iyear in c(1999,2005:2017)) {
  tmp = interp_lai(iyear = iyear, df = fl.lai)
  lai.inter.daily = rbind(lai.inter.daily, tmp)
}

lai.avg.daily = aggregate(lai.inter.daily$decid.mean, list(lai.inter.daily$doy), mean) # multi-year mean lai
names(lai.avg.daily) = c('doy', 'decid.mean')

names(fl.01)[names(fl.01) == 'doy'] = 'doy.hr'  # to merge with lai 
fl.01$doy = floor(fl.01$doy.hr)
fl.01 = merge(fl.01, lai.avg.daily, by = c('doy'))
fl.01 = merge(fl.02[,c('datetime','ustar')], fl.01, by = c('datetime')) # to merge ustar from hf004-02
fl.01$ustar = fl.01$ustar/100
fl.01$z0 = 24 * (0.215 - (fl.01$decid.mean^0.25)/10)
fl.01$d  = 24 * (0.1 + (fl.01$decid.mean^0.2)/2)
fl.01$ustar.cal = von_Kar * fl.01$wind.sp/log((29-fl.01$d)/fl.01$z0)
fl.01$ustar = mapply(replace.ustar, fl.01$ustar, fl.01$ustar.cal)
remove(fl.02); remove(fl.lai)

# compute aerodynamic resistance, boundary layer resistance, stomatal resistance, ozone dry deposition velocity
fl.01$mix.ratio = mapply(mix_ratio, fl.01$ta.27.9m, fl.01$rh.27.9m, fl.01$p.amb)
fl.01$tv.27.9m = fl.01$ta.27.9m*(1+0.61*fl.01$mix.ratio)
fl.01$L = mapply(obk, fl.01$tv.27.9m, fl.01$ustar, fl.01$f.heat)  # f.heat W m^-2: sensible heat at 29m 
fl.01$psi.H = mapply(psi.H, fl.01$d, fl.01$L) 
fl.01$r.a = mapply(r.a, fl.01$z0, fl.01$ustar, fl.01$d, fl.01$L, fl.01$psi.H)
fl.01$r.b = mapply(r.b, fl.01$ustar)
fl.01$r.s = mapply(r.s.penman.monteith, fl.01$p.amb, fl.01$ta.27.9m, fl.01$f.heat, fl.01$f.h2o, fl.01$rh.27.9m, fl.01$r.a, fl.01$r.b, fl.01$ustar)
fl.01$o3vd = mapply(v.d, fl.01$p.amb, fl.01$f.o3, fl.01$o3.mlb) 
fl.01 = fl.01[which(!is.na(fl.01$o3vd) & !is.infinite(fl.01$o3vd)),]

hf.data = fl.01[c('datetime','ustar','rh.27.9m','f.heat','f.h2o','year','doy','month','wind.sp','f.o3','o3.mlb','o3vd','r.a','r.b','r.s')]

# add filter 
hf.data$tmptime = mapply(strsplit, as.vector(hf.data$datetime), "") # time stamp
hf.data$day = mapply(generate_day, hf.data$tmptime)
hf.data$year = mapply(generate_year, hf.data$tmptime)
hf.data$hour = mapply(generate_hour, hf.data$tmptime)
hf.data$month = mapply(generate_month, hf.data$tmptime)
hf.data$tmptime = NULL
dput(colnames(hf.data)) # rearrrange coloumn order

# (1) keep if ustar > 0.2 & !is.na(o3vd) & is.na(sensible heat) & is.na(water vapor flux)
hf.hrly = hf.data[which(!is.na(hf.data$o3vd) & !is.na(hf.data$f.heat) & !is.na(hf.data$f.h2o) & !is.infinite(hf.data$o3vd) & (hf.data$ustar > 0.2)),]

# (2) keep o3vd within +- 3 std
mean.vd = mean(hf.hrly$o3vd) 
sd.vd = sd(hf.hrly$o3vd)
outlier.max = mean.vd + sd.vd * 3
outlier.min = mean.vd - sd.vd * 3
hf.hrly = hf.hrly[which(hf.hrly$o3vd < outlier.max & hf.hrly$o3vd > outlier.min),]

# (3) remove days with too many missing obs to calculate monthly mean and daily mean
hf.hrly$non.miss = F
# remove days with too many missing hourly obs
for (iyear in 1997:2015) { 
  for (imonth in 1:12) {
    for (iday in 1:31) {
      if (iyear <= 1996){ # 8 measurements per day
        if (nrow(hf.hrly[which(hf.hrly$month == imonth & hf.hrly$year == iyear & hf.hrly$day == iday),]) >= 3) { # 9, 12, 15
          hf.hrly$non.miss[hf.hrly$month == imonth & hf.hrly$year == iyear & hf.hrly$day == iday] = T
        }
      }else{ # max 24 measurements per day
        if (nrow(hf.hrly[which(hf.hrly$month == imonth & hf.hrly$year == iyear & hf.hrly$day == iday),]) >= 7) { # 9,10,11,12,13,14,15
          hf.hrly$non.miss[hf.hrly$month == imonth & hf.hrly$year == iyear & hf.hrly$day == iday] = T
        }
      }
    }
  }
}

# (4) mark months with too many missing days
tmpdf = hf.hrly[c('year','month','day','non.miss')]
tmpdf = tmpdf[which(tmpdf$non.miss == T),]
tmpdf = unique(tmpdf[,1:3]) ## non-missing data
tmpdf$non.miss = F ## sum of non-miss days
for (iyear in 1991:2015) {
  for (imonth in 1:12) {
    if (nrow(tmpdf[which(tmpdf$year == iyear & tmpdf$month == imonth),]) >= 7) { ## keep if non-miss days > 25% of days in that months
      tmpdf$non.miss[tmpdf$month == imonth & tmpdf$year == iyear] = T
    }
  }
}
non.miss.month = unique(tmpdf[,c(1,2,4)])

# average daily o3vd 
hf.hrly = hf.hrly[c('year','month','day','hour','f.o3','o3vd','r.a','r.b','r.s')]
hf.daytime.hrly = hf.hrly[which(hf.hrly$hour >= 9 & hf.hrly$hour <= 15),]  # daytime daily vd: 9:00 ~ 15:00 
hf.daily = aggregate(hf.hrly[,c('f.o3','o3vd','r.a','r.b','r.s')], list(hf.hrly$day, hf.hrly$month, hf.hrly$year), mean, na.rm = T) # daily vd 24hr
colnames(hf.daily)[1:3] = c('day','month','year')

# bootstrapping average monthly vd
hf.btstrp.day.avgmly = array(NA, dim = c(12,3)) # nyear, nmonth, (95%CI mean, 5th CI, 95% CI) 
for (imonth in 1:12) {
  tmpvd = hf.day.daily[which(hf.day.daily$month == imonth),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') # get 95% confidence interval
    hf.btstrp.day.avgmly[imonth, 1] = results$t0
    # hf.btstrp.day.avgmly[imonth, 2] = ci.results$bca[4]
    # hf.btstrp.day.avgmly[imonth, 3] = ci.results$bca[5]
    hf.btstrp.day.avgmly[imonth, 2] = sd_trim(tmpboot, trim = 0.25, const = F) # sd
  }
}

# bootstrapping seasonal average diurnal cycle vd
hf.btstrp.summer.avghrly = array(NA, dim = c(24,3))  # hour, mean 95CI
hf.btstrp.winter.avghrly = array(NA, dim = c(24,3))
for (ihour in 1:24) {
  tmpvd = hf.hrly[which(hf.hrly$hour == (ihour-1) & (hf.hrly$month == 6 | hf.hrly$month == 7 | hf.hrly$month == 8)),]  # summer
  if (nrow(tmpvd)>30) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca')
    hf.btstrp.summer.avghrly[ihour, 1] = results$t0
    hf.btstrp.summer.avghrly[ihour, 2] = ci.results$bca[4]
    hf.btstrp.summer.avghrly[ihour, 3] = ci.results$bca[5]
    hf.btstrp.summer.avghrly[ihour, 2] = sd_trim(tmpvd$o3vd, trim = 0.25, const = F) # use standard deviation
  }
  tmpvd = hf.hrly[which(hf.hrly$hour == (ihour-1) & (hf.hrly$month ==12 | hf.hrly$month == 1 | hf.hrly$month == 2)),] # winter
  if (nrow(tmpvd)>10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca')
    hf.btstrp.winter.avghrly[ihour, 1] = results$t0
    hf.btstrp.winter.avghrly[ihour, 2] = ci.results$bca[4]
    hf.btstrp.winter.avghrly[ihour, 3] = ci.results$bca[5]
  }
}

#
# ------------------------------ borden forest ---------------------------------
#

# data: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2017MS001231
fl = read.csv('~/Dropbox/DryDepEval/Wu2018/ModelInterComparison-DryDeposition-DDMC-ON_Borden-2008_2013.csv', stringsAsFactors = F)
bf.data = fl[60:nrow(fl),] # remove rows of file description
rownames(bf.data) = 1:nrow(bf.data)
bf.data = bf.data[,1:12]
for (icol in 1:12) {
  colnames(bf.data)[icol] = as.character(bf.data[1,icol])
}
bf.data = bf.data[-1:-2,]
bf.data = bf.data[,-1]
remove(fl)

bf.data$date = format(as.Date(bf.data$DateStartLocalTime, format = "%Y-%m-%d")) # 
bf.data$month = lubridate::month(bf.data$date)
bf.data$year = lubridate::year(bf.data$date)
bf.data$day = lubridate::day(bf.data$date)

bf.hrly = bf.data[,c(3,11:15)]
bf.hrly$hour = mapply(convert_hour, bf.hrly$TimeStartLocalTime)
bf.hrly$TimeStartLocalTime = NULL

bf.hrly$o3vd = mapply(as.numeric, bf.hrly$VelocityDepO3obs_cms)
bf.hrly = bf.hrly[,-1]
bf.hrly = bf.hrly[-nrow(bf.hrly),]  # remove last row (NA)
bf.hrly$o3vd = mapply(convert_na, bf.hrly$o3vd)
bf.hrly = na.omit(bf.hrly)

# filter 
bf.hrly$vdfilt = NA
bf.hrly$vdfilt = mapply(negativ_vd_filter, bf.hrly$o3vd) 
bf.hrly$rank = rank(-bf.hrly$o3vd)  # rank 1 is highest
bf.hrly$vdfilt = mapply(remove_vd_binary, bf.hrly$o3vd, bf.hrly$rank, 3516)  # remove same portion of positive high vd values
bf.hrly = bf.hrly[c('month','year','day','hour','vdfilt')]
colnames(bf.hrly)[5] = 'o3vd'
bf.hrly = na.omit(bf.hrly)

# daily average
bf.vd.daily = aggregate(bf.hrly[,c('o3vd')], list(bf.hrly$day, bf.hrly$month, bf.hrly$year), mean, na.rm = T) # o3vd daily average 
colnames(bf.vd.daily) = c('day','month','year','o3vd')

# daytime daily average
tmp = bf.hrly[which(bf.hrly$hour >= 6 & bf.hrly$hour <= 18),]  # daytime 6:00 - 18:00, to compare with wu et al. 2018 
bf.vd.day.daily =  aggregate(tmp[,c('o3vd')], list(tmp$day, tmp$month, tmp$year), mean, na.rm = T) # o3vd daily average 
colnames(bf.vd.day.daily) = c('day','month','year','o3vd')
remove(tmp)
bf.day.vd = array(NA, dim = c(6,12,30)) 
for (iyear in 2008:2013) {
  for (imonth in 1:12) {
    for (iday in 1:30) {
      if (nrow(bf.vd.day.daily[which(bf.vd.day.daily$year == iyear & bf.vd.day.daily$month == imonth & bf.vd.day.daily$day == iday),]) != 0) {
        bf.day.vd[iyear-2007, imonth, iday] = 0.01*tmp$o3vd  ## cm/s -> m/s to compare with simulation
      }
    }
  }
}

# bootstrap monthly aveage 
bf.btstrp.avgmonthly = array(NA, dim = c(12,3))
for (imonth in 1:12) {
  tmpvd = bf.vd.daily[which(bf.vd.daily$month == imonth),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') 
    bf.btstrp.avgmonthly[imonth, 1] = results$t0
    bf.btstrp.avgmonthly[imonth, 2] = ci.results$bca[4]
    bf.btstrp.avgmonthly[imonth, 3] = ci.results$bca[5]
  }
}

# daytime bootstrp monthly average
bf.btstrp.day.avgmonthly = array(NA, dim = c(12,3))
for (imonth in 1:12) {
  tmpvd = bf.vd.day.daily[which(bf.vd.day.daily$month == imonth),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') 
    bf.btstrp.day.avgmonthly[imonth, 1] = results$t0
    bf.btstrp.day.avgmonthly[imonth, 2] = ci.results$bca[4]
    bf.btstrp.day.avgmonthly[imonth, 3] = ci.results$bca[5]
    bf.btstrp.day.avgmonthly[imonth, 2] = sd_trim(tmpboot, trim = 0.25, const = F) # sd
  }
}

# seasonal mean,  standard deviation
bf.summer.avghrly =  array(NA, dim = c(24,2))  # nhour, mean, sd
bf.winter.avghrly =  array(NA, dim = c(24,2))  
bf.spring.avghrly = array(NA, dim = c(24,2))
bf.fall.avghrly = array(NA, dim = c(24,2))
for (ihour in 1:24) {
  #summer
  tmpvd = bf.hrly.vd.all[which(bf.hrly.vd.all$hour == ihour-1 & (bf.hrly.vd.all$month == 6 | bf.hrly.vd.all$month == 7 | bf.hrly.vd.all$month == 8 | bf.hrly.vd.all$month == 9)),]
  if (nrow(tmpvd) > 10) {
    #tmp = tmpvd$o3vd
    tmp = tmpvd[(tmpvd$o3vd < quantile(tmpvd$o3vd, 0.8) & tmpvd$o3vd > quantile(tmpvd$o3vd, 0.2)),]
    tmp = tmp$o3vd
    bf.summer.avghrly[ihour,1] = mean(tmp, na.rm = T)
    bf.summer.avghrly[ihour,2] = sd(tmp,na.rm = T)
  }
  #winter
  tmpvd = bf.hrly.vd.all[which(bf.hrly.vd.all$hour == (ihour-1) & (bf.hrly.vd.all$month == 12 | bf.hrly.vd.all$month == 1 | bf.hrly.vd.all$month == 2 | bf.hrly.vd.all$month == 11 | bf.hrly.vd.all$month == 3 | bf.hrly.vd.all$month == 4)),]
  if (nrow(tmpvd) > 10) {
    tmp = tmpvd$o3vd
    bf.winter.avghrly[ihour,1] = mean(tmp, na.rm = T)
    bf.winter.avghrly[ihour,2] = sd(tmp,na.rm = T)
  }
  #spring
  tmpvd = bf.hrly.vd.all[which(bf.hrly.vd.all$hour == ihour-1 & (bf.hrly.vd.all$month == 3 | bf.hrly.vd.all$month == 4 | bf.hrly.vd.all$month == 5)),]
  if (nrow(tmpvd) > 10) {
    #tmp = tmpvd$o3vd
    tmp = tmpvd[(tmpvd$o3vd < quantile(tmpvd$o3vd, 0.8) & tmpvd$o3vd > quantile(tmpvd$o3vd, 0.2)),]
    tmp = tmp$o3vd
    bf.spring.avghrly[ihour,1] = mean(tmp, na.rm = T)
    bf.spring.avghrly[ihour,2] = sd(tmp,na.rm = T)
  }
  #fall
  tmpvd = bf.hrly.vd.all[which(bf.hrly.vd.all$hour == ihour-1 & (bf.hrly.vd.all$month == 9 | bf.hrly.vd.all$month == 10 | bf.hrly.vd.all$month == 11)),]
  if (nrow(tmpvd) > 10) {
    #tmp = tmpvd$o3vd
    tmp = tmpvd[(tmpvd$o3vd < quantile(tmpvd$o3vd, 0.8) & tmpvd$o3vd > quantile(tmpvd$o3vd, 0.2)),]
    tmp = tmp$o3vd
    bf.fall.avghrly[ihour,1] = mean(tmp, na.rm = T)
    bf.fall.avghrly[ihour,2] = sd(tmp,na.rm = T)
  }
}

bf.summer.avghrly[,1] = wu.summer.2018.decid$o3vd # wu et al. 2018

#
# --------------------------------- hyytiala forest ------------------------------------
#

fl = read.csv('~/Dropbox/DryDepEval/dryDepData/smear2/smeardata_20070101120000-flx.csv')
fl.o3 = read.csv('~/Dropbox/DryDepEval/dryDepData/smear2/smeardata_20070101120000-o3.csv')
hyy.data = merge(fl, fl.o3, by = c('Year','Month','Day','Hour','Minute','Second'))
hyy.data = hyy.data[which(!is.na(hyy.data$HYY_EDDYTOW.F_O3_radtow) & !is.na(hyy.data$HYY_META.O3336) & !is.na(hyy.data$HYY_META.O3168)),]
hyy.data$co3 = mapply(mean_o3, hyy.data$HYY_META.O3336, hyy.data$HYY_META.O3168)
hyy.data$o3vd = -hyy.data$HYY_EDDYTOW.F_O3_radtow/hyy.data$co3
hyy.data = hyy.data[,c(1:4,ncol(hyy.data))]

# daily average
hyy.daily = aggregate(hyy.data[,5], list(hyy.data$Day, hyy.data$Month, hyy.data$Year), mean, na.rm = T)
colnames(hyy.daily) = c('day','month','year','o3vd')
tmp = hyy.data[which(hyy.data$Hour <= 15 & hyy.data$Hour >= 9),]
hyy.daytime.daily = aggregate(tmp[,5], list(tmp$Day, tmp$Month, tmp$Year), mean, na.rm = T)
colnames(hyy.daytime.daily) = c('day','month','year','o3vd')
remove(tmp)
hyy.dly.vd = array(NA, dim = c(4,12,30)) # year,month,day
hyy.day.dly.vd = array(NA, dim = c(4,12,30))  
for (iyear in 2007:2010) {
  for (imonth in 1:12) {
    for (iday in 1:30) {
      tmp = hyy.daily[which(hyy.daily$year == iyear & hyy.daily$month == imonth & hyy.daily$day == iday),]
      if (nrow(tmp) != 0) {
        hyy.dly.vd[iyear-2006,imonth,iday] = tmp$o3vd
      }
      tmp = hyy.daytime.daily[which(hyy.daytime.daily$year == iyear & hyy.daytime.daily$month == imonth & hyy.daytime.daily$day == iday),]
      if (nrow(tmp) != 0) {
        hyy.day.dly.vd[iyear-2006,imonth,iday] = tmp$o3vd
      }
    }
  }
}

# multiyear bootstrapping monthly average obs o3vd
hyy.btstrp.avgmonthly = array(NA, dim = c(12,3)) # nmonth, (95%CI mean, 5th CI, 97.5% CI) 
hyy.btstrp.daytime.avgmonthly = array(NA, dim = c(12,3))
for (imonth in 1:12) {
  tmpvd = hyy.daily[which(hyy.daily$month == imonth),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') 
    hyy.btstrp.avgmonthly[imonth, 1] = results$t0
    hyy.btstrp.avgmonthly[imonth, 2] = ci.results$bca[4]
    hyy.btstrp.avgmonthly[imonth, 3] = ci.results$bca[5]
  }
  tmpvd = hyy.daytime.daily[which(hyy.daytime.daily$month == imonth),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') 
    hyy.btstrp.daytime.avgmonthly[imonth, 1] = results$t0
    hyy.btstrp.daytime.avgmonthly[imonth, 2] = ci.results$bca[4]
    hyy.btstrp.daytime.avgmonthly[imonth, 3] = ci.results$bca[5]
    # hyy.btstrp.daytime.avgmonthly[imonth, 2] = sd_trim(tmpboot, trim = 0.25, const = F)
  }
}

# Bootstrapping seasonal diurnal mean vd,o3 
hyy.btstrp.summer.avghrly = array(NA, dim = c(24, 3)) # nhour, mean, 95CI
hyy.btstrp.spring.avghrly = array(NA, dim = c(24, 3)) # nhour, mean, 95CI
hyy.btstrp.winter.avghrly = array(NA, dim = c(24, 3)) # nhour, mean, 95CI
hyy.btstrp.fall.avghrly = array(NA, dim = c(24, 3)) # nhour, mean, 95CI
for (ihour in 1:24) {  ## hour 0 ~ 23 
  # summer
  tmpvd = hyy.data[which(hyy.data$Hour == (ihour-1) & (hyy.data$Month == 6 | hyy.data$Month == 7 | hyy.data$Month == 8)),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') # get 95% confidence interval
    hyy.btstrp.summer.avghrly[ihour, 1] = results$t0
    # hyy.btstrp.summer.avghrly[ihour, 2] = ci.results$bca[4]
    # hyy.btstrp.summer.avghrly[ihour, 3] = ci.results$bca[5]
    
    # use sd
    hyy.btstrp.summer.avghrly[ihour, 2] = sd_trim(tmpvd$o3vd, trim = 0.25, const = F)
    
  }
  # spring
  tmpvd = hyy.data[which(hyy.data$Hour == (ihour-1) & (hyy.data$Month == 3 | hyy.data$Month == 4 | hyy.data$Month == 5)),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') # get 95% confidence interval
    hyy.btstrp.spring.avghrly[ihour, 1] = results$t0
    hyy.btstrp.spring.avghrly[ihour, 2] = ci.results$bca[4]
    hyy.btstrp.spring.avghrly[ihour, 3] = ci.results$bca[5]
  }
  # winter
  tmpvd = hyy.data[which(hyy.data$Hour == (ihour-1) & (hyy.data$Month == 12 | hyy.data$Month == 1 | hyy.data$Month == 2)),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') # get 95% confidence interval
    hyy.btstrp.winter.avghrly[ihour, 1] = results$t0
    hyy.btstrp.winter.avghrly[ihour, 2] = ci.results$bca[4]
    hyy.btstrp.winter.avghrly[ihour, 3] = ci.results$bca[5]
  }
  # fall
  tmpvd = hyy.data[which(hyy.data$Hour == (ihour-1) & (hyy.data$Month == 9 | hyy.data$Month == 10 | hyy.data$Month == 11)),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') # get 95% confidence interval
    hyy.btstrp.fall.avghrly[ihour, 1] = results$t0
    hyy.btstrp.fall.avghrly[ihour, 2] = ci.results$bca[4]
    hyy.btstrp.fall.avghrly[ihour, 3] = ci.results$bca[5]
  }
}



# --------------------------------- blodgett forest ------------------------------------
startyr.blo = 2001; endyr.blo = 2007
blo = readxl::read_excel('~/Dropbox/DryDepEval/dryDepData/blodgett/Blodgett.xlsx')
blo[blo == -9999] = NA
blo$date = as.Date(blo$`Time of year`, origin = paste0(blo$Year, '-01-01'))
blo$month = lubridate::month(blo$date)
blo$day = lubridate::day(blo$date)
colnames(blo)[25] = 'o3vd'
blo = blo[which(blo$o3vd < 0.05),]

# daily average 
blo.obs.daily = aggregate(blo[,c('o3vd')], list(blo$Year, blo$month, blo$day), mean, na.rm = T)
colnames(blo.obs.daily) = c('year','month','day','o3vd')
tmp = blo[which(blo$Hour >= 6 & blo$Hour <= 18),]  # 4/15/2019
blo.obs.day.daily = aggregate(tmp[,c('o3vd')], list(tmp$Year, tmp$month, tmp$day), mean, na.rm = T)
colnames(blo.obs.day.daily) = c('year','month','day','o3vd')
remove(tmp)
blo.dly.vd = array(NA, c(endyr.blo-startyr.blo+1, 12, 30))
blo.day.dly.vd = array(NA, c(endyr.blo-startyr.blo+1, 12, 30)) # daytime
for (iyear in startyr.blo:endyr.blo) {
  for (imonth in 1:12) {
    for (iday in 1:30) {
      tmp = blo.obs.daily[which(blo.obs.daily$year == iyear & blo.obs.daily$month == imonth & blo.obs.daily$day == iday),]
      if (nrow(tmp)!=0) {
        blo.dly.vd[iyear - startyr.blo + 1, imonth, iday] = tmp$o3vd
      }
      tmp = blo.obs.day.daily[which(blo.obs.day.daily$year == iyear & blo.obs.day.daily$month == imonth & blo.obs.day.daily$day == iday),]
      if (nrow(tmp)!=0) {
        blo.day.dly.vd[iyear - startyr.blo + 1, imonth, iday] = tmp$o3vd
      }
    }
  }
}

# bootstrapping monthly average 
blo.btstrp.avgmonthly = array(NA, dim = c(12,3))
blo.btstrp.day.avgmonthly = array(NA, dim = c(12,3)) # daytime
for (imonth in 1:12) {
  tmpvd = blo.obs.daily[which(blo.obs.daily$month == imonth),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') 
    blo.btstrp.avgmonthly[imonth, 1] = results$t0
    blo.btstrp.avgmonthly[imonth, 2] = ci.results$bca[4]
    blo.btstrp.avgmonthly[imonth, 3] = ci.results$bca[5]
  }
  
  tmpvd = blo.obs.day.daily[which(blo.obs.day.daily$month == imonth),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') 
    blo.btstrp.day.avgmonthly[imonth, 1] = results$t0
    blo.btstrp.day.avgmonthly[imonth, 2] = ci.results$bca[4]
    blo.btstrp.day.avgmonthly[imonth, 3] = ci.results$bca[5]
    # blo.btstrp.day.avgmonthly[imonth, 2] = sd_trim(tmpboot, trim = 0.25, const = F) # sd
    
  }
}

# Bootstrapping seasonal diurnal mean vd,o3
blo.btstrp.summer.avghrly = array(NA, dim = c(24, 3)) 
blo.btstrp.spring.avghrly = array(NA, dim = c(24, 3)) 
blo.btstrp.winter.avghrly = array(NA, dim = c(24, 3)) 
blo.btstrp.fall.avghrly = array(NA, dim = c(24, 3)) 
for (ihour in 1:24) {  ## hour 0 ~ 23 
  # summer
  tmpvd = blo[which(blo$Hour >= (ihour-1) & blo$Hour < ihour & (blo$month == 6 | blo$month == 7 | blo$month == 8)),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca')
    blo.btstrp.summer.avghrly[ihour, 1] = results$t0
    blo.btstrp.summer.avghrly[ihour, 2] = ci.results$bca[4]
    blo.btstrp.summer.avghrly[ihour, 3] = ci.results$bca[5]
    # blo.btstrp.summer.avghrly[ihour, 2] = sd_trim(tmpboot, trim = 0.25, const = F)
  }
  # spring
  tmpvd = blo[which(blo$Hour == (ihour-1) & (blo$month == 3 | blo$month == 4 | blo$month == 5)),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') 
    blo.btstrp.spring.avghrly[ihour, 1] = results$t0
    blo.btstrp.spring.avghrly[ihour, 2] = ci.results$bca[4]
    # blo.btstrp.spring.avghrly[ihour, 3] = ci.results$bca[5]
  }
  # winter
  tmpvd = blo[which(blo$Hour >= (ihour-1) & blo$Hour < ihour & (blo$month == 12 | blo$month == 1 | blo$month == 2)),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') # get 95% confidence interval
    blo.btstrp.winter.avghrly[ihour, 1] = results$t0
    blo.btstrp.winter.avghrly[ihour, 2] = ci.results$bca[4]
    blo.btstrp.winter.avghrly[ihour, 3] = ci.results$bca[5]
  }
  # fall
  tmpvd = blo[which(blo$Hour >= (ihour-1) & blo$Hour < ihour & (blo$month == 9 | blo$month == 10 | blo$month == 11)),]
  if (nrow(tmpvd) > 10) {
    tmpboot = tmpvd$o3vd
    results = boot(data = tmpboot, statistic = vd.mean, R = 1000)
    ci.results = boot.ci(results, type = 'bca') 
    blo.btstrp.fall.avghrly[ihour, 1] = results$t0
    blo.btstrp.fall.avghrly[ihour, 2] = ci.results$bca[4]
    blo.btstrp.fall.avghrly[ihour, 3] = ci.results$bca[5]
  }
}


# ===================================================================================================================================================
# 3. calculate stomatal conductance 
# ===================================================================================================================================================
# load site FLUXNET meteorology
# hf9301.met = read_meteo(casename = 'hf9301w9819930101', dirname = 'shsun/', startyr = 1993, endyr = 2001, utc.adjust = -5) # Harvard Forest
# wu18.met = read_meteo(casename = 'wu18w9820080101', dirname = 'shsun/', startyr = 2008, endyr = 2013, utc.adjust = -5) # Borden Forest
# fare10.met = read_meteo(casename = 'fare10w9820010101', dirname = 'shsun/', startyr = 2001, endyr = 2007, utc.adjust = -8) # Blodgett Forest
# rannik12.met = read_meteo(casename = 'rannik12w9820010101', dirname = 'shsun/', startyr = 2001, endyr = 2009, utc.adjust = 2) # Hyytiala Forest

# --------------------------------- harvard forest ------------------------------------
z.r = 29
z0 = 0.1*z.r
d = 0.7*z.r
hf9301.met$obk = hf9301.met$rs = hf9301.met$ra = hf9301.met$rb = hf9301.met$psi.H = array(NA, dim = dim(hf9301.met$cldtot))
for (iyear in 1:dim(hf9301.met$cldtot)[1]) {
  for (imonth in 1:12) {
    for (iday in 1:30) {
      for (ihour in 1:24) {
        
        hf9301.met$obk[iyear,imonth,iday,ihour] = obk(T.s = hf9301.met$t2m[iyear,imonth,iday,ihour]- 273.15, ustar = hf9301.met$ustar[iyear,imonth,iday,ihour], H = hf9301.met$hflux[iyear,imonth,iday,ihour])
        hf9301.met$psi.H[iyear,imonth,iday,ihour] = psi.H(d = 16.8, L = hf9301.met$obk[iyear,imonth,iday,ihour], z.r = z.r)
        hf9301.met$ra[iyear,imonth,iday,ihour] = r.a(ustar = hf9301.met$ustar[iyear,imonth,iday,ihour], z0 = z0, L = hf9301.met$obk[iyear,imonth,iday,ihour], d = d, psi.H = hf9301.met$psi.H[iyear,imonth,iday,ihour], z.r = z.r)
        hf9301.met$rb[iyear,imonth,iday,ihour] = r.b(ustar = hf9301.met$ustar[iyear,imonth,iday,ihour])
        hf9301.met$rs[iyear,imonth,iday,ihour] = r.s.penman.monteith(p = hf9301.met$slp[iyear,imonth,iday,ihour], t.s = hf9301.met$t2m[iyear,imonth,iday,ihour]- 273.15,H = hf9301.met$hflux[iyear,imonth,iday,ihour],
                                                                     E = hf9301.met$eflux[iyear,imonth,iday,ihour]/2442000, qa = hf9301.met$qv2m[iyear,imonth,iday,ihour], rh = NA, 
                                                                     r.a = hf9301.met$ra[iyear,imonth,iday,ihour], r.b = hf9301.met$rb[iyear,imonth,iday,ihour], ustar = hf9301.met$ustar[iyear,imonth,iday,ihour])
      }
    }
  }
} 

tmp = hf9301.met
tmp$rs[is.infinite(tmp$rs) | tmp$rs < 0 | tmp$rs > 10000] = NA
#quantile(tmp$rs, na.rm = T)
#hf9301.rs.jja.diurnal = apply(tmp$rs[,6:8,,], c(4), na.rm = T, quantile, probs = c(.1, .2, .8, .9))
hf9301.ra.jja.diurnal = apply(tmp$ra[,6:8,,], c(4), mean, na.rm = T, trim = 0.1)
hf9301.rb.jja.diurnal = apply(tmp$rb[,6:8,,], c(4), mean, na.rm = T, trim = 0.1)
hf9301.gc.jja.diurnal = 1/(1/hf.btstrp.summer.avghrly[,1] - hf9301.ra.jja.diurnal - hf9301.rb.jja.diurnal)
hf9301.fgs.jja.diurnal = (1/hf9301.rs.jja.diurnal)/hf9301.gc.jja.diurnal
hf9301.fgs.jja.diurnal[hf9301.fgs.jja.diurnal>1] = 1
hf9301.rs.jja.diurnal = apply(1/tmp$rs[,6:8,,], c(4), mean, na.rm = T, trim = 0.2)
# hf9301.rs.jja.diurnal.sd = apply(1/tmp$rs[,6:8,,], c(4), sd_trim, trim = 0.25, const = F)
hf9301.rs.jja.diurnal.sd = apply(1/tmp$rs[,6:8,,], c(4), sd.trim, trim = 0.2, na.rm = T)
hf9301.rs.jja.diurnal.sd = data.frame(hour = 0:23, sd = hf9301.rs.jja.diurnal.sd, rs = hf9301.rs.jja.diurnal)

# --------------------------------- blodgett forest ------------------------------------
z.r = 12.5
z0 = 0.1*z.r
d = 0.7*z.r
fare10.met$obk = fare10.met$rs = fare10.met$ra = fare10.met$psi.H = fare10.met$rb = array(NA, dim = dim(fare10.met$cldtot))
for (iyear in 1:dim(fare10.met$cldtot)[1]) {
  for (imonth in 1:12) {
    for (iday in 1:30) {
      for (ihour in 1:24) {
        fare10.met$obk[iyear,imonth,iday,ihour] = obk(T.s = fare10.met$t2m[iyear,imonth,iday,ihour]- 273.15, ustar = fare10.met$ustar[iyear,imonth,iday,ihour], H = fare10.met$hflux[iyear,imonth,iday,ihour])
        fare10.met$psi.H[iyear,imonth,iday,ihour] = psi.H(d = d, L = fare10.met$obk[iyear,imonth,iday,ihour], z.r = z.r)
        fare10.met$ra[iyear,imonth,iday,ihour] = r.a(ustar = fare10.met$ustar[iyear,imonth,iday,ihour], z0 = z0, L = fare10.met$obk[iyear,imonth,iday,ihour], d = d, psi.H = fare10.met$psi.H[iyear,imonth,iday,ihour], z.r = z.r)
        fare10.met$rb[iyear,imonth,iday,ihour] = r.b(ustar = fare10.met$ustar[iyear,imonth,iday,ihour])
        fare10.met$rs[iyear,imonth,iday,ihour] = r.s.penman.monteith(p = fare10.met$slp[iyear,imonth,iday,ihour], t.s = fare10.met$t2m[iyear,imonth,iday,ihour]- 273.15,H = fare10.met$hflux[iyear,imonth,iday,ihour],
                                                                     E = fare10.met$eflux[iyear,imonth,iday,ihour]/2442000, qa = fare10.met$qv2m[iyear,imonth,iday,ihour], rh = NA, 
                                                                     r.a = fare10.met$ra[iyear,imonth,iday,ihour], r.b = fare10.met$rb[iyear,imonth,iday,ihour], ustar = fare10.met$ustar[iyear,imonth,iday,ihour])
      }
    }
  }
} 

tmp = fare10.met
tmp$rs[is.infinite(tmp$rs) | tmp$rs < 0 | tmp$rs > 10000] = NA
fare10.rs.jja.diurnal = apply(1/tmp$rs[,6:8,,], c(4), mean, na.rm = T, trim = 0.2)
fare10.rs.jja.diurnal.sd = apply(1/tmp$rs[,6:8,,], c(4), sd.trim, trim = 0.2, na.rm = T)
fare10.rs.jja.diurnal.sd = data.frame(hour = 0:23, sd = fare10.rs.jja.diurnal.sd, rs = fare10.rs.jja.diurnal)

# --------------------------------- hyytiala forest ---------------------------------
z.r = 23
z0 = 0.1*z.r
d = 0.7*z.r

rannik12.met$obk = rannik12.met$rs = rannik12.met$ra = rannik12.met$psi.H = rannik12.met$rb = array(NA, dim = dim(rannik12.met$cldtot))
for (iyear in 1:dim(rannik12.met$cldtot)[1]) {
  for (imonth in 1:12) {
    for (iday in 1:30) {
      for (ihour in 1:24) {
        
        rannik12.met$obk[iyear,imonth,iday,ihour] = obk(T.s = rannik12.met$t2m[iyear,imonth,iday,ihour]- 273.15, ustar = rannik12.met$ustar[iyear,imonth,iday,ihour], H = rannik12.met$hflux[iyear,imonth,iday,ihour])
        rannik12.met$psi.H[iyear,imonth,iday,ihour] = psi.H(d = d, L = rannik12.met$obk[iyear,imonth,iday,ihour], z.r = z.r)
        rannik12.met$ra[iyear,imonth,iday,ihour] = r.a(ustar = rannik12.met$ustar[iyear,imonth,iday,ihour], z0 = z0, L = rannik12.met$obk[iyear,imonth,iday,ihour], d = d, psi.H = rannik12.met$psi.H[iyear,imonth,iday,ihour], z.r = z.r)
        rannik12.met$rb[iyear,imonth,iday,ihour] = r.b(ustar = rannik12.met$ustar[iyear,imonth,iday,ihour])
        rannik12.met$rs[iyear,imonth,iday,ihour] = r.s.penman.monteith(p = rannik12.met$slp[iyear,imonth,iday,ihour], t.s = rannik12.met$t2m[iyear,imonth,iday,ihour]- 273.15,H = rannik12.met$hflux[iyear,imonth,iday,ihour],
                                                                       E = rannik12.met$eflux[iyear,imonth,iday,ihour]/2442000, qa = rannik12.met$qv2m[iyear,imonth,iday,ihour], rh = NA, 
                                                                       r.a = rannik12.met$ra[iyear,imonth,iday,ihour], r.b = rannik12.met$rb[iyear,imonth,iday,ihour], ustar = rannik12.met$ustar[iyear,imonth,iday,ihour])
      }
    }
  }
} 

tmp = rannik12.met
tmp$rs[is.infinite(tmp$rs) | tmp$rs < 0 | tmp$rs > 10000] = NA
rannik12.rs.jja.diurnal = apply(1/tmp$rs[,6:8,,], c(4), mean, na.rm = T, trim = 0.2)
rannik12.rs.jja.diurnal.sd = apply(1/tmp$rs[,6:8,,], c(4), sd.trim, trim = 0.2, na.rm = T)
rannik12.rs.jja.diurnal.sd = data.frame(hour = 0:23, sd = rannik12.rs.jja.diurnal.sd, rs = rannik12.rs.jja.diurnal)

# --------------------------------- borden forest -------------------------------------

bf08 = data.frame()
for (iyear in 2008:2012) {
  header = read.csv(paste0('~/Documents/FLUXNET_Canada_1335/data/ON-BordenMixedWood/ON-BordenMixedWood_Met/BordenMet_', iyear, '.csv'), skip = 1, header = F, nrows = 1, as.is = T)
  fl = read.csv(paste0('~/Documents/FLUXNET_Canada_1335/data/ON-BordenMixedWood/ON-BordenMixedWood_Met/BordenMet_', iyear, '.csv'), skip = 3, header = F)
  colnames(fl) = header
  
  fl$time = as.POSIXct(fl$EndTime*24*60*60+1, origin = "2008-01-01", tz = "UTC")
  fl$day = format(as.Date(fl$time), "%d")
  fl$month = format(as.Date(fl$time), "%m")
  fl$hour = lubridate::hour(fl$time)
  fl$hm = format(fl$time, "%H:%M")
  
  bf08_met = fl[c('RelHum_33m', 'AirTemp_AbvCnpy_33m', 'SurfPress', 'AirDensity_33m', 'time', 'Year','month', 'hour', 'day','hm')]
  header = read.csv(paste0('~/Documents/FLUXNET_Canada_1335/data/ON-BordenMixedWood/ON-BordenMixedWood_Flux/BordenFlux_',2008,'.csv'), skip = 1, header = F, nrows = 1, as.is = T)
  fl = read.csv(paste0('~/Documents/FLUXNET_Canada_1335/data/ON-BordenMixedWood/ON-BordenMixedWood_Flux/BordenFlux_',2008,'.csv'), skip = 3, header = F)
  colnames(fl) = header
  fl$time = as.POSIXct(fl$EndTime*24*60*60+1, origin = "2008-01-01", tz = "UTC")
  fl$day = format(as.Date(fl$time), "%d")
  fl$month = format(as.Date(fl$time), "%m")
  fl$hour = lubridate::hour(fl$time)
  fl$hm = format(fl$time, "%H:%M")
  
  bf08_flx = fl[c('SonicAirTemp', 'AirDensity_GapF','LatentHtFluxWPL', 'SensHtFlux','U*(rot)', 'time','Year', 'month','hour','day','hm')]
  bf08 = rbind(bf08, merge(bf08_met, bf08_flx, by = c('Year','month','hour', 'day','hm')))
}

bf08[bf08 == -999] = NA
bf08[bf08 == Inf] = NA
bf08 = bf08[complete.cases(bf08),]

z.r = 22
z0 = 0.1*z.r
d = 0.7*z.r

bf08$obk = mapply(obk, T.s = bf08$AirTemp_AbvCnpy_33m, ustar = bf08$`U*(rot)`, H = bf08$SensHtFlux)
bf08$psi.H = mapply(psi.H, d = d, L = bf08$obk, z.r = z.r)
bf08$ra = mapply(r.a, ustar = bf08$`U*(rot)`, z0 = z0, d = d, L = bf08$obk, psi.H =  bf08$psi.H, z.r = z.r) 
# bf08$rb = mapply(r.b, bf08$AirTemp_AbvCnpy_33m + 273.15, bf08$SurfPress*100, 48E-3, bf08$`U*(rot)`)
# bf08$rs = r.s.penman.monteith(p = bf08$SurfPress*100, t.s = bf08$AirTemp_AbvCnpy_33m+273.15, H = bf08$SensHtFlux, E = bf08$LatentHtFluxWPL/2442000, rh = bf08$RelHum_33m*0.01,
#                               r.a = bf08$ra, r.b = bf08$rb, ustar = bf08$`U*(rot)`)
bf08$rb = mapply(r.b, ustar = bf08$`U*(rot)`)
bf08$rs = mapply(r.s.penman.monteith, p = bf08$SurfPress*100, t.s = bf08$AirTemp_AbvCnpy_33m,
                 H = bf08$SensHtFlux, E = bf08$LatentHtFluxWPL/2442000, rh = 0.01*bf08$RelHum_33m,
                 r.a = bf08$ra, r.b = bf08$rb, ustar = bf08$`U*(rot)`)

# summertime average diurnal cycles
bf08$month = as.numeric(bf08$month)

test = bf08[which(bf08$month <= 9 & bf08$month >= 6 & bf08$rs > 0 & bf08$rs < 5000), ]
test = bf08[which(bf08$month <= 9 & bf08$month >= 6 & bf08$rs > 0 & bf08$rs < 10000), ]

test = bf08[which(bf08$month <= 9 & bf08$month >= 6), ]
quantile(test$rs, na.rm = T)

bf08_jjas_avghrly = aggregate(1/test$rs, by = list(test$hour), mean, na.rm = T, trim = 0.2)
bf08_jjas_avghrly_sd = aggregate(1/test$rs, by = list(test$hour), sd.trim, na.rm = T, trim = 0.2)
#bf08_jjas_avghrly_sd$x = 1/bf08_jjas_avghrly_sd$x

colnames(bf08_jjas_avghrly_sd) = c('hour','sd')
colnames(bf08_jjas_avghrly) = c('hour','rs')
bf08_jjas_avghrly$hour = bf08_jjas_avghrly_sd$hour = c(18:23,0:17)
bf08_jjas_avghrly = bf08_jjas_avghrly[order(bf08_jjas_avghrly$hour),] 
bf08_jjas_avghrly_sd = bf08_jjas_avghrly_sd[order(bf08_jjas_avghrly_sd$hour),] 
bf08_jjas_avghrly_sd = merge(bf08_jjas_avghrly, bf08_jjas_avghrly_sd, by = c('hour'))


## daytime monthly ra, rb, rs

# harvard 
# tmp = hf9301.met
# tmp$rs[is.infinite(tmp$rs) | tmp$rs < 0 | tmp$rs > 10000] = NA
# hf9301.rs.day.mly = apply(tmp$rs[,,,10:16], c(2), mean, na.rm = T, trim = 0.05)
# f_plot_monthly_lt(case = hf9301.mean.day.mly, to_plot_var = 'rs', to_plot_obs = 1/hf9301.rs.day.mly,  yaxis_name = bquote(G[s]~ m ~s^-1))

tmp = hf9301.met
tmp$rs[is.infinite(tmp$rs)] = NA
tmp$rs[tmp$rs < 0] = NA
hf9301.rs.day.mly = apply(tmp$rs[,,,10:16], c(2), mean, na.rm = T, trim = 0.05)

hf9301.rs.mly = apply(tmp$rs, c(2), mean, na.rm = T, trim = 0.05)

# borden forest
test = bf08[which(bf08$rs > 0 & bf08$rs < 10000 & bf08$hour <= 21 & bf08$hour >= 15), ] #? utc 
bf08.rs.day.mly = aggregate(test$rs, by = list(test$month), mean, na.rm = T, trim = 0.05)
test = bf08[which(bf08$rs > 0 & bf08$rs < 10000), ]
bf08.rs.mly = aggregate(test$rs, by = list(test$month), mean, na.rm = T, trim = 0.1)

# hyytiala 
tmp = rannik12.met
tmp$rs[is.infinite(tmp$rs) | tmp$rs < 0 | tmp$rs > 10000] = NA
rannik12.rs.day.mly = apply(tmp$rs[,,,10:16], c(2), mean, na.rm = T, trim = 0.05)
rannik12.rs.mly = apply(tmp$rs, c(2), mean, na.rm = T, trim = 0.05)

# blodgett 
tmp = fare10.met
tmp$rs[is.infinite(tmp$rs) | tmp$rs < 0 | tmp$rs > 10000] = NA
fare10.rs.day.mly = apply(tmp$rs[,,,10:16], c(2), mean, na.rm = T, trim = 0.05)
fare10.rs.mly = apply(tmp$rs, c(2), mean, na.rm = T, trim = 0.05)

# ===================================================================================================================================================
#  plot
# ===================================================================================================================================================


f_plot_monthly_lt(case = hf9301.mean.day.mly, to_plot_var = 'rs', to_plot_obs = 1/hf9301.rs.day.mly,  yaxis_name = bquote(G[s]~ m ~s^-1))
f_plot_monthly_lt(case = rannik12.mean.day.mly, to_plot_var = 'rs', to_plot_obs = 1/rannik12.rs.day.mly,  yaxis_name = bquote(G[s]~ m ~s^-1))
f_plot_monthly_rs(case = fare10.mean.day.mly, to_plot_var = 'rs', to_plot_obs = 1/fare10.rs.day.mly,  yaxis_name = bquote(G[s]~ m ~s^-1))
f_plot_diurnal_rs(case = wu18.jjas.avghrly, to_plot_var = 'rs',  to_plot_obs =  bf08_jjas_avghrly$rs, ylim_high = 0.015, ylim_low = -0.002, obs_sd = bf08_jjas_avghrly_sd)





