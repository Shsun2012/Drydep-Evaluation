# ========================================================
# functions used in obsdata_plot.R 
# ========================================================
source('~/Dropbox/GitHub/Drydep-Evaluation/geophys_const.R')
library(fields)
library(maps)
library(abind)
library(ncdf4)
library(parallel)
library(lubridate)
library(QuantPsyc)
library(zoo)
library(tidyr)
library(plyr)
library(data.table)
library(boot)
library(ggpubr)

plotModObs= function(seasonl, modlist, obslist, avg, daytime = F, title, night = F, to_avg_hour = 1:24, bootstrap = F, 
                     y_lim = 1.2, axis_break = c(0,0.4,0.8,1.2), title_x = 0.2, errorbar = T, y_adjust = 0.3, point_size = 4.5, text_size = 7, toAvgHour = 10:16){
  require(ggpubr)
  modlist = lapply(modlist, "*", 100)  # convert from m to cm
  if (night) {
    to_avg_hour = c(22:24,1:4)
  }
  if (daytime == T) {
    to_avg_hour = toAvgHour
  }
  
  if (avg == T) {  # multiple datasets
    df = data.frame(cbind(season = rep(seasonl[[1]], 1), scale = rep('hourly', 1), mod = mean(modlist[[1]][to_avg_hour],na.rm=T), obs = mean(obslist[[1]][to_avg_hour],na.rm=T), modsd = sd(modlist[[1]][to_avg_hour],na.rm=T), obssd = sd(obslist[[1]][to_avg_hour],na.rm=T), modci1 = NA, modci2 = NA, obsci1 = NA, obsci2 = NA)) # m/s -> cm/s
    df$modci1 = as.numeric(levels(df$modci1))[df$modci1]
    df$obsci1 = as.numeric(levels(df$obsci1))[df$obsci1]
    df$modci2 = as.numeric(levels(df$modci2))[df$modci2]
    df$obsci2 = as.numeric(levels(df$obsci2))[df$obsci2]
    if (bootstrap) {
      tmpboot = boot(data = modlist[[1]][to_avg_hour], statistic = vd.mean, R = 1000)
      tmp.result = boot.ci(tmpboot, type = 'bca')
      df$mod[1] = tmpboot$t0
      df$modci1[1] = tmp.result$bca[4]
      df$modci2[1] = tmp.result$bca[5]
      tmpboot = boot(data = obslist[[1]][to_avg_hour], statistic = vd.mean, R = 1000)
      tmp.result = boot.ci(tmpboot, type = 'bca')
      df$obs[1] = tmpboot$t0
      df$obsci1[1] = tmp.result$bca[4]
      df$obsci2[1] = tmp.result$bca[5]
    }
    for (i in 2:length(modlist)) {
      df = rbind(df, data.frame(cbind(season = rep(seasonl[[i]], 1), scale = rep('hourly', 1), mod = mean(modlist[[i]][to_avg_hour],na.rm=T), obs = mean(obslist[[i]][to_avg_hour],na.rm=T), modsd = sd(modlist[[i]][to_avg_hour],na.rm=T), obssd = sd(obslist[[i]][to_avg_hour],na.rm=T), modci1 = NA, modci2 = NA, obsci1 = NA, obsci2 = NA)))
      if (bootstrap) {
        df$modci1[i] = as.numeric(levels(df$modci1))[df$modci1][i]
        df$obsci1[i] = as.numeric(levels(df$obsci1))[df$obsci1][i]
        df$modci2[i] = as.numeric(levels(df$modci2))[df$modci2][i]
        df$obsci2[i] = as.numeric(levels(df$obsci2))[df$obsci2][i]
        tmpboot = boot(data = modlist[[i]][to_avg_hour], statistic = vd.mean, R = 1000)
        tmp.result = boot.ci(tmpboot, type = 'bca')
        df$mod[i] = tmpboot$t0
        df$modci1[i] = tmp.result$bca[4]
        df$modci2[i] = tmp.result$bca[5]
        if (!any(is.na(obslist[[i]][to_avg_hour])) & !all(obslist[[i]][to_avg_hour] == rev(obslist[[i]][to_avg_hour]))) {
          tmpboot = boot(data = obslist[[i]][to_avg_hour], statistic = vd.mean, R = 1000)
          tmp.result = boot.ci(tmpboot, type = 'bca')
          df$obs[i] = tmpboot$t0
          df$obsci1[i] = tmp.result$bca[4]
          df$obsci2[i] = tmp.result$bca[5]
        }else{
          df$obs[i] = mean(obslist[[i]][to_avg_hour], na.rm = T)
          df$obsci1[i] = df$obsci2[i] = df$obs[i]
        }
        
      }
    }
    #colnames(df) = c('season','scale','mod','obs','modsd','obssd')
    df$mod = as.numeric(levels(df$mod))[df$mod]
    df$obs = as.numeric(levels(df$obs))[df$obs]
    df$modsd = as.numeric(levels(df$modsd))[df$modsd]
    df$obssd = as.numeric(levels(df$obssd))[df$obssd]
    
    for (i in 1:length(modlist)) {
      if ((df$mod[i] - df$modsd[i])<=0) {
        df$modsd[i] = df$mod[i]
      }
      if ((df$mod[i]+df$modsd[i])>1.2) {
        df$modsd[i] = 1.2 - df$mod[i]
      }
      if ((df$obs[i]-df$obssd[i])<=0) {
        df$obssd[i] = df$obs[i]
      }
      if ((df$obs[i]+df$obssd[i])>1.2) {
        df$obssd[i] = 1.2 - df$obs[i]
      }
    }
    ### quantitative metrics
    nmbf = calNMBF(df$mod, df$obs)
    nmaef = calNMAEF(df$mod, df$obs)
    mbe = model.bias.error(df$obs, df$mod)
    r = cor(df$obs, df$mod, use = "pairwise.complete.obs")
    
    #### calculate rsquared
    tmp.lm = lm(mod ~ obs, data = df)
    rsquare = format(round(summary(tmp.lm)$r.squared, 2),nsmall = 2)
    
    p <-ggplot(df, aes(x = obs, y = mod, color = season))+
      geom_abline(slope = 1, size = 2)+
      geom_point(size=point_size)+
      geom_errorbar(aes(ymin=mod-modsd, ymax=mod+modsd), width = .03)+
      geom_errorbarh(aes(xmin=obs-obssd, xmax=obs+obssd),height = .03)+
      scale_x_continuous(limits = c(0,y_lim), breaks = axis_break)+
      scale_y_continuous(limits = c(0,y_lim), breaks = axis_break)+
      xlab(bquote('Observed'~O[3]~ ~V[d]~ cm ~s^-1))+
      ylab(bquote('Modelled'~O[3]~ ~V[d]~ cm ~s^-1))+
      #labs(x = "Observed Deposition velocity", y = "Simulated Deposition velocity")+
      #ggtitle(paste0(title))+
      theme_bw()+
      theme(axis.text.x = element_text(colour = "grey20",size=15),
            axis.text.y = element_text(colour = "grey20",size=15))+
      theme(axis.title = element_text(face = "bold", size = 16))+
      #theme(plot.title = element_text(face = "bold", size = 16))+
      theme(legend.text = element_text(size = 20), legend.title = element_blank())+
      annotate("text", x = c(title_x,y_lim-y_adjust,y_lim-y_adjust), y = c(y_lim-0.1,0.1,0.3), 
               label = c(title,paste0("NMBF=",format(round(nmbf, digits = 2), nsmall = 2)), paste0("NMAEF=",format(round(nmaef, digits = 2), nsmall = 2))), size = c(7,5.5,5.5))
    
    if (bootstrap) {
      df$modci1 = as.numeric(df$modci1)
      df$modci2 = as.numeric(df$modci2)
      df$obsci1 = as.numeric(df$obsci1)
      df$obsci2 = as.numeric(df$obsci2)
      p <-ggplot(df, aes(x = obs, y = mod, color = season))+
        geom_abline(slope = 1, size = 2)+
        geom_point(size=point_size)+
        geom_errorbar(data = df, aes(ymin=modci1, ymax=modci2), width = .03)+
        geom_errorbarh(aes(xmin=obsci1, xmax=obsci2),height = .03)+
        scale_x_continuous(limits = c(0,y_lim), breaks = axis_break)+
        scale_y_continuous(limits = c(0,y_lim), breaks = axis_break)+
        xlab(bquote('Observed'~O[3]~ ~V[d]~ cm ~s^-1))+
        ylab(bquote('Modelled'~O[3]~ ~V[d]~ cm ~s^-1))+
        #labs(x = "Observed Deposition velocity", y = "Simulated Deposition velocity")+
        #ggtitle(paste0(title))+
        theme_bw()+
        theme(axis.text.x = element_text(colour = "grey20",size=15),
              axis.text.y = element_text(colour = "grey20",size=15))+
        theme(axis.title = element_text(face = "bold", size = 16))+
        #theme(plot.title = element_text(face = "bold", size = 16))+
        theme(legend.text = element_text(size = 20), legend.title = element_blank())+
        annotate("text", x = c(title_x,y_lim-y_adjust,y_lim-y_adjust), y = c(y_lim-0.1,0.1,0.3), 
                 label = c(title,paste0("NMBF=",format(round(nmbf, digits = 2), nsmall = 2)), paste0("NMAEF=",format(round(nmaef, digits = 2), nsmall = 2))), size = c(7,5.5,5.5))
    }
    if (!errorbar) {
      p <-ggplot(df, aes(x = obs, y = mod, color = season, shape = season))+
        geom_abline(slope = 1, size = 2)+
        geom_point(size=point_size)+
        #geom_point(size=point_size-2, colour = 'white')+
        scale_shape_manual(values = c(15,16,17,18))+
        scale_x_continuous(limits = c(0,y_lim), breaks = axis_break)+
        scale_y_continuous(limits = c(0,y_lim), breaks = axis_break)+
        xlab(bquote('Observed'~O[3]~ ~V[d]~ cm ~s^-1))+
        ylab(bquote('Modelled'~O[3]~ ~V[d]~ cm ~s^-1))+
        #labs(x = "Observed Deposition velocity", y = "Simulated Deposition velocity")+
        #ggtitle(paste0(title))+
        theme_bw()+
        theme(axis.text.x = element_text(colour = "grey20",size=15),
              axis.text.y = element_text(colour = "grey20",size=15))+
        theme(axis.title = element_text(face = "bold", size = 16))+
        #theme(plot.title = element_text(face = "bold", size = 16))+
        theme(legend.text = element_text(size = 20), legend.title = element_blank())+
        annotate("text", x = c(title_x,y_lim-y_adjust,y_lim-y_adjust), y = c(y_lim-0.1,0.1,0.3),
                 label = c(title,paste0("NMBF=",format(round(nmbf, digits = 2), nsmall = 2)), paste0("NMAEF=",format(round(nmaef, digits = 2), nsmall = 2))), size = c(7,5.5,5.5))
      #annotate("text", x = c(title_x,y_lim-y_adjust,y_lim-y_adjust,y_lim-y_adjust), y = c(y_lim-0.1,0.1,0.3,0.5), 
      #          label = c(title, paste0("R=",format(round(r, digits = 2), nsmall = 2)),paste0("NMBF=",format(round(nmbf, digits = 2), nsmall = 2)),
      #          paste0("NMAEF=",format(round(nmaef, digits = 2), nsmall = 2))), size = text_size)
      # annotate("text", x = c(title_x,y_lim-y_adjust,y_lim-y_adjust,y_lim-y_adjust), y = c(y_lim-0.1,0.1,0.3,0.5), 
      #        label = c(title, as.expression(bquote(R^2 == .(rsquare))),paste0("NMBF=",format(round(nmbf, digits = 2), nsmall = 2)),
      #                  paste0("NMAEF=",format(round(nmaef, digits = 2), nsmall = 2))), size = text_size)
      
    }
    return(p)   
    
  }else{ ## avg = F ------------------------------------------------------------------------------------------------------------------
    if (daytime == F) {
      df = data.frame(cbind(rep(seasonl[[1]], 24), rep('hourly', 24), modlist[[1]], obslist[[1]])) # m/s -> cm/s
      for (i in 2:length(modlist)) {
        df = rbind(df, data.frame(cbind(rep(seasonl[[i]], 24), rep('hourly', 24), modlist[[i]], obslist[[i]])))
      }
    }else{
      df = data.frame(cbind(rep(seasonl[[1]], 7), rep('hourly', 7), modlist[[1]][10:16], obslist[[1]][10:16])) # m/s -> cm/s
      for (i in 2:length(modlist)) {
        df = rbind(df, data.frame(cbind(rep(seasonl[[i]], 7), rep('hourly', 7), modlist[[i]][10:16], obslist[[i]][10:16])))
      }
    }
    colnames(df) = c('season','scale','mod','obs')
    df$mod = as.numeric(levels(df$mod))[df$mod]
    df$obs = as.numeric(levels(df$obs))[df$obs]
    
    ### quantitative metrics
    nmbf = calNMBF(df$mod, df$obs)
    nmaef = calNMAEF(df$mod, df$obs)
    mbe = model.bias.error(df$obs, df$mod)
    r = cor(df$obs, df$mod, use = "pairwise.complete.obs")
    
    p <- ggplot(df, aes(x = obs, y = mod, color = season))+
      geom_abline(slope = 1, size = 2)+
      geom_point(size=3.5)+
      scale_x_continuous(limits = c(0,1.2), breaks = c(0,0.4,0.8,1.2))+
      scale_y_continuous(limits = c(0,1.2), breaks = c(0,0.4,0.8,1.2))+
      labs(x = "Observed Deposition velocity", y = "Simulated Deposition velocity")+
      ggtitle(paste0(title))+
      theme_bw()+
      theme(axis.text.x = element_text(colour = "grey20",size=15),
            axis.text.y = element_text(colour = "grey20",size=15))+
      theme(axis.title = element_text(face = "bold", size = 16))+
      theme(plot.title = element_text(face = "bold", size = 16))+
      # theme(legend.text = element_text(size = 15), legend.title = element_blank())+
      annotate("text", x = title_x, y = c(1.1,1.2), 
               label = c(paste0("NMBF=",format(round(nmbf, digits = 2), nsmall = 2)), paste0("NMAEF=",format(round(nmaef, digits = 2), nsmall = 2))), size = 7)
    
    return(list(p))   
  }
}

interp_lai = function(iyear, df){ # linearly interpolate mean deciduous lai 
  tmp = complete(df[which(df$year == iyear),], doy = full_seq(doy, 1))
  tmp$decid.mean = na.approx(tmp$decid.mean)
  tmp$year = iyear
  return(tmp)
}

f_esat = function(T_C, derivative=FALSE) {
  # This function calculates the saturation water vapor pressure (Pa) for a given temperature "T_K" (K) or a vector/matrix of "T_K" (K) based on Lowe and Ficke (1974).
  #T_C = T_K - 273.15			# Convert K to degC.
  a0 = 6.107799961
  a1 = 4.436518521e-1
  a2 = 1.428945805e-2
  a3 = 2.650648471e-4
  a4 = 3.031240396e-6
  a5 = 2.034080948e-8
  a6 = 6.136820929e-11
  if (derivative) {
    desat_dT = (a1 + 2*a2*T_C + 3*a3*T_C^2 + 4*a4*T_C^3 + 5*a5*T_C^4 + 6*a6*T_C^5)*100
    return(desat_dT)
  } else {
    esat = (a0 + a1*T_C + a2*T_C^2 + a3*T_C^3 + a4*T_C^4 + a5*T_C^5 + a6*T_C^6)*100
    return(esat)
  }
}

mix_ratio = function(temp, rh, p){
  if (is.na(p)) {
    p = 98624
  }
  e = rh/100 * f_esat(temp)
  r = 0.622*e/(p-e)
  return(r)
}

obk = function(T.s, ustar, H){ # virtual temperature
  rho = 1.1839
  c_p = 1010
  if (!is.na(H)) {
    den = von_Kar * g_E * H   # denominator
    if (abs(den) > 0) {
      OBK = - rho* c_p * (T.s + 273.15)* ustar^3 / von_Kar / g_E / H  ## positive -> stable
    }else{
      OBK = 1E5
    }
    return(OBK)
  }else{
    return(NA)
  }
}

psi.H = function(d, L, height = 29){ # stability function 
  if (!is.na(L)) {
    tmp = (height- d)/L
    if (tmp < 0 && tmp > -2) {
      psi = 2 * log(0.5 + 0.475 * (1 - 11.6 * tmp)^0.5)
      return(psi)
    }else if (tmp > 0 && tmp < 1) {
      #psi = - 7.8 * tmp
      psi = 1 - (1 + 0.667*tmp)^(3/2) - 0.667*(tmp - 5/0.35)*exp(-0.35*tmp) - 0.667*5/0.35
      return(psi)
    }else{
      return(NA)
    }
  }else{
    return(NA)
  }
}

v.d = function(p, fo3, c.o3, T.s){  # ozone deposition velocity
  # c.o3: o3.mlb, inferred concentration from fast response ozone detector (unit: partsPerBillion)
  # fo3: f.o3, ozone eddy covariance flux (unit: micromolePerMeterSquaredPerHour)
  M_da = 28.966e-3		   # Molar mass of dry air (kg mol^-1)
  rho = 1.1839/M_da # air density at 1010hPa (mol/m-3)
  k_B = 1.38065e-23	# Boltzmann constant (J K^-1)
  N_A = 6.02214e23 	# Avogadro constant (molecules mol^-1)
  R_uni = k_B*N_A		# Universal gas constant (J K^-1 mol^-1)
  if (!is.na(T.s)) {
    rho = p / R_uni / (T.s + 273.15) # mol/m3
  }
  if (!is.na(fo3) & !is.na(c.o3)) {
    vd = - fo3*1000/(rho*c.o3*3600) # m/s
    return(vd)
  }else{
    return(NA)
  }
}

r.a = function(z0, ustar, d, L, psi.H, height = 29){ 
  if (!is.na(psi.H)) {
    ra = (log((height - d)/z0) - psi.H * (height - d) / L)/(von_Kar * ustar)
    return(ra)
  }else{
    return(NA)
  }
}

r.b = function(ustar){  ### quasi-laminar layer resistance for ozone
  kappa = 0.2  # thermal diffusivity of air (0.2 cm^2 s^-1)
  D.o3 = 0.13 # diffusivity of ozone in air (0.13 cm^2 s^-1 )
  if (!is.na(ustar)) {
    rb = (2 / (von_Kar * ustar)) * (kappa / D.o3) ^ 0.667
    return(rb)
  }else{
    return(NA)
  }
}

## evaporative resistance form of Penman-Monteith equation
r.s.penman.monteith = function(p, t.s, H, E, qa = NA, rh = NA, r.a, r.b, ustar){ ## evaporative resistance form of Penman-Monteith equation
  epsilon = 0.622 # molar mass ratio of water and dry air
  rho = 1.1839 # the mass density of air kg/m3
  kappa = 0.2  # thermal diffusivity of air (cm^2 s^-1)
  D.o3 = 0.13 # diffusivity of ozone in air (cm^2 s^-1 )
  D.h2o = 0.22 # diffusivity of water in air 
  c.p = 1010 # specific heat of air 
  r.b.h = (2 / (von_Kar * ustar))  ## quasi-laminar layer resistance to heat
  if (is.na(t.s) | is.na(p) | is.na(E) | is.na(r.a) | is.na(r.b) | is.na(ustar)) {
    return(NA)
  }
  
  r.b.w = (2 / (von_Kar * ustar)) * ( kappa / D.h2o) ^ 0.667 ## quasi-laminar layer resistance to water vapor
  ## leaf temperature
  t.f = t.s + (r.a + r.b.h)*H/(c.p*rho) ## r.b is quasi-laminar layer resistance to heat
  if (is.na(qa)) {
    e = rh * f_esat(t.f)
  }else{
    e = p*qa/0.622
  }

  r.sw = epsilon * rho * (f_esat(t.f) - e)/(p * E) - (r.a + r.b.w)  ## evaporative-resistance 
  r.s = r.sw*1.61  
  
  return(r.s)   
}

sd.trim <- function(x, trim=0, na.rm=FALSE)
{
  if(!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    warning("argument is not numeric or logical: returning NA")
    return(NA_real_)
  }
  if(na.rm) x <- x[!is.na(x)]
  if(!is.numeric(trim) || length(trim) != 1)
    stop("'trim' must be numeric of length one")
  n <- length(x)
  if(trim > 0 && n > 0) {
    if(is.complex(x)) stop("trimmed sd are not defined for complex data")
    if(trim >= 0.5) return(0)
    lo <- floor(n * trim) + 1
    hi <- n + 1 - lo
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
  }
  sd(x)
}

replace.ustar = function(ustar.obs, ustar.cal){
  if (!is.na(ustar.obs)) {
    return(ustar.obs)
  }else{
    return(ustar.cal)
  }
}

convert_hour = function(hour){ # to convert wu et al. borden foerst hour
  if (nchar(hour) == 4) {
    return(substr(hour,0,1))  
  }else{
    return(substr(hour,0,2))
  }
}

convert_na = function(input){ # mark NA values
  if (input < -10) {
    return(NA)
  }else{
    return(input)
  }
}

mean_o3 = function(c1,c2){ # to calculate o3 at height 23m from height 33.6m and 16.8m
  return((c1+c2)/2)
}

generate_hour = function(df){
  return(as.double(paste0(df[12], df[13])))
}
generate_day = function(df){
  return(as.double(paste0(df[9], df[10])))
}
generate_month = function(df){
  return(as.double(paste0(df[6], df[7])))
}
generate_year = function(df){
  return(as.double(paste0(df[1], df[2], df[3], df[4])))
}

negativ_vd_filter = function(o3vd){
  if (!is.na(o3vd) & o3vd < 0) {
    return(NA)
  }else{
    return(o3vd)
  }
}

remove_vd_binary = function(o3vd, rank, num){ 
  if (!is.na(o3vd) & rank <= num) {
    return(NA)
  }else if (!is.na(o3vd) & o3vd < 0) {
    return(NA)
  }else{
    return(o3vd)
  }
}


plotBootMonthlyComp = function(modmonthly, obsmonthly, title, ylim = 0.01, boot = T){
  # simulated o3vd
  df = data.frame(month = 1:12, W89 = modmonthly$w98[,1], W89FBB = modmonthly$w98fbb[,1], W89MED = modmonthly$w98med[,1],
                  Z03 = modmonthly$zh[,1], Z03FBB = modmonthly$zhfbb[,1],Z03MED = modmonthly$zhmed[,1])
  df.low = data.frame(month = 1:12, W89 = modmonthly$w98[,2], W89FBB = modmonthly$w98fbb[,2], W89MED = modmonthly$w98med[,2],
                      Z03 = modmonthly$zh[,2], Z03FBB = modmonthly$zhfbb[,2],Z03MED = modmonthly$zhmed[,2])
  
  df.high = data.frame(month = 1:12, W89 = modmonthly$w98[,3], W89FBB = modmonthly$w98fbb[,3], W89MED = modmonthly$w98med[,3],
                       Z03 = modmonthly$zh[,3], Z03FBB = modmonthly$zhfbb[,3],Z03MED = modmonthly$zhmed[,3])
  
  df.low = melt(df.low, id = 'month'); colnames(df.low) = c('month','tag','ci.low')
  df.high = melt(df.high, id = 'month'); colnames(df.high) = c('month','tag', 'ci.high')
  
  df = melt(df, id = 'month'); colnames(df) = c('month','tag','o3vd')
  
  gg = merge(df.low, df.high, by = c('month','tag'))
  gg = merge(gg, df, by = c('month','tag'))
  # -----
  # observed o3vd
  if (boot) {
    df.obs = data.frame(month = 1:12, o3vd = obsmonthly[,1])
    df.low = data.frame(month = 1:12, ci.low = obsmonthly[,2])
    df.high = data.frame(month = 1:12,ci.high = obsmonthly[,3])
    
    gg.obs = merge(df.low, df.high, by = c('month'))
    gg.obs = merge(gg.obs, df.obs, by = c('month'))
    p <- ggplot()+
      geom_line(data=gg, aes(x = month, y = o3vd, group = tag, colour = tag), size = 1.2, linetype='twodash')+
      #geom_point(data=gg, aes(x = month, y = value, group = variable, colour = variable), size = 3)+
      geom_line(data=gg.obs, aes(x = month, y = o3vd), colour = 'black', size = 2)+
      #geom_errorbar(data = gg, aes(x = month, ymin=ci.low, ymax=ci.high, colour = tag), width = .3)+
      #geom_errorbar(data = gg.obs, aes(x = month, ymin=o3vd-sd, ymax=o3vd+sd), width = .3, size = 1.5)+
      geom_ribbon(data = gg.obs, aes(x = month, ymin=ci.low, ymax=ci.high), alpha = 0.3)+
      scale_x_continuous(breaks = seq(1,12,by=1))+
      scale_y_continuous(limits = c(0,ylim))+
      labs(x = "Month")+
      ylab(bquote('Modelled'~O[3]~ ~V[d]~ m ~s^-1))+
      ggtitle(title)+
      scale_colour_brewer(palette = "Set1")+
      theme_bw()+
      theme(axis.text.x = element_text(colour = "grey20",size=15),
            axis.text.y = element_text(colour = "grey20",size=15))+
      theme(axis.title = element_text(face = "bold", size = 16))+
      theme(plot.title = element_text(face = "bold", size = 20))+
      theme(legend.text = element_text(size = 15), legend.title = element_blank())
    
    p
  }else{
    gg.obs = data.frame(month = 1:12, o3vd = obsmonthly[,1], sd = obsmonthly[,2])
    p <- ggplot()+
      geom_line(data=gg, aes(x = month, y = o3vd, group = tag, colour = tag), size = 1.2, linetype='twodash')+
      #geom_point(data=gg, aes(x = month, y = value, group = variable, colour = variable), size = 3)+
      geom_line(data=gg.obs, aes(x = month, y = o3vd), colour = 'black', size = 2)+
      #geom_errorbar(data = gg, aes(x = month, ymin=ci.low, ymax=ci.high, colour = tag), width = .3)+
      #geom_errorbar(data = gg.obs, aes(x = month, ymin=o3vd-sd, ymax=o3vd+sd), width = .3, size = 1.5)+
      geom_ribbon(data = gg.obs, aes(x = month, ymin=o3vd-sd, ymax=o3vd+sd), alpha = 0.3)+
      scale_x_continuous(breaks = seq(1,12,by=1))+
      scale_y_continuous(limits = c(0,ylim))+
      labs(x = "Month")+
      ylab(bquote('Modelled'~O[3]~ ~V[d]~ m ~s^-1))+
      ggtitle(title)+
      scale_colour_brewer(palette = "Set1")+
      theme_bw()+
      theme(axis.text.x = element_text(colour = "grey20",size=15),
            axis.text.y = element_text(colour = "grey20",size=15))+
      theme(axis.title = element_text(face = "bold", size = 16))+
      theme(plot.title = element_text(face = "bold", size = 20))+
      theme(legend.text = element_text(size = 15), legend.title = element_blank())
    
    p
  }
  
  
  return(p)
}


plot_diurnal_obsmod = function(obs, mod, ylim_high = 1.5, ylim_low = 0, title = "", bootstrap = F, to_plot_var = 'vd'){
  
  mod.w98 = mod[[paste0(to_plot_var,'.w98')]]
  mod.w98fbb = mod[[paste0(to_plot_var,'.w98fbb')]]
  mod.w98med = mod[[paste0(to_plot_var,'.w98med')]]
  mod.zh = mod[[paste0(to_plot_var,'.zh')]]
  mod.zhfbb = mod[[paste0(to_plot_var,'.zhfbb')]]
  mod.zhmed = mod[[paste0(to_plot_var,'.zhmed')]]
  
  title.y = ylim_high
  
  if (to_plot_var == 'vd') {
    ylab = bquote('Modelled'~O[3]~ ~V[d]~ m ~s^-1)
  }
  if (to_plot_var == 'ra') {
    ylab = bquote('Modelled' ~R[a]~ m ~s^-1)
  }
  if (to_plot_var == 'rb') {
    ylab = bquote('Modelled' ~R[b]~ m ~s^-1)
  }
  if (to_plot_var == 'rs') {
    ylab = bquote('Modelled' ~G[s]~ m ~s^-1)
  }
  
  if (bootstrap) {
    df = data.frame(hour = 0:23, W89 = mod.w98[,1], W89FBB = mod.w98fbb[,1], W89MED = mod.w98med[,1], 
                    Z03 = mod.zh[,1], Z03FBB = mod.zhfbb[,1], Z03MED = mod.zhmed[,1])
    mod.sd = data.frame(hour = 0:23, W89 = mod.w98[,2], W89FBB = mod.w98fbb[,2], W89MED = mod.w98med[,2], 
                        Z03 = mod.zh[,2], Z03FBB = mod.zhfbb[,2], Z03MED = mod.zhmed[,2])
    gg.sd = melt(mod.sd, id = 'hour')
    colnames(gg.sd)[3] = 'sd'
    gg = melt(df, id = 'hour')
    gg = merge(gg, gg.sd, by = c('hour','variable'))
    
    df.obs = data.frame(hour = 0:23, obsMean = obs[,1])
    gg.obs = melt(df.obs, id = 'hour')
    obs.ci1 = data.frame(hour = 0:23, ci1 = obs[,2])
    obs.ci2 = data.frame(hour = 0:23, ci2 = obs[,3])
    df.ci = merge(obs.ci1, obs.ci2, by = c('hour'))
    #df.ci$hour = as.factor(df.ci$hour)
    
    p <- ggplot()+
      geom_line(data=gg, aes(x = hour, y = value, group = variable, colour = variable), size = 1.2, linetype='twodash')+
      #geom_point(data=gg, aes(x = hour, y = value, group = variable, colour = variable), size = 3)+
      geom_line(data=gg.obs, aes(x = hour, y = value), colour = 'black', size = 2)+
      #geom_errorbar(data = gg, aes(x = hour, ymin=value-sd, ymax=value+sd, colour = variable), width = .5)+
      geom_errorbar(data = df.ci, aes(x = hour, ymin=ci1, ymax=ci2), width = .5, size = 1.2)+
      scale_x_continuous(breaks = seq(0,23,by=2))+
      scale_y_continuous(limits = c(ylim_low,ylim_high))+
      #labs(x = "Hour", y = "O3 Dry Deposition velocity (cm/s)")+
      ylab(ylab)+
      #ggtitle(title)+
      scale_colour_brewer(palette = "Set1")+
      theme_bw()+
      theme(axis.text.x = element_text(colour = "grey20",size=15),
            axis.text.y = element_text(colour = "grey20",size=15))+
      theme(axis.title = element_text(face = "bold", size = 16))+
      annotate("text", x = 3.5, y = title.y, label = title, size = 10, colour = 'grey20')+
      #theme(plot.title = element_text(face = "bold", size = 16))+
      theme(legend.text = element_text(size = 15), legend.title = element_blank())
    
  }else{
    df = data.frame(hour = 0:23, W89 = mod.w98[,1], W89FBB = mod.w98fbb[,1], W89MED = mod.w98med[,1], 
                    Z03 = mod.zh[,1], Z03FBB = mod.zhfbb[,1], Z03MED = mod.zhmed[,1])
    mod.sd = data.frame(hour = 0:23, W89 = mod.w98[,2], W89FBB = mod.w98fbb[,2], W89MED = mod.w98med[,2], 
                        Z03 = mod.zh[,2], Z03FBB = mod.zhfbb[,2], Z03MED = mod.zhmed[,2])
    gg.sd = melt(mod.sd, id = 'hour')
    colnames(gg.sd)[3] = 'sd'
    gg = melt(df, id = 'hour')
    gg = merge(gg, gg.sd, by = c('hour','variable'))
    
    df.obs = data.frame(hour = 0:23, obsMean = obs[,1])
    obs.sd = data.frame(hour = 0:23, obsSd = obs[,2])
    gg.obs.sd = melt(obs.sd, id = 'hour')
    colnames(gg.obs.sd)[3] = 'sd'
    gg.obs = melt(df.obs, id = 'hour')
    gg.obs = merge(gg.obs[c(1,3)], gg.obs.sd[c(1,3)], by = c('hour'))
    p <- ggplot()+
      geom_line(data=gg, aes(x = hour, y = value, group = variable, colour = variable), size = 1.2, linetype='twodash')+
      #geom_point(data=gg, aes(x = hour, y = value, group = variable, colour = variable), size = 3)+
      geom_line(data=gg.obs, aes(x = hour, y = value), colour = 'black', size = 2)+
      #geom_errorbar(data = gg, aes(x = hour, ymin=value-sd, ymax=value+sd, colour = variable), width = .5)+
      #geom_errorbar(data = gg.obs, aes(x = hour, ymin=value-sd, ymax=value+sd), width = .5, size = 1.2)+
      geom_ribbon(data = gg.obs, aes(x = hour, ymin=value-sd, ymax=value+sd), alpha = 0.3)+
      scale_x_continuous(breaks = seq(0,23,by=2))+
      scale_y_continuous(limits = c(ylim_low,ylim_high))+
      #labs(x = "Hour", y = "O3 Dry Deposition velocity (cm/s)")+
      ylab(ylab)+
      #ggtitle(title)+
      scale_colour_brewer(palette = "Set1")+
      theme_bw()+
      theme(axis.text.x = element_text(colour = "grey20",size=15),
            axis.text.y = element_text(colour = "grey20",size=15))+
      theme(axis.title = element_text(face = "bold", size = 16))+
      annotate("text", x = 3.5, y = title.y, label = title, size = 10, colour = 'grey20')+
      #theme(plot.title = element_text(face = "bold", size = 16))+
      theme(legend.text = element_text(size = 15), legend.title = element_blank())
  }
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


# comp_vd_data.R

calNMBF = function(mod, obs){ 
  mod.mean = mean(mod, na.rm = T)
  obs.mean = mean(obs, na.rm = T)
  if ( mod.mean >= obs.mean) {
    return(mod.mean/obs.mean-1)
  }else{
    return(1-obs.mean/mod.mean)
  }
}

calNMAEF = function(mod, obs){
  mod.mean = mean(mod, na.rm = T)
  obs.mean = mean(obs, na.rm = T)
  sum1 = sum(abs(mod - obs), na.rm = T)
  sum2 = sum(obs, na.rm = T)
  sum3 = sum(mod, na.rm = T)
  if ( mod.mean >= obs.mean ) {
    return(sum1/sum2)
  }else{
    return(sum1/sum3)
  }
}

vd.mean = function(data, indices){
  d = data[indices]
  tmp.mean = mean(d, na.rm = T)
  return(tmp.mean)
}


scat_modobs_vd = function(obs, mod, season = "", text_ylim = 0.1, text_x = 1, title_x = 0.2, title_y = 1.1, to_comp_month = 1:12, title = "", abline = T){
  caselist = c('vd.w98','vd.w98fbb','vd.w98med','vd.zh','vd.zhfbb','vd.zhmed')
  title = c('w89','w89fbb','w89med','z03','z03fbb','z03med')
  plot = list()
  for (icase in 1:length(caselist)) {
    df = data.frame(obs = c(100*obs[,to_comp_month,]), mod = c(100*mod[[caselist[icase]]][,to_comp_month,]))
    df$obs = trim.quant(df$obs)
    df = na.omit(df)
    nmbf = calNMBF(df$mod, df$obs)
    nmaef = calNMAEF(df$mod, df$obs)
    mbe = model.bias.error(df$obs, df$mod)
    nme = mbe$NME/100
    nmb = mbe$NMB/100
    rmse = mbe$RMSE
    r = cor(df$obs, df$mod, use = "pairwise.complete.obs")
    #### calculate rsquared
    tmp.lm = lm(mod ~ obs, data = df)
    rsquare = format(round(summary(tmp.lm)$r.squared, 2),nsmall = 2)
    if (abline) {
      plot[[caselist[icase]]] <- ggplot(df, aes(x = obs, y = mod))+
        geom_abline(slope = 1, size = 2)+
        #geom_smooth(method = lm, fullrange = T, se = F)+
        geom_point(size=2.5, alpha = 0.1, color = "blue")+
        scale_x_continuous(limits = c(0,1.2), breaks = c(0,0.4,0.8,1.2))+
        scale_y_continuous(limits = c(0,1.2), breaks = c(0,0.4,0.8,1.2))+
        #labs(x = "Observed Deposition velocity", y = "Simulated Deposition velocity")+
        ylab(bquote('Modelled'~O[3]~ ~V[d]~ cm ~s^-1))+
        xlab(bquote('Observed'~O[3]~ ~V[d]~ cm ~s^-1))+
        #ggtitle(paste0(toupper(title[icase]), " ",season))+
        theme_bw()+
        theme(axis.text.x = element_text(colour = "grey20",size=15),
              axis.text.y = element_text(colour = "grey20",size=15))+
        theme(axis.title = element_text(face = "bold", size = 16))+
        #theme(plot.title = element_text(face = "bold", size = 16))+
        theme(legend.text = element_text(size = 15), legend.title = element_blank())+
        annotate("text", x = title_x, y = title_y, label = toupper(title[icase]), size = 10, colour = 'grey20')+
        annotate("text", x = 1, y = c(text_ylim,text_ylim+0.1,text_ylim+0.2),
                 label = c(paste0("R2=",format(round(rsquare, digits = 2), nsmall = 2)),paste0("NMBF=",format(round(nmbf, digits = 2), nsmall = 2)),
                           paste0("NMAEF=",format(round(nmaef, digits = 2), nsmall = 2))), size = 7)
      # annotate("text", x = text_x, y = c(text_ylim,text_ylim+0.1,text_ylim+0.2, text_ylim+0.3),
      #          label = c(as.expression(bquote(R^2 == .(rsquare))),
      #                    paste0("NMB=",format(round(nmb, digits = 2), nsmall = 2)), 
      #                    paste0("NME=",format(round(nme, digits = 2), nsmall = 2)), 
      #                    paste0("RMSE=",format(round(rmse, digits = 2), nsmall = 2))), size = 7)
    }else{
      plot[[caselist[icase]]] <- ggplot(df, aes(x = obs, y = mod))+
        geom_abline(slope = 1, size = 2)+
        #geom_smooth(method = lm, fullrange = T, se = F)+
        geom_point(size=2.5, alpha = 0.1, color = "blue")+
        scale_x_continuous(limits = c(0,1.2), breaks = c(0,0.4,0.8,1.2))+
        scale_y_continuous(limits = c(0,1.2), breaks = c(0,0.4,0.8,1.2))+
        #labs(x = "Observed Deposition velocity", y = "Simulated Deposition velocity")+
        ylab(bquote('Modelled'~O[3]~ ~V[d]~ cm ~s^-1))+
        xlab(bquote('Observed'~O[3]~ ~V[d]~ cm ~s^-1))+
        #ggtitle(paste0(toupper(title[icase]), " ",season))+
        theme_bw()+
        theme(axis.text.x = element_text(colour = "grey20",size=15),
              axis.text.y = element_text(colour = "grey20",size=15))+
        theme(axis.title = element_text(face = "bold", size = 16))+
        #theme(plot.title = element_text(face = "bold", size = 16))+
        theme(legend.text = element_text(size = 15), legend.title = element_blank())+
        annotate("text", x = title_x, y = title_y, label = toupper(title[icase]), size = 10, colour = 'grey20')+
        annotate("text", x = 1, y = c(text_ylim+0.1,text_ylim+0.2),
                 label = c(paste0("NMBF=",format(round(nmbf, digits = 2), nsmall = 2)),
                           paste0("NMAEF=",format(round(nmaef, digits = 2), nsmall = 2))), size = 7)
      
      # annotate("text", x = text_x, y = c(text_ylim,text_ylim+0.1,text_ylim+0.2, text_ylim+0.3),
      #          label = c(as.expression(bquote(R^2 == .(rsquare))),
      #                    paste0("NMB=",format(round(nmb, digits = 2), nsmall = 2)), 
      #                    paste0("NME=",format(round(nme, digits = 2), nsmall = 2)), 
      #                    paste0("RMSE=",format(round(rmse, digits = 2), nsmall = 2))), size = 7)
    }
    
  }
  require(ggpubr)
  ggarrange(plot$vd.w98, plot$vd.w98fbb, plot$vd.w98med, plot$vd.zh, plot$vd.zhfbb, plot$vd.zhmed, nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
}







# ----- rs ra rb ------
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


f_plot_diurnal_frac = function(case, to_plot_var = 'rs', to_plot_var2 = 'gc', to_plot_obs, yaxis_name = '', ylim_high = 0.01){
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


# ------- gs -------

f_esat = function(T_C, derivative=FALSE) {
  # This function calculates the saturation water vapor pressure (Pa) for a given temperature "T_K" (K) or a vector/matrix of "T_K" (K) based on Lowe and Ficke (1974).
  #T_C = T_K - 273.15			# Convert K to degC.
  a0 = 6.107799961
  a1 = 4.436518521e-1
  a2 = 1.428945805e-2
  a3 = 2.650648471e-4
  a4 = 3.031240396e-6
  a5 = 2.034080948e-8
  a6 = 6.136820929e-11
  if (derivative) {
    desat_dT = (a1 + 2*a2*T_C + 3*a3*T_C^2 + 4*a4*T_C^3 + 5*a5*T_C^4 + 6*a6*T_C^5)*100
    return(desat_dT)
  } else {
    esat = (a0 + a1*T_C + a2*T_C^2 + a3*T_C^3 + a4*T_C^4 + a5*T_C^5 + a6*T_C^6)*100
    return(esat)
  }
}

### compute obukhov length and correction function
obk = function(T.s, ustar, H){ # virtual temperature
  rho = 1.1839
  c_p = 1010
  if (!is.na(H)) {
    den = von_Kar * g_E * H   # denominator
    if (abs(den) > 0) {
      OBK = - rho* c_p * (T.s + 273.15)* ustar^3 / von_Kar / g_E / H  ## positive -> stable
    }else{
      OBK = 1E5
    }
    return(OBK)
  }else{
    return(NA)
  }
}

psi.H = function(d, L, height = 29){ # stability function 
  if (!is.na(L)) {
    tmp = (height- d)/L
    if (tmp < 0 && tmp > -2) {
      psi = 2 * log(0.5 + 0.475 * (1 - 11.6 * tmp)^0.5)
      return(psi)
    }else if (tmp > 0 && tmp < 1) {
      #psi = - 7.8 * tmp
      psi = 1 - (1 + 0.667*tmp)^(3/2) - 0.667*(tmp - 5/0.35)*exp(-0.35*tmp) - 0.667*5/0.35
      return(psi)
    }else{
      return(NA)
    }
  }else{
    return(NA)
  }
}

# without combine ozone profile
v.d = function(p, fo3, c.o3){  # ozone deposition velocity
  rho = 1.1839
  c_p = 1010
  if (!is.na(fo3) & !is.na(c.o3)) {
    o3 = c.o3 / 1000
    #rho = p / R_uni / (T.s + 273.15) # mol/m3
    vd = - fo3 * M_da / ( rho * o3 * 3600) # m/s
    return(vd)
  }else{
    return(NA)
  }
}

r.a = function(z0, ustar, d, L, psi.H, height = 29){
  if (!is.na(psi.H)) {
    ra = (log((height - d)/z0) - psi.H * (height - d) / L)/(von_Kar * ustar)
    return(ra)
  }else{
    return(NA)
  }
}

r.b = function(ustar){  ### quasi-laminar layer resistance for ozone
  kappa = 0.2  # thermal diffusivity of air (0.2 cm^2 s^-1)
  D.o3 = 0.13 # diffusivity of ozone in air (0.13 cm^2 s^-1 )
  if (!is.na(ustar)) {
    rb = (2 / (von_Kar * ustar)) * (kappa / D.o3) ^ 0.667
    return(rb)
  }else{
    return(NA)
  }
}

r.s.penman.monteith = function(p, t.s, H, E, qa = NA, rh = NA, r.a, r.b, ustar){ ## evaporative resistance form of Penman-Monteith equation
  epsilon = 0.622 # molar mass ratio of water and dry air
  rho = 1.1839 # the mass density of air kg/m3
  kappa = 0.2  # thermal diffusivity of air (cm^2 s^-1)
  D.o3 = 0.13 # diffusivity of ozone in air (cm^2 s^-1 )
  D.h2o = 0.22 # diffusivity of water in air 
  c.p = 1010 # specific heat of air 
  r.b.h = (2 / (von_Kar * ustar))  ## quasi-laminar layer resistance to heat
  if (is.na(t.s) | is.na(p) | is.na(E) | is.na(r.a) | is.na(r.b) | is.na(ustar)) {
    return(NA)
  }
  
  r.b.w = (2 / (von_Kar * ustar)) * ( kappa / D.h2o) ^ 0.667 ## quasi-laminar layer resistance to water vapor
  ## leaf temperature
  t.f = t.s + (r.a + r.b.h)*H/(c.p*rho) ## r.b is quasi-laminar layer resistance to heat
  if (is.na(qa)) {
    e = rh * f_esat(t.f)
  }else{
    e = p*qa/0.622
  }
  
  ## evaporative-resistance form (Ducker et al.)
  r.sw = epsilon * rho * (f_esat(t.f) - e)/(p * E) - (r.a + r.b.w) 
  r.s = r.sw*1.61  
  
  return(r.s)   
}

sd.trim <- function(x, trim=0, na.rm=FALSE)
{
  if(!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    warning("argument is not numeric or logical: returning NA")
    return(NA_real_)
  }
  if(na.rm) x <- x[!is.na(x)]
  if(!is.numeric(trim) || length(trim) != 1)
    stop("'trim' must be numeric of length one")
  n <- length(x)
  if(trim > 0 && n > 0) {
    if(is.complex(x)) stop("trimmed sd are not defined for complex data")
    if(trim >= 0.5) return(0)
    lo <- floor(n * trim) + 1
    hi <- n + 1 - lo
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
  }
  sd(x)
}



