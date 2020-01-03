# ========================================================
# functions used in obsdata_plot.R 
# ========================================================
source('geophys_const.R')
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

psi.H = function(d, L, z.r = 29){ # stability function 
  if (!is.na(L)) {
    tmp = (z.r- d)/L
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

r.a = function(z0, ustar, d, L, psi.H, z.r = 29){ 
  if (!is.na(psi.H)) {
    ra = (log((z.r - d)/z0) - psi.H * (z.r - d) / L)/(von_Kar * ustar)
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




















