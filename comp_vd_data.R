#########################################################
# Data for evalVdMod.R
#
#


# ----------------------------------------------------------------------------------------------------------------------------------------------------------
#
# sesonal average vd scatter plots 
#
# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------
# deciduous
seasonl = list("summer", "spring", "fall", "summer", "fall", "summer", "winter", "summer", "summer", "winter","winter","summer",
               "fall", "spring", "spring","summer","spring")
obslist = list(obs.hf.summer$o3vd, obs.hf.spring$o3vd, obs.hf.fall$o3vd, wu.june.2011.decid$o3vd, wu.sept.2011.decid$o3vd, 
               munger.decid.summer.hrly$o3vd, munger.decid.winter.hrly$o3vd, fink.2000.decid$o3vd, 
               padro.summer.1992.decid$o3vd, padro.winter.1992.decid$o3vd, wu.winter.2018.decid$o3vd, wu.summer.2018.decid$o3vd,
               bf.fall.avghrly[,1], bf.spring.avghrly[,1], matsuda.dry.2004.decid$o3vd, matsuda.wet.2004.decid$o3vd, matsuda.spring.2002.decid$o3vd)   # 4/5/2019 add
# padro.spring.1992.decid$o3vd removed 
modlist.w98 = list(hf9901.summer.hrly$vd.w98, hf9901.spring.hrly$vd.w98, hf9901.fall.hrly$vd.w98, wu2011.summer.hrly$vd.w98,
                   wu2011.fall.hrly$vd.w98, munger96.summer.hrly$vd.w98, munger96.winter.hrly$vd.w98, fink00.summer.hrly$vd.w98,
                   padro91.summer.hrly$vd.w98, padro91.winter.hrly$vd.w98, bf.winter.hrly$vd.w98, bf.summer.hrly$vd.w98,
                   bf.fall.hrly$vd.w98, bf.spring.hrly$vd.w98, matsuda06.dry.hrly$vd.w98, matsuda06.wet.hrly$vd.w98, matsuda05.spring.hrly$vd.w98)

modlist.w98fbb = list(hf9901.summer.hrly$vd.w98fbb, hf9901.spring.hrly$vd.w98fbb, hf9901.fall.hrly$vd.w98fbb, wu2011.summer.hrly$vd.w98fbb,
                      wu2011.fall.hrly$vd.w98fbb, munger96.summer.hrly$vd.w98fbb, munger96.winter.hrly$vd.w98fbb, fink00.summer.hrly$vd.w98fbb,
                      padro91.summer.hrly$vd.w98fbb, padro91.winter.hrly$vd.w98fbb, bf.winter.hrly$vd.w98fbb, bf.summer.hrly$vd.w98fbb,
                      bf.fall.hrly$vd.w98fbb, bf.spring.hrly$vd.w98fbb, matsuda06.dry.hrly$vd.w98fbb, matsuda06.wet.hrly$vd.w98fbb, matsuda05.spring.hrly$vd.w98fbb)

modlist.w98med = list(hf9901.summer.hrly$vd.w98med, hf9901.spring.hrly$vd.w98med, hf9901.fall.hrly$vd.w98med, wu2011.summer.hrly$vd.w98med,
                      wu2011.fall.hrly$vd.w98med, munger96.summer.hrly$vd.w98med, munger96.winter.hrly$vd.w98med, fink00.summer.hrly$vd.w98med,
                      padro91.summer.hrly$vd.w98med, padro91.winter.hrly$vd.w98med, bf.winter.hrly$vd.w98med, bf.summer.hrly$vd.w98med,
                      bf.fall.hrly$vd.w98med, bf.spring.hrly$vd.w98med, matsuda06.dry.hrly$vd.w98med, matsuda06.wet.hrly$vd.w98med, matsuda05.spring.hrly$vd.w98med)

modlist.zh = list(hf9901.summer.hrly$vd.zh, hf9901.spring.hrly$vd.zh, hf9901.fall.hrly$vd.zh, wu2011.summer.hrly$vd.zh,
                  wu2011.fall.hrly$vd.zh, munger96.summer.hrly$vd.zh, munger96.winter.hrly$vd.zh, fink00.summer.hrly$vd.zh,
                  padro91.summer.hrly$vd.zh, padro91.winter.hrly$vd.zh, bf.winter.hrly$vd.zh, bf.summer.hrly$vd.zh,
                  bf.fall.hrly$vd.zh, bf.spring.hrly$vd.zh, matsuda06.dry.hrly$vd.zh, matsuda06.wet.hrly$vd.zh, matsuda05.spring.hrly$vd.zh)

modlist.zhfbb = list(hf9901.summer.hrly$vd.zhfbb, hf9901.spring.hrly$vd.zhfbb, hf9901.fall.hrly$vd.zhfbb, wu2011.summer.hrly$vd.zhfbb,
                     wu2011.fall.hrly$vd.zhfbb, munger96.summer.hrly$vd.zhfbb, munger96.winter.hrly$vd.zhfbb, fink00.summer.hrly$vd.zhfbb,
                     padro91.summer.hrly$vd.zhfbb, padro91.winter.hrly$vd.zhfbb, bf.winter.hrly$vd.zhfbb, bf.summer.hrly$vd.zhfbb,
                     bf.fall.hrly$vd.zhfbb, bf.spring.hrly$vd.zhfbb, matsuda06.dry.hrly$vd.zhfbb, matsuda06.wet.hrly$vd.zhfbb, matsuda05.spring.hrly$vd.zhfbb)

modlist.zhmed = list(hf9901.summer.hrly$vd.zhmed, hf9901.spring.hrly$vd.zhmed, hf9901.fall.hrly$vd.zhmed, wu2011.summer.hrly$vd.zhmed,
                     wu2011.fall.hrly$vd.zhmed, munger96.summer.hrly$vd.zhmed, munger96.winter.hrly$vd.zhmed, fink00.summer.hrly$vd.zhmed,
                     padro91.summer.hrly$vd.zhmed, padro91.winter.hrly$vd.zhmed, bf.winter.hrly$vd.zhmed, bf.summer.hrly$vd.zhmed,
                     bf.fall.hrly$vd.zhmed, bf.spring.hrly$vd.zhmed, matsuda06.dry.hrly$vd.zhmed, matsuda06.wet.hrly$vd.zhmed, matsuda05.spring.hrly$vd.zhmed)

# statistics
schemelist = c('w98','w98fbb','w98med','zh','zhfbb','zhmed')
# winter ---------------------------------
tmp = list(obs.hf.winter$o3vd, munger.decid.winter.hrly$o3vd, padro.winter.1992.decid$o3vd, wu.winter.2018.decid$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(hf9901.winter.hrly[[paste0("vd.", scheme)]], munger96.winter.hrly[[paste0("vd.", scheme)]],padro91.winter.hrly[[paste0("vd.", scheme)]], bf.winter.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
## daytime  9:00-15:00 (?)
daytime_def = 10:16
tmp = list(obs.hf.winter$o3vd[daytime_def], munger.decid.winter.hrly$o3vd[daytime_def], padro.winter.1992.decid$o3vd[daytime_def], wu.winter.2018.decid$o3vd[daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(hf9901.winter.hrly[[paste0("vd.", scheme)]][daytime_def], munger96.winter.hrly[[paste0("vd.", scheme)]][daytime_def],padro91.winter.hrly[[paste0("vd.", scheme)]][daytime_def], bf.winter.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}


# summer ---------------------------------
tmp = list(obs.hf.summer$o3vd, wu.june.2011.decid$o3vd, munger.decid.summer.hrly$o3vd, fink.2000.decid$o3vd, padro.summer.1992.decid$o3vd, wu.summer.2018.decid$o3vd, matsuda.wet.2004.decid$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs))
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(hf9901.summer.hrly[[paste0("vd.", scheme)]], wu2011.summer.hrly[[paste0("vd.", scheme)]], munger96.summer.hrly[[paste0("vd.", scheme)]],fink00.summer.hrly[[paste0("vd.", scheme)]], 
             padro91.summer.hrly[[paste0("vd.", scheme)]], bf.summer.hrly[[paste0("vd.", scheme)]], matsuda06.wet.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime -----------
tmp = list(obs.hf.summer$o3vd[daytime_def], wu.june.2011.decid$o3vd[daytime_def], munger.decid.summer.hrly$o3vd[daytime_def], fink.2000.decid$o3vd[daytime_def], 
           padro.summer.1992.decid$o3vd[daytime_def], wu.summer.2018.decid$o3vd[daytime_def], matsuda.wet.2004.decid$o3vd[daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs))
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(hf9901.summer.hrly[[paste0("vd.", scheme)]][daytime_def], wu2011.summer.hrly[[paste0("vd.", scheme)]][daytime_def], munger96.summer.hrly[[paste0("vd.", scheme)]][daytime_def],fink00.summer.hrly[[paste0("vd.", scheme)]][daytime_def], 
             padro91.summer.hrly[[paste0("vd.", scheme)]][daytime_def], bf.summer.hrly[[paste0("vd.", scheme)]][daytime_def], matsuda06.wet.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# spring  ---------------------------------
tmp = list(obs.hf.spring$o3vd, matsuda.spring.2002.decid$o3vd, bf.spring.avghrly[,1], matsuda.dry.2004.decid$o3vd)  # bf.spring, borden forest
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs))
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(hf9901.spring.hrly[[paste0("vd.", scheme)]], matsuda05.spring.hrly[[paste0("vd.", scheme)]], bf.spring.hrly[[paste0("vd.", scheme)]], matsuda06.dry.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime 
tmp = list(obs.hf.spring$o3vd[daytime_def], matsuda.spring.2002.decid$o3vd[daytime_def], bf.spring.avghrly[,1][daytime_def], matsuda.dry.2004.decid$o3vd[daytime_def])  # bf.spring, borden forest
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs))
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(hf9901.spring.hrly[[paste0("vd.", scheme)]][daytime_def], matsuda05.spring.hrly[[paste0("vd.", scheme)]][daytime_def], bf.spring.hrly[[paste0("vd.", scheme)]][daytime_def], matsuda06.dry.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# fall ----
tmp = list(obs.hf.fall$o3vd, wu.sept.2011.decid$o3vd, bf.fall.avghrly[,1])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs))
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(hf9901.fall.hrly[[paste0("vd.", scheme)]], wu2011.fall.hrly[[paste0("vd.", scheme)]], bf.fall.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime
tmp = list(obs.hf.fall$o3vd[daytime_def], wu.sept.2011.decid$o3vd[daytime_def], bf.fall.avghrly[,1][daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs))
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(hf9901.fall.hrly[[paste0("vd.", scheme)]][daytime_def], wu2011.fall.hrly[[paste0("vd.", scheme)]][daytime_def], bf.fall.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# -----------------------------------------------------------------------------
# coniferous
seasonl = list("summer", "spring", "fall", "winter", "summer", "spring", "fall", "winter",
               "spring","spring","spring",
               "summer","fall","summer","spring","summer","spring","fall","winter",
               "summer","spring","fall","winter", "summer",
               "summer","spring")  #lamaud
obslist = list(mikk.summer.2004.conif$o3vd, mikk.spring.2004.conif$o3vd, mikk.fall.2004.conif$o3vd, mikk.winter.2004.conif$o3vd, 
               kurp.summer.2002.conif$o3vd, kurp.spring.2002.conif$o3vd, kurp.fall.2002.conif$o3vd, kurp.winter.2002.conif$o3vd, 
               turn.apr.2009.conif$o3vd, turn.may.1.2009.conif$o3vd, turn.may.2.2009.conif$o3vd,
               mikk.june.2000.conif$o3vd, 0.1*mikk.sep.2000.conif$o3vd, schef96.summer.conif$o3vd, 0.1*coe.spring.conif$o3vd, 
               fares.summer.2010.conif$o3vd, fares.spring.2010.conif$o3vd, fares.fall.2010.conif$o3vd, fares.winter.2010.conif$o3vd,
               hole.summer.2004.conif$o3vd, hole.spring.2004.conif$o3vd, hole.fall.2004.conif$o3vd, hole.winter.2004.conif$o3vd, rep(0.388,24),  # turn09 growing season 0.388+-0.04
               lamaud.summer.1994.conif$o3vd, lamaud.spring.1994.conif$o3vd)

modlist.w98 = list(mikk04.summer.hrly$vd.w98,mikk04.spring.hrly$vd.w98, mikk04.fall.hrly$vd.w98,mikk04.winter.hrly$vd.w98,
                   kurp02.summer.hrly$vd.w98,kurp02.spring.hrly$vd.w98,kurp02.fall.hrly$vd.w98,kurp02.winter.hrly$vd.w98,
                   turn09.apr.hrly$vd.w98, turn09.may.1.hrly$vd.w98, turn09.may.2.hrly$vd.w98,
                   mikk00.summer.hrly$vd.w98, mikk00.fall.hrly$vd.w98, schef96.summer.hrly$vd.w98, coe95.spring.hrly$vd.w98,
                   fare10.summer.hrly$vd.w98, fare10.spring.hrly$vd.w98, fare10.fall.hrly$vd.w98, fare10.winter.hrly$vd.w98,
                   hole04.summer.hrly$vd.w98, hole04.spring.hrly$vd.w98, hole04.fall.hrly$vd.w98, hole04.winter.hrly$vd.w98,
                   turn09.summer.hrly$vd.w98, lamaud02.summer.hrly$vd.w98, lamaud02.spring.hrly$vd.w98)

modlist.w98fbb = list(mikk04.summer.hrly$vd.w98fbb,mikk04.spring.hrly$vd.w98fbb, mikk04.fall.hrly$vd.w98fbb,mikk04.winter.hrly$vd.w98fbb,
                      kurp02.summer.hrly$vd.w98fbb,kurp02.spring.hrly$vd.w98fbb,kurp02.fall.hrly$vd.w98fbb,kurp02.winter.hrly$vd.w98fbb,
                      turn09.apr.hrly$vd.w98fbb, turn09.may.1.hrly$vd.w98fbb, turn09.may.2.hrly$vd.w98fbb,
                      mikk00.summer.hrly$vd.w98fbb, mikk00.fall.hrly$vd.w98fbb, schef96.summer.hrly$vd.w98fbb, coe95.spring.hrly$vd.w98fbb,
                      fare10.summer.hrly$vd.w98fbb, fare10.spring.hrly$vd.w98fbb, fare10.fall.hrly$vd.w98fbb, fare10.winter.hrly$vd.w98fbb,
                      hole04.summer.hrly$vd.w98fbb, hole04.spring.hrly$vd.w98fbb, hole04.fall.hrly$vd.w98fbb, hole04.winter.hrly$vd.w98fbb,
                      turn09.summer.hrly$vd.w98fbb, lamaud02.summer.hrly$vd.w98fbb, lamaud02.spring.hrly$vd.w98fbb )

modlist.w98med = list(mikk04.summer.hrly$vd.w98med,mikk04.spring.hrly$vd.w98med, mikk04.fall.hrly$vd.w98med,mikk04.winter.hrly$vd.w98med,
                      kurp02.summer.hrly$vd.w98med,kurp02.spring.hrly$vd.w98med,kurp02.fall.hrly$vd.w98med,kurp02.winter.hrly$vd.w98med,
                      turn09.apr.hrly$vd.w98med, turn09.may.1.hrly$vd.w98med, turn09.may.2.hrly$vd.w98med,
                      mikk00.summer.hrly$vd.w98med, mikk00.fall.hrly$vd.w98med, schef96.summer.hrly$vd.w98med, coe95.spring.hrly$vd.w98med,
                      fare10.summer.hrly$vd.w98med, fare10.spring.hrly$vd.w98med, fare10.fall.hrly$vd.w98med, fare10.winter.hrly$vd.w98med,
                      hole04.summer.hrly$vd.w98med, hole04.spring.hrly$vd.w98med, hole04.fall.hrly$vd.w98med, hole04.winter.hrly$vd.w98med,
                      turn09.summer.hrly$vd.w98med,lamaud02.summer.hrly$vd.w98med, lamaud02.spring.hrly$vd.w98med)

modlist.zh = list(mikk04.summer.hrly$vd.zh,mikk04.spring.hrly$vd.zh, mikk04.fall.hrly$vd.zh,mikk04.winter.hrly$vd.zh,
                  kurp02.summer.hrly$vd.zh,kurp02.spring.hrly$vd.zh,kurp02.fall.hrly$vd.zh,kurp02.winter.hrly$vd.zh,
                  turn09.apr.hrly$vd.zh, turn09.may.1.hrly$vd.zh, turn09.may.2.hrly$vd.zh,
                  mikk00.summer.hrly$vd.zh, mikk00.fall.hrly$vd.zh, schef96.summer.hrly$vd.zh, coe95.spring.hrly$vd.zh,
                  fare10.summer.hrly$vd.zh, fare10.spring.hrly$vd.zh, fare10.fall.hrly$vd.zh, fare10.winter.hrly$vd.zh,
                  hole04.summer.hrly$vd.zh, hole04.spring.hrly$vd.zh, hole04.fall.hrly$vd.zh, hole04.winter.hrly$vd.zh,
                  turn09.summer.hrly$vd.zh,lamaud02.summer.hrly$vd.zh, lamaud02.spring.hrly$vd.zh)

modlist.zhfbb = list(mikk04.summer.hrly$vd.zhfbb,mikk04.spring.hrly$vd.zhfbb, mikk04.fall.hrly$vd.zhfbb,mikk04.winter.hrly$vd.zhfbb,
                     kurp02.summer.hrly$vd.zhfbb,kurp02.spring.hrly$vd.zhfbb,kurp02.fall.hrly$vd.zhfbb,kurp02.winter.hrly$vd.zhfbb,
                     turn09.apr.hrly$vd.zhfbb, turn09.may.1.hrly$vd.zhfbb, turn09.may.2.hrly$vd.zhfbb,
                     mikk00.summer.hrly$vd.zhfbb, mikk00.fall.hrly$vd.zhfbb, schef96.summer.hrly$vd.zhfbb, coe95.spring.hrly$vd.zhfbb,
                     fare10.summer.hrly$vd.zhfbb, fare10.spring.hrly$vd.zhfbb, fare10.fall.hrly$vd.zhfbb, fare10.winter.hrly$vd.zhfbb,
                     hole04.summer.hrly$vd.zhfbb, hole04.spring.hrly$vd.zhfbb, hole04.fall.hrly$vd.zhfbb, hole04.winter.hrly$vd.zhfbb,
                     turn09.summer.hrly$vd.zhfbb,lamaud02.summer.hrly$vd.zhfbb, lamaud02.spring.hrly$vd.zhfbb)

modlist.zhmed = list(mikk04.summer.hrly$vd.zhmed,mikk04.spring.hrly$vd.zhmed, mikk04.fall.hrly$vd.zhmed,mikk04.winter.hrly$vd.zhmed,
                     kurp02.summer.hrly$vd.zhmed,kurp02.spring.hrly$vd.zhmed,kurp02.fall.hrly$vd.zhmed,kurp02.winter.hrly$vd.zhmed,
                     turn09.apr.hrly$vd.zhmed, turn09.may.1.hrly$vd.zhmed, turn09.may.2.hrly$vd.zhmed,
                     mikk00.summer.hrly$vd.zhmed, mikk00.fall.hrly$vd.zhmed, schef96.summer.hrly$vd.zhmed, coe95.spring.hrly$vd.zhmed,
                     fare10.summer.hrly$vd.zhmed, fare10.spring.hrly$vd.zhmed, fare10.fall.hrly$vd.zhmed, fare10.winter.hrly$vd.zhmed,
                     hole04.summer.hrly$vd.zhmed, hole04.spring.hrly$vd.zhmed, hole04.fall.hrly$vd.zhmed, hole04.winter.hrly$vd.zhmed,
                     turn09.summer.hrly$vd.zhmed, lamaud02.summer.hrly$vd.zhmed, lamaud02.spring.hrly$vd.zhmed)

# winter ----
tmp = list(mikk.winter.2004.conif$o3vd, kurp.winter.2002.conif$o3vd, fares.winter.2010.conif$o3vd, hole.winter.2004.conif$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(mikk04.winter.hrly[[paste0("vd.", scheme)]], kurp02.winter.hrly[[paste0("vd.", scheme)]],fare10.winter.hrly[[paste0("vd.", scheme)]], hole04.winter.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime
tmp = list(mikk.winter.2004.conif$o3vd[daytime_def], kurp.winter.2002.conif$o3vd[daytime_def], fares.winter.2010.conif$o3vd[daytime_def], hole.winter.2004.conif$o3vd[daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(mikk04.winter.hrly[[paste0("vd.", scheme)]][daytime_def], kurp02.winter.hrly[[paste0("vd.", scheme)]][daytime_def],fare10.winter.hrly[[paste0("vd.", scheme)]][daytime_def], hole04.winter.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}


# summer ----
tmp = list(mikk.summer.2004.conif$o3vd, kurp.summer.2002.conif$o3vd,mikk.june.2000.conif$o3vd,schef96.summer.conif$o3vd, 
           fares.summer.2010.conif$o3vd, hole.summer.2004.conif$o3vd, rep(0.4,24), lamaud.summer.1994.conif$o3vd)  # lamaud94 avg 0.4
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(mikk04.summer.hrly[[paste0("vd.", scheme)]], kurp02.summer.hrly[[paste0("vd.", scheme)]], mikk00.summer.hrly[[paste0("vd.", scheme)]],
             schef96.summer.hrly[[paste0("vd.", scheme)]],fare10.summer.hrly[[paste0("vd.", scheme)]], hole04.summer.hrly[[paste0("vd.", scheme)]],
             lamaud02.summer.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime
tmp = list(mikk.summer.2004.conif$o3vd[daytime_def], kurp.summer.2002.conif$o3vd[daytime_def], mikk.june.2000.conif$o3vd[daytime_def], schef96.summer.conif$o3vd[daytime_def], 
           fares.summer.2010.conif$o3vd[daytime_def], hole.summer.2004.conif$o3vd[daytime_def], rep(0.4,24)[daytime_def], lamaud.summer.1994.conif$o3vd[daytime_def])  # lamaud94 avg 0.4
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(mikk04.summer.hrly[[paste0("vd.", scheme)]][daytime_def], kurp02.summer.hrly[[paste0("vd.", scheme)]][daytime_def], mikk00.summer.hrly[[paste0("vd.", scheme)]][daytime_def],
             schef96.summer.hrly[[paste0("vd.", scheme)]][daytime_def],fare10.summer.hrly[[paste0("vd.", scheme)]][daytime_def], hole04.summer.hrly[[paste0("vd.", scheme)]][daytime_def],
             lamaud02.summer.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# spring ---------------------------
tmp = list(mikk.spring.2004.conif$o3vd, kurp.spring.2002.conif$o3vd, rep(0.388,24),  # turn09 growing season 0.388+-0.04
           0.1*coe.spring.conif$o3vd,fares.spring.2010.conif$o3vd, hole.spring.2004.conif$o3vd, lamaud.spring.1994.conif$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(mikk04.spring.hrly[[paste0("vd.", scheme)]], kurp02.spring.hrly[[paste0("vd.", scheme)]], turn09.summer.hrly[[paste0("vd.", scheme)]],
             coe95.spring.hrly[[paste0("vd.", scheme)]], fare10.spring.hrly[[paste0("vd.", scheme)]], hole04.spring.hrly[[paste0("vd.", scheme)]],
             lamaud02.spring.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime 
tmp = list(mikk.spring.2004.conif$o3vd[daytime_def], kurp.spring.2002.conif$o3vd[daytime_def], rep(0.388,24)[daytime_def],  # turn09 growing season 0.388+-0.04
           0.1*coe.spring.conif$o3vd[daytime_def],fares.spring.2010.conif$o3vd[daytime_def], hole.spring.2004.conif$o3vd[daytime_def], lamaud.spring.1994.conif$o3vd[daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(mikk04.spring.hrly[[paste0("vd.", scheme)]][daytime_def], kurp02.spring.hrly[[paste0("vd.", scheme)]][daytime_def], turn09.summer.hrly[[paste0("vd.", scheme)]][daytime_def],
             coe95.spring.hrly[[paste0("vd.", scheme)]][daytime_def], fare10.spring.hrly[[paste0("vd.", scheme)]][daytime_def], hole04.spring.hrly[[paste0("vd.", scheme)]][daytime_def],
             lamaud02.spring.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# fall ----
tmp = list(mikk.fall.2004.conif$o3vd, kurp.fall.2002.conif$o3vd, 0.1*mikk.sep.2000.conif$o3vd,fares.fall.2010.conif$o3vd, hole.fall.2004.conif$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(mikk04.fall.hrly[[paste0("vd.", scheme)]],kurp02.fall.hrly[[paste0("vd.", scheme)]],mikk00.fall.hrly[[paste0("vd.", scheme)]],fare10.fall.hrly[[paste0("vd.", scheme)]],hole04.fall.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime
tmp = list(mikk.fall.2004.conif$o3vd[daytime_def], kurp.fall.2002.conif$o3vd[daytime_def], 0.1*mikk.sep.2000.conif$o3vd[daytime_def],fares.fall.2010.conif$o3vd[daytime_def], hole.fall.2004.conif$o3vd[daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(mikk04.fall.hrly[[paste0("vd.", scheme)]][daytime_def],kurp02.fall.hrly[[paste0("vd.", scheme)]][daytime_def],mikk00.fall.hrly[[paste0("vd.", scheme)]][daytime_def],
             fare10.fall.hrly[[paste0("vd.", scheme)]][daytime_def],hole04.fall.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# -----------------------------------------------------------------------------
# grass

seasonl = list('summer','spring','fall','winter','summer','summer','winter','spring',
               'summer','fall', 'fall','winter', 'summer', 'spring')
obslist = list(fowl.2001.summer.grass$o3vd, fowl.2001.spring.grass$o3vd, fowl.2001.fall.grass$o3vd,fowl.2001.winter.grass$o3vd, 
               pio.2000.aug.grass$o3vd, pio.2000.june.grass$o3vd, pio.2000.feb.grass$o3vd, pio.2000.apr.grass$o3vd,
               padro.1994.grass$o3vd, cies.2004.klippen$o3vd, cies.2004.ispra$o3vd, sig.2002.wet$o3vd, rep(0.36, 24), 
               meyers.1998.sand.hrly$o3vd)  # droppo 0.36

modlist.w98 = list(fowl01.summer.hrly$vd.w98, fowl01.spring.hrly$vd.w98, fowl01.fall.hrly$vd.w98, fowl01.winter.hrly$vd.w98, 
                   pio00.aug.hrly$vd.w98, pio00.june.hrly$vd.w98, pio00.feb.hrly$vd.w98, pio00.apr.hrly$vd.w98,
                   padro94.grass.hrly$vd.w98, cies04klippen.sep.hrly$vd.w98, cies04ispra.sep.hrly$vd.w98,
                   sig02.winter.hrly$vd.w98, droppo85.summer.hrly$vd.w98, mey98sand.spring.hrly$vd.w98)

modlist.w98fbb = list(fowl01.summer.hrly$vd.w98fbb, fowl01.spring.hrly$vd.w98fbb, fowl01.fall.hrly$vd.w98fbb, fowl01.winter.hrly$vd.w98fbb, 
                      pio00.aug.hrly$vd.w98fbb, pio00.june.hrly$vd.w98fbb, pio00.feb.hrly$vd.w98fbb, pio00.apr.hrly$vd.w98fbb,
                      padro94.grass.hrly$vd.w98fbb, cies04klippen.sep.hrly$vd.w98fbb, cies04ispra.sep.hrly$vd.w98fbb, 
                      sig02.winter.hrly$vd.w98fbb, droppo85.summer.hrly$vd.w98fbb, mey98sand.spring.hrly$vd.w98fbb)

modlist.w98med = list(fowl01.summer.hrly$vd.w98med, fowl01.spring.hrly$vd.w98med, fowl01.fall.hrly$vd.w98med, fowl01.winter.hrly$vd.w98med, 
                      pio00.aug.hrly$vd.w98med, pio00.june.hrly$vd.w98med, pio00.feb.hrly$vd.w98med, pio00.apr.hrly$vd.w98med,
                      padro94.grass.hrly$vd.w98med, cies04klippen.sep.hrly$vd.w98med, cies04ispra.sep.hrly$vd.w98med, 
                      sig02.winter.hrly$vd.w98med, droppo85.summer.hrly$vd.w98med, mey98sand.spring.hrly$vd.w98med)

modlist.zh = list(fowl01.summer.hrly$vd.zh, fowl01.spring.hrly$vd.zh, fowl01.fall.hrly$vd.zh, fowl01.winter.hrly$vd.zh, 
                  pio00.aug.hrly$vd.zh, pio00.june.hrly$vd.zh, pio00.feb.hrly$vd.zh, pio00.apr.hrly$vd.zh,
                  padro94.grass.hrly$vd.zh, cies04klippen.sep.hrly$vd.zh, cies04ispra.sep.hrly$vd.zh,
                  sig02.winter.hrly$vd.zh, droppo85.summer.hrly$vd.zh, mey98sand.spring.hrly$vd.zh)

modlist.zhfbb = list(fowl01.summer.hrly$vd.zhfbb, fowl01.spring.hrly$vd.zhfbb, fowl01.winter.hrly$vd.zhfbb, fowl01.fall.hrly$vd.zhfbb, 
                     pio00.aug.hrly$vd.zhfbb, pio00.june.hrly$vd.zhfbb, pio00.feb.hrly$vd.zhfbb, pio00.apr.hrly$vd.zhfbb,
                     padro94.grass.hrly$vd.zhfbb, cies04klippen.sep.hrly$vd.zhfbb, cies04ispra.sep.hrly$vd.zhfbb, 
                     sig02.winter.hrly$vd.zhfbb, droppo85.summer.hrly$vd.zhfbb, mey98sand.spring.hrly$vd.zhfbb)

modlist.zhmed = list(fowl01.summer.hrly$vd.zhmed, fowl01.spring.hrly$vd.zhmed, fowl01.fall.hrly$vd.zhmed, fowl01.winter.hrly$vd.zhmed, 
                     pio00.aug.hrly$vd.zhmed, pio00.june.hrly$vd.zhmed, pio00.feb.hrly$vd.zhmed, pio00.apr.hrly$vd.zhmed,
                     padro94.grass.hrly$vd.zhmed, cies04klippen.sep.hrly$vd.zhmed, cies04ispra.sep.hrly$gc.zhmed, 
                     sig02.winter.hrly$vd.zhmed, droppo85.summer.hrly$vd.zhmed, mey98sand.spring.hrly$vd.zhmed)


# summer ----
tmp = list(fowl.2001.summer.grass$o3vd, pio.2000.aug.grass$o3vd, pio.2000.june.grass$o3vd,
           padro.1994.grass$o3vd, rep(0.47,24), sig.2002.wet$o3vd)  # droppo:0.47
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(fowl01.summer.hrly[[paste0("vd.", scheme)]], pio00.aug.hrly[[paste0("vd.", scheme)]], pio00.june.hrly[[paste0("vd.", scheme)]], padro94.crop.jul.hrly[[paste0("vd.", scheme)]], 
             droppo85.summer.hrly[[paste0("vd.", scheme)]], sig02.winter.hrly[[paste0("vd.", scheme)]])  # sig02 summer
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime
tmp = list(fowl.2001.summer.grass$o3vd[daytime_def], pio.2000.aug.grass$o3vd[daytime_def], pio.2000.june.grass$o3vd[daytime_def],
           padro.1994.grass$o3vd[daytime_def], rep(0.47,24)[daytime_def], sig.2002.wet$o3vd[daytime_def])  # droppo:0.47
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(fowl01.summer.hrly[[paste0("vd.", scheme)]][daytime_def], pio00.aug.hrly[[paste0("vd.", scheme)]][daytime_def], pio00.june.hrly[[paste0("vd.", scheme)]][daytime_def], padro94.crop.jul.hrly[[paste0("vd.", scheme)]][daytime_def], 
             droppo85.summer.hrly[[paste0("vd.", scheme)]][daytime_def], sig02.winter.hrly[[paste0("vd.", scheme)]][daytime_def])  # sig02 summer
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# spring
tmp = list(fowl.2001.spring.grass$o3vd, pio.2000.apr.grass$o3vd, meyers.1998.sand.hrly$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(fowl01.spring.hrly[[paste0("vd.", scheme)]], pio00.apr.hrly[[paste0("vd.", scheme)]], mey98sand.spring.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime
tmp = list(fowl.2001.spring.grass$o3vd[daytime_def], pio.2000.apr.grass$o3vd[daytime_def], meyers.1998.sand.hrly$o3vd[daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(fowl01.spring.hrly[[paste0("vd.", scheme)]][daytime_def], pio00.apr.hrly[[paste0("vd.", scheme)]][daytime_def], mey98sand.spring.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# fall ----------------------------
tmp = list(fowl.2001.fall.grass$o3vd,cies.2004.klippen$o3vd,cies.2004.ispra$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(fowl01.fall.hrly[[paste0("vd.", scheme)]], cies04ispra.sep.hrly[[paste0("vd.", scheme)]], cies04klippen.sep.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime
tmp = list(fowl.2001.fall.grass$o3vd[daytime_def],cies.2004.klippen$o3vd[daytime_def],cies.2004.ispra$o3vd[daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(fowl01.fall.hrly[[paste0("vd.", scheme)]][daytime_def], cies04ispra.sep.hrly[[paste0("vd.", scheme)]][daytime_def], cies04klippen.sep.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# winter ----------------------------
tmp = list(fowl.2001.winter.grass$o3vd, pio.2000.feb.grass$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(fowl01.winter.hrly[[paste0("vd.", scheme)]], pio00.feb.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime
tmp = list(fowl.2001.winter.grass$o3vd[daytime_def], pio.2000.feb.grass$o3vd[daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(fowl01.winter.hrly[[paste0("vd.", scheme)]][daytime_def], pio00.feb.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}


# -----------------------------------------------------------------------------
# crops
seasonl = list('C3 crop', 'C4 crop', 'C4 crop','C4 crop','C4 crop','C3 crop','C3 crop','C3 crop')
obslist = list(padro.1994.crop$o3vd, stella.2011.gringon.crop$o3vd, stella.2011.capesud.crops$o3vd, stella.2011.lamasquere.crop$o3vd,
               meyers.1998.bond.crop$o3vd, meyers.1998.nash.crop$o3vd, coyle.2009.crop$o3vd, 
               pilegaard.1998.hrly$o3vd)

modlist.w98 = list(padro94.crop.jul.hrly$vd.w98, stella11LaCape.hrly$vd.w98, stella11Lama.hrly$vd.w98, stella11Grig.hrly$vd.w98, mey98bond.hrly$vd.w98, 
                   mey98nash.hrly$vd.w98, coyle09.hrly$vd.w98, pilegaard98.hrly$vd.w98)
modlist.w98fbb = list(padro94.crop.jul.hrly$vd.w98fbb, stella11LaCape.hrly$vd.w98fbb, stella11Lama.hrly$vd.w98fbb, stella11Grig.hrly$vd.w98fbb, mey98bond.hrly$vd.w98fbb, 
                      mey98nash.hrly$vd.w98fbb, coyle09.hrly$vd.w98fbb, pilegaard98.hrly$vd.w98fbb)
modlist.w98med = list(padro94.crop.jul.hrly$vd.w98med, stella11LaCape.hrly$vd.w98med, stella11Lama.hrly$vd.w98med, stella11Grig.hrly$vd.w98med, mey98bond.hrly$vd.w98med, 
                      mey98nash.hrly$vd.w98med, coyle09.hrly$vd.w98med, pilegaard98.hrly$vd.w98med)
modlist.zh = list(padro94.crop.jul.hrly$vd.zh, stella11LaCape.hrly$vd.zh, stella11Lama.hrly$vd.zh, stella11Grig.hrly$vd.zh, mey98bond.hrly$vd.zh, 
                  mey98nash.hrly$vd.zh, coyle09.hrly$vd.zh, pilegaard98.hrly$vd.zh)
modlist.zhfbb = list(padro94.crop.jul.hrly$vd.zhfbb, stella11LaCape.hrly$vd.zhfbb, stella11Lama.hrly$vd.zhfbb, stella11Grig.hrly$vd.zhfbb, mey98bond.hrly$vd.zhfbb, 
                     mey98nash.hrly$vd.zhfbb, coyle09.hrly$vd.zhfbb, pilegaard98.hrly$vd.zhfbb)
modlist.zhmed = list(padro94.crop.jul.hrly$vd.zhmed, stella11LaCape.hrly$vd.zhmed, stella11Lama.hrly$vd.zhmed, stella11Grig.hrly$vd.zhmed, mey98bond.hrly$vd.zhmed, 
                     mey98nash.hrly$vd.zhmed, coyle09.hrly$vd.zhmed, pilegaard98.hrly$vd.zhmed)


tmp = list(padro.1994.crop$o3vd, stella.2011.gringon.crop$o3vd, stella.2011.capesud.crops$o3vd, stella.2011.lamasquere.crop$o3vd,
           meyers.1998.bond.fast.crop$o3vd, meyers.1998.nash.fast.crop$o3vd, coyle.2009.crop$o3vd, pilegaard.1998.hrly$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(padro94.crop.jul.hrly[[paste0("vd.", scheme)]], stella11LaCape.hrly[[paste0("vd.", scheme)]], stella11Lama.hrly[[paste0("vd.", scheme)]],stella11Grig.hrly[[paste0("vd.", scheme)]], 
             mey98bond.fast.hrly[[paste0("vd.", scheme)]], mey98nash.fast.hrly[[paste0("vd.", scheme)]], coyle09.hrly[[paste0("vd.", scheme)]], pilegaard98.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}
# daytime
tmp = list(padro.1994.crop$o3vd[daytime_def], stella.2011.gringon.crop$o3vd[daytime_def], stella.2011.capesud.crops$o3vd[daytime_def], stella.2011.lamasquere.crop$o3vd[daytime_def],
           meyers.1998.bond.fast.crop$o3vd[daytime_def], meyers.1998.nash.fast.crop$o3vd[daytime_def], coyle.2009.crop$o3vd[daytime_def], pilegaard.1998.hrly$o3vd[daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(padro94.crop.jul.hrly[[paste0("vd.", scheme)]][daytime_def], stella11LaCape.hrly[[paste0("vd.", scheme)]][daytime_def], stella11Lama.hrly[[paste0("vd.", scheme)]][daytime_def],stella11Grig.hrly[[paste0("vd.", scheme)]][daytime_def], 
             mey98bond.fast.hrly[[paste0("vd.", scheme)]][daytime_def], mey98nash.fast.hrly[[paste0("vd.", scheme)]][daytime_def], coyle09.hrly[[paste0("vd.", scheme)]][daytime_def], pilegaard98.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# -----------------------------------------------------------------------------
# rainforest
seasonl = list("wet", "wet", "wet", "dry", "wet")
obslist = list(0.1*fowl.2011.july.rainf$o3vd, 0.1*fowl.2011.apr.rainf$o3vd, rum.wet.2007.rainf$o3vd, rum.dry.2007.rainf$o3vd, fan.1990.rainf$o3vd)
modlist.w98 = list(danum.summer.hrly$vd.w98, danum.spring.hrly$vd.w98, rum07.spring.hrly$vd.w98, rum07.fall.hrly$vd.w98, fan90.spring.hrly$vd.w98)
modlist.w98fbb = list(danum.summer.hrly$vd.w98fbb, danum.spring.hrly$vd.w98fbb, rum07.spring.hrly$vd.w98fbb, rum07.fall.hrly$vd.w98fbb, fan90.spring.hrly$vd.w98fbb)
modlist.w98med = list(danum.summer.hrly$vd.w98med, danum.spring.hrly$vd.w98med, rum07.spring.hrly$vd.w98med, rum07.fall.hrly$vd.w98med, fan90.spring.hrly$vd.w98med)
modlist.zh = list(danum.summer.hrly$vd.zh, danum.spring.hrly$vd.zh, rum07.spring.hrly$vd.zh, rum07.fall.hrly$vd.zh, fan90.spring.hrly$vd.zh)
modlist.zhfbb = list(danum.summer.hrly$vd.zhfbb, danum.spring.hrly$vd.zhfbb, rum07.spring.hrly$vd.zhfbb, rum07.fall.hrly$vd.zhfbb, fan90.spring.hrly$vd.zhfbb)
modlist.zhmed = list(danum.summer.hrly$vd.zhmed, danum.spring.hrly$vd.zhmed, rum07.spring.hrly$vd.zhmed, rum07.fall.hrly$vd.zhmed, fan90.spring.hrly$vd.zhmed)

# wet ----
tmp = list(0.1*fowl.2011.july.rainf$o3vd, rum.wet.2007.rainf$o3vd, fan.1990.rainf$o3vd, 0.1*fowl.2011.apr.rainf$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(danum.summer.hrly[[paste0("vd.", scheme)]],rum07.spring.hrly[[paste0("vd.", scheme)]],fan90.spring.hrly[[paste0("vd.", scheme)]], danum.spring.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# dry ----
tmp = list(rum.dry.2007.rainf$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(rum07.fall.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# all ----
tmp = list(0.1*fowl.2011.july.rainf$o3vd, rum.wet.2007.rainf$o3vd, fan.1990.rainf$o3vd, 0.1*fowl.2011.apr.rainf$o3vd,rum.dry.2007.rainf$o3vd)
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(danum.summer.hrly[[paste0("vd.", scheme)]],rum07.spring.hrly[[paste0("vd.", scheme)]],
             fan90.spring.hrly[[paste0("vd.", scheme)]], danum.spring.hrly[[paste0("vd.", scheme)]],rum07.fall.hrly[[paste0("vd.", scheme)]])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

# daytime
tmp = list(0.1*fowl.2011.july.rainf$o3vd[daytime_def], rum.wet.2007.rainf$o3vd[daytime_def], fan.1990.rainf$o3vd[daytime_def], 
           0.1*fowl.2011.apr.rainf$o3vd[daytime_def],rum.dry.2007.rainf$o3vd[daytime_def])
tmpobs = lapply(tmp, mean)
sd(unlist(tmpobs), na.rm = T)
mean(unlist(tmp),na.rm = T)
for (scheme in schemelist) {
  tmp = list(danum.summer.hrly[[paste0("vd.", scheme)]][daytime_def],rum07.spring.hrly[[paste0("vd.", scheme)]][daytime_def],
             fan90.spring.hrly[[paste0("vd.", scheme)]][daytime_def], danum.spring.hrly[[paste0("vd.", scheme)]][daytime_def],rum07.fall.hrly[[paste0("vd.", scheme)]][daytime_def])
  tmpmod = lapply(tmp, mean)
  sd = sd(unlist(tmpmod))
  print(scheme)
  print(mean(unlist(tmp),na.rm = T))
  print(sd)
}

































