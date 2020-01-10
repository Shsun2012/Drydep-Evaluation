
library(ggpubr)

# ---------------------------------------------------------------------------------------------------------
# load Ducker et al. 2018 data
# monthly average Gs
synflux.day = read.csv('~/Dropbox/DryDepEval/dryDepData/Ducker_et_al_2018_Supplementary_Material/SynFlux_monthly_gapfill_day_header_v1.1.csv', header = T, stringsAsFactors = F)
synflux.day.prev = read.csv('~/Dropbox/DryDepEval/dryDepData/Ducker_et_al_2018_Supplementary_Material/Synflux_day_header.csv', header = T, stringsAsFactors = F)
synflux.day[which(synflux.day$site == 'Var'),]$site = "US-Var"
fluxnet.site = unique(synflux.day[c('site')])
fluxnet.site$site = sort(fluxnet.site$site)
test = read.csv('~/Dropbox/DryDepEval/SynFlux_sites_mod.csv')
test = test[c('SiteID','Latitude','Longitude','Period')]
test = cbind(test, site = fluxnet.site$site)
test$check = 0
test$check = mapply(check.identical, test$SiteID, test$site)
synflux.day[which(synflux.day$site == 'IT-Co1'),]$site = 'IT-Col'
synflux.day[which(synflux.day$site == 'ES-Lgs'),]$site = 'ES-LgS'

synflx.site.latlon = read.csv('~/Dropbox/DryDepEval/SynFlux_sites_mod.csv', stringsAsFactors = F)
synflx.site.latlon = synflx.site.latlon[c('SiteID','Latitude','Longitude','Period','PFT')]
colnames(synflx.site.latlon)[1] = 'site'
synflx.site.latlon$startyear = as.integer(substr(synflx.site.latlon$Period, 1, 4))
synflx.site.latlon$endyear = as.integer(substr(synflx.site.latlon$Period, 6, 9))
# pft ---
synflx.site.latlon$PFT.no = NA
pft_number = function(pft_name, latitude){
  if (pft_name == "GRA") { # grassland
    pft_no = 14 # c3 non-arctic grass
    if (latitude <= 80) {
      pft_no = 14 # non-arctic
    }
    if (latitude > 80) {
      pft_no = 13 # arctic
    }
  }
  if (pft_name == "ENF"){ # evergreen needleleaf forest
    pft_no = 3 # needleleaf evergreen boreal tree
  }
  if (pft_name == "EBF") { # evergreen broadleaf forest
    pft_no = 5 # broadleaf evergreen temperate tree
  }
  if (pft_name == "DBF") { # deciduous broadleaf forest
    # broadleaf deciduous tree (tropical/temperal/boreal)
    if (latitude <= 30) {
      pft_no = 6 # tropical 
    }
    if (latitude > 30 & latitude <= 60) {
      pft_no = 8 # temperate
    }
    if (latitude > 60) {
      pft_no = 9 # boreal
    }
  }
  if (pft_name == "MF") { # mixed forest
    pft_no = 8 # no pft for mixed forest in clm
  }
  if (pft_name == "WSA") { # woody savanna
    pft_no = 7 # broadleaf deciduous tropical tree
  }
  if (pft_name == "OSH"){ # open shrubland (dense foliage cover)
    pft_no = 11 # broadleaf deviduous temperate shrub
  }
  if (pft_name == "CSH"){ # closed shrubland (mid-dense foliage cover)
    pft_no = 12 # broadleaf deciduous boreal shrub
  }
  if (pft_name == "CRO") { # crop
    # pft_no = 16 # c3 crop
    pft_no = 17 # c3 irrigated
    # pft_no = 18 # corn
    # pft_no = 19 # irrigated corn
  }
  if (pft_name == "WET") { # wetland
    pft_no = 10 # no wetland pft in clm, use broadleaf evergreen shurb
  }
  return(pft_no)
}

synflx.site.latlon$PFT.no = mapply(pft_number, synflx.site.latlon$PFT, synflx.site.latlon$Latitude)

# ------------------- load simulations ------------------------------------
# load TEMIR simulated Gs with default parameters and MAP parameters
load('synflx_data.RData')


# ------------------- plot ---------------------
site_pft = synflx.site.latlon[c('site','PFT')]
# function to plot scatter plot temir-synflux
plot_syn = function(syn_df = NA, title = NA, year_avg = F, summer = F, ylim = 2, text_x = 1.5, pft = NA, site_avg = F, title.x = 0.25){
  test = syn_df[which(!is.na(syn_df[,ncol(syn_df)])),]
  if (summer) {
    test.jja = test[which(test$month >=6 & test$month <= 8),]
  }else{
    test.jja = test
  }
  
  df = test.jja[c(1,2,3,4,ncol(test.jja))]
  df = cbind(test.jja[c(1:4)], test.jja[,ncol(test.jja)])
  names(df) = c('site','year','month','gs','gs_temir')
  df$gs_temir = 100*as.numeric(df$gs_temir)
  df[df==-9999] = NA
  df = df[complete.cases(df),]
  # filter
  # df = df[which(df$gs_temir < 1.5),]
  
  if (year_avg) {
    df = aggregate(df[c('gs','gs_temir')], by = list(df$site,df$year), mean, na.rm = T)
    names(df)[1:2] = c('site','year')
  }
  
  if (site_avg) {
    df = aggregate(df[c('gs','gs_temir')], by = list(df$site), mean, na.rm = T)
    names(df)[1] = 'site'
  }
  
  #df = df[which(df$PFT != 'OSH' & df$PFT != 'WET' & df$PFT != 'MF' & df$PFT != 'CRO'),]
  df = merge(df, site_pft, by = c('site'))
  df = df[which(df$PFT != 'WET' & df$PFT != 'MF' & df$PFT != 'OSH'),]
  if (!is.na(pft)) {
    df = df[which(df$PFT == pft),]
  }
  
  nmbf = calNMBF(df$gs_temir,df$gs)
  nmaef = calNMAEF(df$gs_temir,df$gs)
  tmp.lm = lm(gs ~ gs_temir, data = df)
  lab.rsquare = paste("R^2 == ",format(round(summary(tmp.lm)$r.squared, 2),nsmall = 3))
  
  text1.y = ylim - 0.1
  text2.y = 0.1 
  text3.y = 0.3
  text4.y = 0.5
  
  p <- ggplot(df, aes(x = gs, y = gs_temir, color = PFT))+
    geom_abline(slope = 1, size = 2)+
    geom_point(size=3)+
    scale_x_continuous(limits = c(0, ylim))+
    scale_y_continuous(limits = c(0, ylim))+
    theme_bw()+
    theme(legend.position = 'none')+
    ylab(bquote('Modelled'~G[s]~ cm ~s^-1))+
    xlab(bquote('SynFlux'~G[s]~ cm ~s^-1))+
    theme(axis.text.x = element_text(colour = "grey20",size=15),
          axis.text.y = element_text(colour = "grey20",size=15))+
    theme(axis.title = element_text(face = "bold", size = 16))+
    #theme(plot.title = element_text(face = "bold", size = 16))+
    theme(legend.text = element_text(size = 20), legend.title = element_blank())+
    annotate("text", x = text_x, y = c(text3.y, text2.y), label = c(paste("NMBF = ",format(round(nmbf, 2),nsmall = 2)), 
                                                                    paste("NMAEF = ",format(round(nmaef, 2), nsmall = 2))), size = 6.5, colour = 'grey20')+
    annotate("text", x = text_x, y = c(text4.y), label = lab.rsquare, parse = T, size = 6.5, colour = 'grey20')+
    annotate("text", x = title.x, y = text1.y, label = title, size = 10, colour = 'grey20')
  
  return(p)
}


###  plot with different options
# 
p_zhmed = plot_syn(syn_df = z03med_syn, title = 'MED', year_avg = T, summer = F)
p_zhfbb = plot_syn(syn_df = z03fbb_syn, title = 'FBB', year_avg = T, summer = F)
p_zh = plot_syn(syn_df = z03_syn, title = 'Z03', year_avg = T, summer = F)
p_w98 = plot_syn(syn_df = w89_syn, title = 'W89', year_avg = T, summer = F)
ggarrange(p_zh, p_zhfbb, p_zhmed, p_w98, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")

# average each site 
p_zhmed = plot_syn(syn_df = z03med_syn, title = 'MED', year_avg = T, summer = F, site_avg = T)
p_zhfbb = plot_syn(syn_df = z03fbb_syn, title = 'FBB', year_avg = T, summer = F, site_avg = T)
p_zh = plot_syn(syn_df = z03_syn, title = 'Z03', year_avg = T, summer = F, site_avg = T)
p_w98 = plot_syn(syn_df = w89_syn, title = 'W89', year_avg = T, summer = F, site_avg = T)
ggpubr::ggarrange(p_zh, p_zhfbb, p_zhmed, p_w98, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")



# ---- plot each pft -----
# DBF
p_zhmed = plot_syn(syn_df = z03med_syn, title = 'MED', year_avg = T, summer = F, pft = 'DBF', site_avg = T)
p_zhfbb = plot_syn(syn_df = z03fbb_syn, title = 'FBB', year_avg = T, summer = F, pft = 'DBF', site_avg = T)
p_zh = plot_syn(syn_df = z03_syn, title = 'Z03', year_avg = T, summer = F, pft = 'DBF', site_avg = T)
p_w98 = plot_syn(syn_df = w89_syn, title = 'W89', year_avg = T, summer = F, pft = 'DBF', site_avg = T)
ggarrange(p_zh, p_zhfbb, p_zhmed, p_w98, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")

# ENF
p_zhmed = plot_syn(syn_df = z03med_syn, title = 'MED', year_avg = T, summer = F, pft = 'ENF', site_avg = T)
p_zhfbb = plot_syn(syn_df = z03fbb_syn, title = 'FBB', year_avg = T, summer = F, pft = 'ENF', site_avg = T)
p_zh = plot_syn(syn_df = z03_syn, title = 'Z03', year_avg = T, summer = F, pft = 'ENF', site_avg = T)
p_w98 = plot_syn(syn_df = w89_syn, title = 'W89', year_avg = T, summer = F, pft = 'ENF', site_avg = T)
ggpubr::ggarrange(p_zh, p_zhfbb, p_zhmed, p_w98, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")

# CRO
p_zhmed = plot_syn(syn_df = z03med_syn, title = 'MED', year_avg = T, summer = F, pft = 'CRO', site_avg = T)
p_zhfbb = plot_syn(syn_df = z03fbb_syn, title = 'FBB', year_avg = T, summer = F, pft = 'CRO', site_avg = T)
p_zh = plot_syn(syn_df = z03_syn, title = 'Z03', year_avg = T, summer = F, pft = 'CRO', site_avg = T)
p_w98 = plot_syn(syn_df = w89_syn, title = 'W89', year_avg = T, summer = F, pft = 'CRO', site_avg = T)
ggarrange(p_zh, p_zhfbb, p_zhmed, p_w98, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")

# GRA
p_zhmed = plot_syn(syn_df = z03med_syn, title = 'MED', year_avg = T, summer = F, pft = 'GRA', site_avg = T)
p_zhfbb = plot_syn(syn_df = z03fbb_syn, title = 'FBB', year_avg = T, summer = F, pft = 'GRA', site_avg = T)
p_zh = plot_syn(syn_df = z03_syn, title = 'Z03', year_avg = T, summer = F, pft = 'GRA', site_avg = T)
p_w98 = plot_syn(syn_df = w89_syn, title = 'W89', year_avg = T, summer = F, pft = 'GRA', site_avg = T)
ggarrange(p_zh, p_zhfbb, p_zhmed, p_w98, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")


