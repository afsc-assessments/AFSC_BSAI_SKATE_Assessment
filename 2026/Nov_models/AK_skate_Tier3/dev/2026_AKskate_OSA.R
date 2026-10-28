# OSA residuals
# developed by C Tribuzio October 2026
# using afscOSA https://noaa-afsc.github.io/afscOSA/index.html

#TODO

# downloading compResidual:
# https://github.com/fishfollower/compResidual#composition-residuals for
# installation instructions

# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# remotes::install_github("fishfollower/compResidual/compResidual", force=TRUE)

# remotes::install_github("noaa-afsc/afscOSA", force=TRUE)

# library(afscOSA)


# setup ----
libs <- c("r4ss", "here", "tidyverse", 'afscOSA')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Default with no version downloads the latest release
# r4ss::get_ss3_exe()

AYR <- 2026

#exe_loc <- here::here(paste0(AYR, '/Nov_models/AK_skate_Tier3/ss.exe'))

sx = 1 # USER INPUT define sex
fleet = c(1, 2, 3) # USER INPUT define fleets

##########
# Model 14_2d1----
# base model
M14_2d1_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M14_2d1')
M14_2d1_out <- SSgetoutput(dirvec = M14_2d1_path)

# comps for the fleets defined in "fleet" and "sx"
comps <- as.data.frame(M14_2d1_out[[1]]$lendbase[,c(1,6,13,16:18)])
comps <- comps[comps$Fleet %in% fleet & comps$Sex %in% sx, ]
comps <- reshape2::melt(comps,id.vars = c('Yr','Fleet','Sex','Bin'))
#comps <- comps |> 
#  pivot_wider(names_from = Bin, values_from = value) |> 
#  mutate(across(everything(), ~ replace_na(., 0.0000000001))) |> 
#  pivot_longer(
#    cols = !c('Yr', 'Fleet', 'Sex', 'variable'),
#    names_to = "Bin", 
#    values_to = "value"
#  )
#comps$Bin <- as.numeric(comps$Bin)
  

# input sample sizes for the fleets defined in "fleet" and "sx"
Ndf <- as.data.frame(M14_2d1_out[[1]]$lendbase[,c(1,6,13,16,20,22)])
#Ndf <- Ndf[Ndf$Bin == min(Ndf$Bin),] #doesn't work with AKskate data
# too many zeros at the beginning
Ndf <- Ndf |> 
  filter(Bin == 100)

# length bins
lens <- sort(unique(comps$Bin))

# LGL (fleet 1) ----

flt <- 1 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt,]
lens <- sort(unique(tmp$Bin))

# input sample sizes (vector)
N <- Ndf$Nsamp_adj[Ndf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])
#replace NAs with zeros
obs[is.na(obs)] <- 0.0000000001 #doesn't fix the problem

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])
exp[is.na(exp)] <- 0.0000000001


# should all be true!
stopifnot(all(length(N) == length(yrs), length(N) == nrow(obs), nrow(obs) == nrow(exp)))

outLGL <- run_osa(fleet = '14_2d1LGL', index_label = 'Length',
                         obs = obs, exp = exp, N = N, index = lens, 
                         years = yrs)

# TWL (fleet 2) ----

flt <- 2 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt,]
lens <- sort(unique(tmp$Bin))

# input sample sizes (vector)
N <- Ndf$Nsamp_adj[Ndf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])
#replace NAs with zeros
obs[is.na(obs)] <- 0.0000000001 #doesn't fix the problem

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])
exp[is.na(exp)] <- 0.0000000001


# should all be true!
stopifnot(all(length(N) == length(yrs), length(N) == nrow(obs), nrow(obs) == nrow(exp)))

outTWL <- run_osa(fleet = '14_2d1TWL', index_label = 'Length',
                  obs = obs, exp = exp, N = N, index = lens, 
                  years = yrs)

# SURV (fleet 3) ----

flt <- 3 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt,]
lens <- sort(unique(tmp$Bin))

# input sample sizes (vector)
N <- Ndf$Nsamp_adj[Ndf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])
#replace NAs with zeros
obs[is.na(obs)] <- 0.0000000001 #doesn't fix the problem

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])
exp[is.na(exp)] <- 0.0000000001


# should all be true!
stopifnot(all(length(N) == length(yrs), length(N) == nrow(obs), nrow(obs) == nrow(exp)))

outSURV <- run_osa(fleet = '14_2d1SURV', index_label = 'Length',
                  obs = obs, exp = exp, N = N, index = lens, 
                  years = yrs)

# M14_2d1 plots----
outpath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'osa')

#note use the modified plot in afscOSA_skatesR file
osaplots <- plot_osa_model(list(outLGL, outTWL, outSURV), outpath = outpath, model = '14_2d1')

##########
# Model 25_3----
M25_3_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M25_3')
M25_3_out <- SSgetoutput(dirvec = M25_3_path)

sx = 1 # USER INPUT define sex
fleet = c(1, 2, 3) # USER INPUT define fleets

# comps for the fleets defined in "fleet" and "sx"
comps <- as.data.frame(M25_3_out[[1]]$lendbase[,c(1,6,13,16:18)])
comps <- comps[comps$Fleet %in% fleet & comps$Sex %in% sx, ]
comps <- reshape2::melt(comps,id.vars = c('Yr','Fleet','Sex','Bin'))

# input sample sizes for the fleets defined in "fleet" and "sx"
Ndf <- as.data.frame(M25_3_out[[1]]$lendbase[,c(1,6,13,16,20,22)])
#Ndf <- Ndf[Ndf$Bin == min(Ndf$Bin),] #doesn't work with AKskate data
# too many zeros at the beginning
Ndf <- Ndf |> 
  filter(Bin == 100)

# length bins
lens <- sort(unique(comps$Bin))

# LGL (fleet 1) ----

flt <- 1 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt,]
lens <- sort(unique(tmp$Bin))

# input sample sizes (vector)
N <- Ndf$Nsamp_adj[Ndf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])
#replace NAs with zeros
obs[is.na(obs)] <- 0.0000000001 #doesn't fix the problem

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])
exp[is.na(exp)] <- 0.0000000001


# should all be true!
stopifnot(all(length(N) == length(yrs), length(N) == nrow(obs), nrow(obs) == nrow(exp)))

outLGL <- run_osa(fleet = 'M25_3LGL', index_label = 'Length',
                  obs = obs, exp = exp, N = N, index = lens, 
                  years = yrs)

# TWL (fleet 2) ----

flt <- 2 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt,]
lens <- sort(unique(tmp$Bin))

# input sample sizes (vector)
N <- Ndf$Nsamp_adj[Ndf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])
#replace NAs with zeros
obs[is.na(obs)] <- 0.0000000001 #doesn't fix the problem

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])
exp[is.na(exp)] <- 0.0000000001


# should all be true!
stopifnot(all(length(N) == length(yrs), length(N) == nrow(obs), nrow(obs) == nrow(exp)))

outTWL <- run_osa(fleet = 'M25_3TWL', index_label = 'Length',
                  obs = obs, exp = exp, N = N, index = lens, 
                  years = yrs)

# SURV (fleet 3) ----

flt <- 3 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt,]
lens <- sort(unique(tmp$Bin))

# input sample sizes (vector)
N <- Ndf$Nsamp_adj[Ndf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])
#replace NAs with zeros
obs[is.na(obs)] <- 0.0000000001 #doesn't fix the problem

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])
exp[is.na(exp)] <- 0.0000000001


# should all be true!
stopifnot(all(length(N) == length(yrs), length(N) == nrow(obs), nrow(obs) == nrow(exp)))

outSURV <- run_osa(fleet = 'M25_3SURV', index_label = 'Length',
                   obs = obs, exp = exp, N = N, index = lens, 
                   years = yrs)

# M25_3 plots----
#note use the modified plot in afscOSA_skatesR file
osaplots <- plot_osa_model(list(outLGL, outTWL, outSURV), outpath = outpath, model = 'M25_3')





