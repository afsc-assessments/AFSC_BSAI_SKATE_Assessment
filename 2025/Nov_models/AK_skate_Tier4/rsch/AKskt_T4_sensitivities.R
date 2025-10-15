# Tier 4 sbpr senstivities to M and seletivity assumptions
# Cindy Tribuzio
# Sept 24, 2025

# setup----
AYR <- 2025

#get spbr functions
source(here::here(AYR, 'Nov_models', 'AK_skate_Tier4', 'R', 'T4_sbpr_fun.R'))

# Basic AK skates setup ----
data_inputs <- list(#mat_a50 = 15.280, #Matta MS thesis
  #mat_sigmaa = 1.574, #Matta MS thesis
  vb_Linf = 135.39, #Matta and Gunderson 2007
  vb_t0 = -1.60, #Matta and Gunderson 2007
  vb_k = 0.10, #Matta and Gunderson 2007
  natM = 0.13, #current assessment value
  lw_a = 0.000009, #current assessment value
  lw_b = 2.9617, #current assessment value
  Amax = 26, #AGP data
  selex = 1, # 1= knife edge, 2= logistic #TBD for now
  selex_dat1 = 5, #TBD
  selex_dat2 = NA #tBD
)

# maturity curve set up
ages <- 0:26
mata_fem <- -15.280
matb_fem <- 1.574
maa <- 1/(1+exp(-(mata_fem+matb_fem*ages)))

#Selectivity set up
#AK skates are caught at age 0, generally have a dome shaped selectivity, best approximation would be from Kotwicki and Weinberg 2005
# See Figure 5
# https://www.adfg.alaska.gov/static/home/library/PDFs/afrb/kotwv11n2.pdf
selexF <- c(seq(0.1, 1, 0.2), rep(1, 16), rev(seq(0.5, 1, 0.1)))
length(selexF)
plot(selexF) #Goal is for a reasonable dome shaped curve

#set up M vector
Mvec <- seq(0.05, 0.2, 0.01)

# M sensitivities----
# setup the loop
outmat <- matrix(nrow = length(Mvec), ncol = 3)
colnames(outmat) <- c('Mval', 'F40', 'F35')

for(i in 1:length(Mvec)){
  res <- run_spr_fun(F_vec = seq(0, 1, by = 0.001),
                     R0 = 1,
                     ages = 0:26,
                     sex_ratio = 0.5,
                     M = Mvec[i], #loop through Mvec
                     Linf = data_inputs$vb_Linf, # cm
                     k = data_inputs$vb_k,
                     t0 = data_inputs$vb_t0,
                     alpha =  data_inputs$lw_a,
                     beta = data_inputs$lw_b,
                     maa = maa,
                     selex = selexF)
  outmat[i, 1] <- Mvec[i]
  outmat[i, 2] <- res$F40
  outmat[i, 3] <- res$F35
}

out2 <- as.data.frame(outmat) %>% 
  pivot_longer(!Mval, names_to = "Ftype", values_to = "Frate")
Massess <- as.data.frame(outmat) %>% 
  filter(Mval == 0.13)
ggplot(out2, aes(x = Mval, y = Frate, color = Ftype))+
  geom_point(size = 3)+
  geom_line()+
  geom_vline(xintercept = 0.13)+
  coord_cartesian(ylim = c(0, 0.1))+
  labs(x = "Natural Mortality (M)", y = "F rate", colour = "", title = "Tier 4 Sensitivity to Natural Mortality")

# selexF sensitivities----
# base selectivity curve
# define the linear ramp up, starting at 0.5 at age = 0, ending at 1 at age = 10
srupval <- 0.5
srupage <- 0
erupage <- 10
erupval <- 1
startramp <- seq(srupval, erupval, (erupval - srupval)/(erupage-srupage))

#define the descending ramp
srdnval <- 1
srdnage <- 15
erdnage <- 26
erdnval <- 0.5
endramp <- seq(srdnval, erdnval, (erdnval - srdnval)/(erdnage-srdnage))

#put the selectivity curve together
topwidth <- (srdnage - erupage)-1
selexF <- c(startramp, rep(1, topwidth), endramp)
length(selexF)
plot(selexF) 

#plan for selectivity sensitivies
#1) steepness of ascending ramp (i.e., widen top width to earlier ages)
#2) steepness of descending ramp (i.e., widen top width to later ages)
#3) starting value
#4) lower peak value

# acending ramp----
erupage <- seq(1, srdnage-1, 1)
outmat <- matrix(nrow = length(erupage), ncol = 3)
colnames(outmat) <- c('erupage', 'F40', 'F35')
sout <- matrix(ncol = length(erupage), nrow = length(0:26))
colnames(sout) <- c(erupage)

for(i in 1:length(erupage)){
  #loops through making steeper starting ramp
  startramp <- seq(srupval, erupval, (erupval - srupval)/(erupage[i]-srupage))
  topwidth <- if_else((srdnage - erupage[i])-1 < 0, 1,
                      (srdnage - erupage[i])-1)
  selexF <- c(startramp, rep(1, topwidth), endramp)
  res <- run_spr_fun(F_vec = seq(0, 1, by = 0.001),
                     R0 = 1,
                     ages = 0:26,
                     sex_ratio = 0.5,
                     M = 0.13, 
                     Linf = data_inputs$vb_Linf, # cm
                     k = data_inputs$vb_k,
                     t0 = data_inputs$vb_t0,
                     alpha =  data_inputs$lw_a,
                     beta = data_inputs$lw_b,
                     maa = maa,
                     selex = selexF)
  outmat[i, 1] <- erupage[i]
  outmat[i, 2] <- res$F40
  outmat[i, 3] <- res$F35
  sout[,i] <- selexF
}

sout <- as.data.frame(sout) %>% 
  mutate(age = 0:26) %>% 
  pivot_longer(!age, names_to = 'endrampage', values_to = 'proportion')
ggplot(sout, aes(x = age, y = proportion, color = endrampage))+
  geom_line(show.legend = F)+
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 26))+
  labs(x = "Age", y = "Selectivity Proportion at Age", title = 'Tier 4 Start Ramps')

out2 <- as.data.frame(outmat) %>% 
  pivot_longer(!erupage, names_to = "Ftype", values_to = "Frate")
ggplot(out2, aes(x = erupage, y = Frate, color = Ftype))+
  geom_point(size = 3)+
  geom_line()+
  geom_vline(xintercept = 10)+
  coord_cartesian(ylim = c(0, 0.1), xlim = c(min(erupage), max(erupage)))+
  labs(x = "End age of start ramp", y = "F rate", colour = "", title = "Tier 4 Sensitivity to start ramp")

# descending ramp----
# remember to reset to starting values in the base selectivity section
srdnage <- seq(erupage+1, 25, 1)
outmat <- matrix(nrow = length(srdnage), ncol = 3)
colnames(outmat) <- c('srdnage', 'F40', 'F35')
sout <- matrix(ncol = length(srdnage), nrow = length(0:26))
colnames(sout) <- c(srdnage)

for(i in 1:length(srdnage)){
  #loops through making steeper starting ramp
  endramp <- seq(srdnval, erdnval, (erdnval - srdnval)/(erdnage-srdnage[i]))
  topwidth <- if_else((srdnage[i] - erupage)-1 < 0, 1,
                      (srdnage[i] - erupage)-1)
  selexF <- c(startramp, rep(1, topwidth), endramp)
  res <- run_spr_fun(F_vec = seq(0, 1, by = 0.001),
                     R0 = 1,
                     ages = 0:26,
                     sex_ratio = 0.5,
                     M = 0.13, 
                     Linf = data_inputs$vb_Linf, # cm
                     k = data_inputs$vb_k,
                     t0 = data_inputs$vb_t0,
                     alpha =  data_inputs$lw_a,
                     beta = data_inputs$lw_b,
                     maa = maa,
                     selex = selexF)
  outmat[i, 1] <- srdnage[i]
  outmat[i, 2] <- res$F40
  outmat[i, 3] <- res$F35
  sout[,i] <- selexF
}

sout <- as.data.frame(sout) %>% 
  mutate(age = 0:26) %>% 
  pivot_longer(!age, names_to = 'startrampage', values_to = 'proportion')
ggplot(sout, aes(x = age, y = proportion, color = startrampage))+
  geom_line(show.legend = F)+
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 26))+
  labs(x = "Age", y = "Selectivity Proportion at Age", title = 'Tier 4 End Ramps')

out2 <- as.data.frame(outmat) %>% 
  pivot_longer(!srdnage, names_to = "Ftype", values_to = "Frate")
ggplot(out2, aes(x = srdnage, y = Frate, color = Ftype))+
  geom_point(size = 3)+
  geom_line()+
  geom_vline(xintercept = 15)+
  coord_cartesian(ylim = c(0, 0.1), xlim = c(min(srdnage), max(srdnage)))+
  labs(x = "Start age of end ramp", y = "F rate", colour = "", title = "Tier 4 Sensitivity to end ramp")

# starting value----
srupval <- seq(0, 1, 0.1)
outmat <- matrix(nrow = length(srupval), ncol = 3)
colnames(outmat) <- c('srupval', 'F40', 'F35')
sout <- matrix(ncol = length(srupval), nrow = length(0:26))
colnames(sout) <- c(srupval)

for(i in 1:length(srupval)){
  #loops through making steeper starting ramp
  startramp <- seq(srupval[i], erupval, (erupval - srupval[i])/(erupage-srupage))
  if(srupval[i] == erupval){
    selexF = c(rep(1, topwidth + 11), endramp)
    } else {
      selexF = c(startramp, rep(1, topwidth), endramp)
    }
  res <- run_spr_fun(F_vec = seq(0, 1, by = 0.001),
                     R0 = 1,
                     ages = 0:26,
                     sex_ratio = 0.5,
                     M = 0.13, 
                     Linf = data_inputs$vb_Linf, # cm
                     k = data_inputs$vb_k,
                     t0 = data_inputs$vb_t0,
                     alpha =  data_inputs$lw_a,
                     beta = data_inputs$lw_b,
                     maa = maa,
                     selex = selexF)
  outmat[i, 1] <- srupval[i]
  outmat[i, 2] <- res$F40
  outmat[i, 3] <- res$F35
  sout[,i] <- selexF
}

sout <- as.data.frame(sout) %>% 
  mutate(age = 0:26) %>% 
  pivot_longer(!age, names_to = 'srupval', values_to = 'proportion')
ggplot(sout, aes(x = age, y = proportion, color = srupval))+
  geom_line(show.legend = F)+
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 26))+
  labs(x = "Age", y = "Selectivity Proportion at Age", title = 'Tier 4 Starting Values')

out2 <- as.data.frame(outmat) %>% 
  pivot_longer(!srupval, names_to = "Ftype", values_to = "Frate")
ggplot(out2, aes(x = srupval, y = Frate, color = Ftype))+
  geom_point(size = 3)+
  geom_line()+
  geom_vline(xintercept = 0.5)+
  coord_cartesian(ylim = c(0, 0.1), xlim = c(min(srupval), max(srupval)))+
  labs(x = "Starting Value for the Start Ramp", y = "F rate", colour = "", title = "Tier 4 Sensitivity to Starting Values")

# ending value----
erdnval <- rev(seq(0, 1, 0.1))
outmat <- matrix(nrow = length(erdnval), ncol = 3)
colnames(outmat) <- c('erdnval', 'F40', 'F35')
sout <- matrix(ncol = length(erdnval), nrow = length(0:26))
colnames(sout) <- c(erdnval)

for(i in 1:length(erdnval)){
  #loops through making steeper starting ramp
  endramp <- seq(srdnval, erdnval[i], (erdnval[i] - srdnval)/(erdnage-srdnage))
  if(srdnval == erdnval[i]){
    selexF = c(startramp, rep(1, topwidth + 12))
  } else {
    selexF = c(startramp, rep(1, topwidth), endramp)
  }
  res <- run_spr_fun(F_vec = seq(0, 1, by = 0.001),
                     R0 = 1,
                     ages = 0:26,
                     sex_ratio = 0.5,
                     M = 0.13, 
                     Linf = data_inputs$vb_Linf, # cm
                     k = data_inputs$vb_k,
                     t0 = data_inputs$vb_t0,
                     alpha =  data_inputs$lw_a,
                     beta = data_inputs$lw_b,
                     maa = maa,
                     selex = selexF)
  outmat[i, 1] <- erdnval[i]
  outmat[i, 2] <- res$F40
  outmat[i, 3] <- res$F35
  sout[,i] <- selexF
}

sout <- as.data.frame(sout) %>% 
  mutate(age = 0:26) %>% 
  pivot_longer(!age, names_to = 'erdnval', values_to = 'proportion')
ggplot(sout, aes(x = age, y = proportion, color = erdnval))+
  geom_line(show.legend = F)+
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 26))+
  labs(x = "Age", y = "Selectivity Proportion at Age", title = 'Tier 4 Ending Values')

out2 <- as.data.frame(outmat) %>% 
  pivot_longer(!erdnval, names_to = "Ftype", values_to = "Frate")
ggplot(out2, aes(x = erdnval, y = Frate, color = Ftype))+
  geom_point(size = 3)+
  geom_line()+
  geom_vline(xintercept = 0.5)+
  coord_cartesian(ylim = c(0, 0.1), xlim = c(min(erdnval), max(erdnval)))+
  labs(x = "Ending Value for the End Ramp", y = "F rate", colour = "", title = "Tier 4 Sensitivity to Ending Values")

# lower top value----
topval <- seq(0.6, 1, 0.1)
outmat <- matrix(nrow = length(topval), ncol = 3)
colnames(outmat) <- c('topval', 'F40', 'F35')
sout <- matrix(ncol = length(topval), nrow = length(0:26))
colnames(sout) <- c(topval)

for(i in 1:length(topval)){
  erupval <- topval[i]
  startramp <- seq(srupval, erupval, (erupval - srupval)/(erupage-srupage))
  srdnval <- topval[i]
  endramp <- seq(srdnval, erdnval, (erdnval - srdnval)/(erdnage-srdnage))
  selexF <- c(startramp, rep(topval[i], topwidth), endramp)
  res <- run_spr_fun(F_vec = seq(0, 1, by = 0.001),
                     R0 = 1,
                     ages = 0:26,
                     sex_ratio = 0.5,
                     M = 0.13, 
                     Linf = data_inputs$vb_Linf, # cm
                     k = data_inputs$vb_k,
                     t0 = data_inputs$vb_t0,
                     alpha =  data_inputs$lw_a,
                     beta = data_inputs$lw_b,
                     maa = maa,
                     selex = selexF)
  outmat[i, 1] <- topval[i]
  outmat[i, 2] <- res$F40
  outmat[i, 3] <- res$F35
  sout[,i] <- selexF
}

sout <- as.data.frame(sout) %>% 
  mutate(age = 0:26) %>% 
  pivot_longer(!age, names_to = 'topval', values_to = 'proportion')
ggplot(sout, aes(x = age, y = proportion, color = topval))+
  geom_line(show.legend = F)+
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 26))+
  labs(x = "Age", y = "Selectivity Proportion at Age", title = 'Tier 4 Maximum Selectivity at Age')

out2 <- as.data.frame(outmat) %>% 
  pivot_longer(!topval, names_to = "Ftype", values_to = "Frate")
ggplot(out2, aes(x = topval, y = Frate, color = Ftype))+
  geom_point(size = 3)+
  geom_line()+
  geom_vline(xintercept = 0.5)+
  coord_cartesian(ylim = c(0, 0.1), xlim = c(min(topval), max(topval)))+
  labs(x = "Maximum Selectivity at Age", y = "F rate", colour = "", title = "Tier 4 Sensitivity to Top Value")

# shifting pyramid----
topwidth <- 2
erupage <- seq(1, 24)

outmat <- matrix(nrow = length(erupage), ncol = 3)
colnames(outmat) <- c('pkstart', 'F40', 'F35')
sout <- matrix(ncol = length(erupage), nrow = length(0:26))
colnames(sout) <- c(erupage)

for(i in 1:length(erupage)){
  startramp <- seq(srupval, erupval, (erupval - srupval)/(erupage[i]-srupage))
  endramp <- seq(srdnval, erdnval, (erdnval - srdnval)/(erdnage-erupage[i]-1))
  selexF <- c(startramp, endramp)
  res <- run_spr_fun(F_vec = seq(0, 1, by = 0.001),
                     R0 = 1,
                     ages = 0:26,
                     sex_ratio = 0.5,
                     M = 0.13, 
                     Linf = data_inputs$vb_Linf, # cm
                     k = data_inputs$vb_k,
                     t0 = data_inputs$vb_t0,
                     alpha =  data_inputs$lw_a,
                     beta = data_inputs$lw_b,
                     maa = maa,
                     selex = selexF)
  outmat[i, 1] <- erupage[i]
  outmat[i, 2] <- res$F40
  outmat[i, 3] <- res$F35
  sout[,i] <- selexF
}

sout <- as.data.frame(sout) %>% 
  mutate(age = 0:26) %>% 
  pivot_longer(!age, names_to = 'erupage', values_to = 'proportion')
ggplot(sout, aes(x = age, y = proportion, color = erupage))+
  geom_line(show.legend = F)+
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 26))+
  labs(x = "Age", y = "Selectivity Proportion at Age", title = 'Tier 4 Shifting Pyramid')

out2 <- as.data.frame(outmat) %>% 
  pivot_longer(!pkstart, names_to = "Ftype", values_to = "Frate")
ggplot(out2, aes(x = pkstart, y = Frate, color = Ftype))+
  geom_point(size = 3)+
  geom_line()+
  geom_vline(xintercept = 10)+
  coord_cartesian(ylim = c(0, 0.1), xlim = c(min(erupage), max(erupage)))+
  labs(x = "Start of Top Peak", y = "F rate", colour = "", title = "Tier 4 Sensitivity to Shifting Pyramid")

# M and ascending ramp together----
# not reporting at this time because we aren't ready to change the M value
erupage <- seq(1, srdnage-1, 1)
Mvec <- seq(0.13, 0.2, 0.01)
unqkey <- as.data.frame(crossing(Mvec, erupage))
looplength <- nrow(crossing(Mvec, erupage))

outmat <- matrix(nrow = looplength, ncol = 4)
colnames(outmat) <- c('Mvec', 'erupage', 'F40', 'F35')
sout <- matrix(ncol = length(0:26), nrow = looplength)
colnames(sout) <- c((0:26))

for(i in 1:nrow(unqkey)){
  startramp <- seq(srupval, erupval, (erupval - srupval)/(unqkey[i,2]-srupage))
  topwidth <- if_else((srdnage - unqkey[i,2])-1 < 0, 1,
                      (srdnage - unqkey[i,2])-1)
  selexF <- c(startramp, rep(1, topwidth), endramp)
  res <- run_spr_fun(F_vec = seq(0, 1, by = 0.001),
                     R0 = 1,
                     ages = 0:26,
                     sex_ratio = 0.5,
                     M = unqkey[i,1], 
                     Linf = data_inputs$vb_Linf, # cm
                     k = data_inputs$vb_k,
                     t0 = data_inputs$vb_t0,
                     alpha =  data_inputs$lw_a,
                     beta = data_inputs$lw_b,
                     maa = maa,
                     selex = selexF)
  outmat[i, 1] <- unqkey[i,1]
  outmat[i, 2] <- unqkey[i,2]
  outmat[i, 3] <- res$F40
  outmat[i, 4] <- res$F35
  sout[i,] <- selexF
}

#sout <- as.data.frame(sout) %>% 
#  bind_cols(unqkey) %>% 
#  pivot_longer(!c(Mvec, erupage), names_to = 'age', values_to = 'proportion')
#ggplot(sout, aes(x = Mvec, y = erupage, color = erdnval))+
#  geom_line(show.legend = F)+
#  coord_cartesian(ylim = c(0, 1), xlim = c(0, 26))+
#  labs(x = "Age", y = "Selectivity Proportion at Age", title = 'Tier 4 Ending Values')

out2 <- as.data.frame(outmat) %>% 
  pivot_longer(!c(Mvec, erupage), names_to = "Ftype", values_to = "Frate") %>% 
  filter(Ftype == "F40")
ggplot(out2, aes(x = erupage, y = Frate, color = as.factor(Mvec)))+
  geom_point(size = 3)+
  geom_line()+
  geom_vline(xintercept = 10)+
  geom_hline(yintercept = 0.065)+
  geom_hline(yintercept = 0.093, color = 'blue')+
  geom_hline(yintercept = 0.13, color = 'purple')+
  annotate("text", x = 12, y = 0.135, label = "Tier 5 FOFL = 0.13", color = "purple", size = 4)+
  annotate("text", x = 12, y = 0.1, label = "2023 FOFL = 0.093", color = "blue", size = 4)+
  annotate("text", x = 12, y = 0.06, label = "Proposed Tier 4 FOFL = 0.065", color = "black", size = 4)+
  coord_cartesian(ylim = c(0, 0.15), xlim = c(min(erupage), max(erupage)))+
  labs(x = "End Age of Start Ramp", y = "FOFL", colour = "", title = "Tier 4 Sensitivity to M and start ramp")

