# function to run retrospecitve analysis
#adapted from P Hulson: https://github.com/pete-hulson/goa_pcod/blob/master/2024/R/mdl_anlys_fcns.R#L18

# setup ----
libs <- c("r4ss", "here", "tidyverse", 'viridis', 'patchwork')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Default with no version downloads the latest release
# r4ss::get_ss3_exe()

AYR <- 2026

# run retrospectives----
ret_yr <- 10

# M14_2d1
r4ss::retro(dir = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', "mgmt", 'M14_2d1'),
            years = 0:-ret_yr,
            exe = 'ss')

# M25_3
r4ss::retro(dir = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', "mgmt", 'M25_3'),
            years = 0:-ret_yr,
            exe = 'ss')

# summarize the results----
# load the retrospective models
retro_m14_2d1 <- r4ss::SSgetoutput(dirvec = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', "mgmt", 'M14_2d1', 
                                                   'retrospectives', paste("retro", 0:-ret_yr, sep = "")),
                               verbose = FALSE)
retro_m25_3 <- r4ss::SSgetoutput(dirvec = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', "mgmt", 'M25_3', 
                                                       'retrospectives', paste("retro", 0:-ret_yr, sep = "")),
                                   verbose = FALSE)
# summarize the model results (for plotting)
retrosumm_m14_2d1 <- r4ss::SSsummarize(retro_m14_2d1,
                                    verbose = FALSE)
retrosumm_m25_3 <- r4ss::SSsummarize(retro_m25_3,
                                       verbose = FALSE)

retro_res <- list(retrosumm_m14_2d1 = retrosumm_m14_2d1, retrosumm_m25_3 = retrosumm_m25_3)

# save results
  if (!dir.exists(here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', "retro"))) {
    dir.create(here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', "retro"), recursive = TRUE)
  }
  save(retro_res, file = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', "retro", "retro_res.RData"))

# Mohns Rho calc ----
rhopath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'retro')

MR14_2d1_out <- SSmohnsrho(retrosumm_m14_2d1)
MR25_3_out <- SSmohnsrho(retrosumm_m25_3)

MR14_2d1_out <- MR14_2d1_out |> 
  unlist() |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  rename(Metric = rowname,
         Rho_value = 'unlist(MR14_2d1_out)') |> 
  mutate(Model = '14_2d1')
MR25_3_out <- MR25_3_out |> 
  unlist() |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  rename(Metric = rowname,
         Rho_value = 'unlist(MR25_3_out)') |> 
  mutate(Model = '25_3')

rhoout <- MR25_3_out |> 
  bind_rows(MR14_2d1_out) |> 
  pivot_wider(names_from = Model, values_from = Rho_value)

write_csv(rhoout, here::here(rhopath, paste0(AYR, 'Rho_model_comparison.csv')))

# retrospective plots----
# TODO make this into a function
# retro M14_2d1----
endyrvec <-retrosumm_m14_2d1$endyrs + 0:-10

#SSB
baserun <- retrosumm_m14_2d1$SpawnBio %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d1 = replist1) %>% 
  select(M14_2d1, Yr)

retrobioLL <- retrosumm_m14_2d1$SpawnBioLower %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d1 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_SSBLL')
retrobioUL <- retrosumm_m14_2d1$SpawnBioUpper %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d1 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_SSBUL')

retrobio <- retrosumm_m14_2d1$SpawnBio %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d1 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_SSB') %>% 
  left_join(baserun) %>% 
  mutate(Peel_diff = (Peel_SSB-M14_2d1)/M14_2d1) %>% 
  left_join(retrobioLL) %>% 
  left_join(retrobioUL)

models <- unique(retrobio$Peel)

for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_SSB <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_diff <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_SSBLL <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_SSBUL <- NA}

#recruits
baserun_rec <- retrosumm_m14_2d1$recruits %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d1 = replist1) %>% 
  select(M14_2d1, Yr)

retrorecLL <- retrosumm_m14_2d1$recruitsLower %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d1 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_recLL')
retrorecUL <- retrosumm_m14_2d1$recruitsUpper %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d1 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_recUL')

retrorec <- retrosumm_m14_2d1$recruits %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d1 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_rec') %>% 
  left_join(baserun) %>% 
  mutate(Peel_diff = (Peel_rec-M14_2d1)/M14_2d1) %>% 
  left_join(retrorecLL) %>% 
  left_join(retrorecUL)

models <- unique(retrorec$Peel)

for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_rec <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_diff <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_recLL <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_recUL <- NA}

#SSB Plots
plot_retrossb <- ggplot(retrobio, aes(x = Yr, y = Peel_SSB/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_SSBLL/1000, ymax = Peel_SSBUL/1000, fill = Peel), 
              show.legend = F, alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "Spawning Biomass (1,000s t)", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrossb10 <- ggplot(retrobio[retrobio$Yr >= 2015,], aes(x = Yr, y = Peel_SSB/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_SSBLL/1000, ymax = Peel_SSBUL/1000, fill = Peel), 
              show.legend = F, alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_continuous(breaks =c(2015, 2017, 2019, 2021, 2023, 2025))+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrodiff <- ggplot(retrobio, aes(x = Yr, y = Peel_diff, color = Peel))+
  geom_line()+
  labs(y = "SSB Relative Difference", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrodiff10 <- ggplot(retrobio[retrobio$Yr >= 2015,], aes(x = Yr, y = Peel_diff, color = Peel))+
  geom_line()+
  labs(y = "", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_continuous(breaks =c(2015, 2017, 2019, 2021, 2023, 2025))+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
#Recruitment Plots
plot_retrorec <- ggplot(retrorec, aes(x = Yr, y = Peel_rec/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_recLL/1000, ymax = Peel_recUL/1000, fill = Peel), 
              show.legend = F, alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "Recruits (1,000s t)", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrorec10 <- ggplot(retrorec[retrorec$Yr >= 2015,], aes(x = Yr, y = Peel_rec/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_recLL/1000, ymax = Peel_recUL/1000, fill = Peel), 
              show.legend = F, alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_continuous(breaks =c(2015, 2017, 2019, 2021, 2023, 2025))+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrorecdiff <- ggplot(retrobio, aes(x = Yr, y = Peel_diff, color = Peel))+
  geom_line()+
  labs(y = "Recruitment Relative Difference", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.04, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE))

plot_retro <- (plot_retrossb / plot_retrorec / plot_retrodiff) +
  plot_layout(guides = "collect") #& theme(legend.position = 'bottom') #theme throws an error now, but I don't think we need a legend
plot_retro10 <- ((plot_retrossb + plot_retrossb10) /
                   (plot_retrorec + plot_retrorec10) /
                   (plot_retrodiff + plot_retrodiff10)) +
  plot_layout(guides = "collect") #& theme(legend.position = 'bottom')
ggsave(path = rhopath,
       "M14_2d1Combined_retro_plot.png",plot=plot_retro10,dpi=600,width = 6, height = 8)

# retro M25_3----
endyrvec <-retrosumm_m25_3$endyrs + 0:-10

#SSB
baserun <- retrosumm_m25_3$SpawnBio %>% 
  filter(Yr <= AYR) %>% 
  rename(M25_3 = replist1) %>% 
  select(M25_3, Yr)

retrobioLL <- retrosumm_m25_3$SpawnBioLower %>% 
  filter(Yr <= AYR) %>% 
  rename(M25_3 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_SSBLL')
retrobioUL <- retrosumm_m25_3$SpawnBioUpper %>% 
  filter(Yr <= AYR) %>% 
  rename(M25_3 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_SSBUL')

retrobio <- retrosumm_m25_3$SpawnBio %>% 
  filter(Yr <= AYR) %>% 
  rename(M25_3 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_SSB') %>% 
  left_join(baserun) %>% 
  mutate(Peel_diff = (Peel_SSB-M25_3)/M25_3) %>% 
  left_join(retrobioLL) %>% 
  left_join(retrobioUL)

models <- unique(retrobio$Peel)

for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_SSB <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_diff <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_SSBLL <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_SSBUL <- NA}

#recruits
baserun_rec <- retrosumm_m25_3$recruits %>% 
  filter(Yr <= AYR) %>% 
  rename(M25_3 = replist1) %>% 
  select(M25_3, Yr)

retrorecLL <- retrosumm_m25_3$recruitsLower %>% 
  filter(Yr <= AYR) %>% 
  rename(M25_3 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_recLL')
retrorecUL <- retrosumm_m25_3$recruitsUpper %>% 
  filter(Yr <= AYR) %>% 
  rename(M25_3 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_recUL')

retrorec <- retrosumm_m25_3$recruits %>% 
  filter(Yr <= AYR) %>% 
  rename(M25_3 = replist1,
         run2025 = replist2,
         run2024 = replist3,
         run2023 = replist4,
         run2022 = replist5,
         run2021 = replist6,
         run2020 = replist7,
         run2019 = replist8,
         run2018 = replist9,
         run2017 = replist10) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_rec') %>% 
  left_join(baserun) %>% 
  mutate(Peel_diff = (Peel_rec-M25_3)/M25_3) %>% 
  left_join(retrorecLL) %>% 
  left_join(retrorecUL)

models <- unique(retrorec$Peel)

for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_rec <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_diff <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_recLL <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_recUL <- NA}

#SSB Plots
plot_retrossb <- ggplot(retrobio, aes(x = Yr, y = Peel_SSB/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_SSBLL/1000, ymax = Peel_SSBUL/1000, fill = Peel), 
              show.legend = F, alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "Spawning Biomass (1,000s t)", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrossb10 <- ggplot(retrobio[retrobio$Yr >= 2015,], aes(x = Yr, y = Peel_SSB/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_SSBLL/1000, ymax = Peel_SSBUL/1000, fill = Peel), 
              show.legend = F, alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_continuous(breaks =c(2015, 2017, 2019, 2021, 2023, 2025))+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrodiff <- ggplot(retrobio, aes(x = Yr, y = Peel_diff, color = Peel))+
  geom_line()+
  labs(y = "SSB Relative Difference", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrodiff10 <- ggplot(retrobio[retrobio$Yr >= 2015,], aes(x = Yr, y = Peel_diff, color = Peel))+
  geom_line()+
  labs(y = "", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_continuous(breaks =c(2015, 2017, 2019, 2021, 2023, 2025))+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
#Recruitment Plots
plot_retrorec <- ggplot(retrorec, aes(x = Yr, y = Peel_rec/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_recLL/1000, ymax = Peel_recUL/1000, fill = Peel), 
              show.legend = F, alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "Recruits (1,000s t)", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrorec10 <- ggplot(retrorec[retrorec$Yr >= 2015,], aes(x = Yr, y = Peel_rec/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_recLL/1000, ymax = Peel_recUL/1000, fill = Peel), 
              show.legend = F, alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_continuous(breaks =c(2015, 2017, 2019, 2021, 2023, 2025))+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrorecdiff <- ggplot(retrobio, aes(x = Yr, y = Peel_diff, color = Peel))+
  geom_line()+
  labs(y = "Recruitment Relative Difference", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.04, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE))

plot_retro <- (plot_retrossb / plot_retrorec / plot_retrodiff) +
  plot_layout(guides = "collect") #& theme(legend.position = 'bottom') #theme throws an error now, but I don't think we need a legend
plot_retro10 <- ((plot_retrossb + plot_retrossb10) /
                   (plot_retrorec + plot_retrorec10) /
                   (plot_retrodiff + plot_retrodiff10)) +
  plot_layout(guides = "collect") #& theme(legend.position = 'bottom')
ggsave(path = rhopath,
       "M25_3Combined_retro_plot.png",plot=plot_retro10,dpi=600,width = 6, height = 8)
