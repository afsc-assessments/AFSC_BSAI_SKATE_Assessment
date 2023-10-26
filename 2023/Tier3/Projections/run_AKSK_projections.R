#based on code from Maia Kapur
# https://github.com/mkapur-noaa/bsai-fhs/blob/main/2023/R/3_runProjections_makeFigsTables.R

# Setup ----
require(dplyr)
require(tidyr)
require(ggplot2)
require(here)
library(readr)
library(janitor)
library(r4ss)

this_year = lubridate::year(Sys.Date())
last_yr = this_year-1

# catches for projection years ----
###  use last 5 years' real data average
SS_catch <- read_csv(paste0(getwd(), "/Data/", this_year, "/BSAIsk_totcatch_", this_year, ".csv")) %>% 
  filter(OBS_name == "ALASKA SKATE") %>% 
  group_by(YEAR) %>% 
  summarise(catch = sum(CATCH)) %>% 
  clean_names()

##  use last 5 years' real data average
projc <- SS_catch %>% 
  filter(year  < this_year & year  > (this_year-6)) %>% 
  group_by(year) %>% 
  summarise(catch = sum(catch)) %>% 
  summarise(mean(catch)) %>% as.numeric()

catch_this_year <- SS_catch %>% 
  filter(year == this_year) %>% 
  select(catch) %>% 
  as.numeric()

yrs_spcat <- 2023:(this_year+2) ## years to infill catches
catchvec <- data.frame('year' = yrs_spcat, 'catches' = NA) 
## fill in known catches (complete years)
catchvec$catches[catchvec$year < this_year] <- 
  SS_catch$catch[SS_catch$year < this_year & SS_catch$year %in% yrs_spcat]
## fill in estimated & projected catches
catchvec$catches[catchvec$year == this_year] <- round(catch_this_year,0)
catchvec$catches[catchvec$year > this_year] <- round(projc,0)
catchvec <- as.data.frame(catchvec)

save(catchvec, file = paste0(getwd(), "/Code/Tier3/", AYR, "/Projections/", Sys.Date(), "-catches_for_proj.rdata"))

####
# test codes from other examples, not run----
# saved for future developments

# Code to produce results 
#theme_set(afscassess::theme_report())
#lapply(list.files("C:/Users/maia.kapur/Work/assessments/proj_functions/", full.names = T, pattern = ".r$"),  source)
#file.copy('C:/Users/maia.kapur/Work/assessments/proj_functions/main.exe', here('projection'), overwrite = TRUE)
#file.copy('C:/Users/maia.kapur/Work/assessments/proj_functions/tacpar.dat', here('projection'), overwrite = TRUE)

# Write proj files ----
mod_2023_dir <- paste0(getwd(), "/Code/Tier3/", AYR, "/Model_Runs/M14_2_update")
mod_2023<- SS_output(mod_2023_dir, verbose = F)
## passed to write_proj function
#NSEX=1						# number of sexes used in assessment model
#Nfishery=2					# number of fisheries(fleets) #This was set equal to 2
#fleets=2					# fleet index number (associated with commercial fishery)
#rec_age=0					# assumed age at recruitment
#max_age=25					# maximum age in model
#NAGE=length(rec_age:max_age)			# number of ages
#FY=1964 					# first year used to subset SSB, per memo this is always 1977, 1964 for consistency
#rec_FY=1964					# first year used to subset recruitment
#rec_LY_decrement=0				# value subtracted from assessment final year to subset recruitment vector
#spawn_month=6					# spawning month
#Fratios=1            				# Proportion F per fishery
#passed to write_proj_spcat
#ct_yrs=4			#Number of future catch years given to projection model
## passed to setup function
#nsims=1000			# number of projection model simulations
#nproj=14			# number of projection years ALSO USED BY get_proj_res
## passed to get_proj_res
#spp="BSAI_Alaskaskate"

#file.copy(from = here('projection','2021_projections','model_proj.dat'),
#          to = here('projection'), overwrite = T)
# write_proj(dir=paste0(getwd(), "/Code/Tier3/", AYR, "/Projections"),
#            # sdir =x,
#            data_file="Model_Proj.dat",
#            data= mod_2023,
#            NSEX=NSEX, NAGE=NAGE, Nfishery=Nfishery,
#            fleets=fleets, rec_age=rec_age, max_age=max_age, FY=FY,
#            rec_FY=rec_FY, rec_LY_decrement=rec_LY_decrement,
#            spawn_month=spawn_month, Fratios=Fratios)
## write spcat with year-specific catch info
## you might have to manually make the notes on lines 2 and 11 single lines
#write_proj_spcat(dir = paste0(getwd(), "/Code/Tier3/", AYR, "/Projections"),
#                 # sdir = here('projection'),
#                 data_file = 'spp_catch.dat', ## name of new file - must match what is in spp_catch.dat
#                 data =  mod_2023, ## model basis
#                 ct_yrs = 3, ## eg for 2009 takes proj catch 2008-2012; increments up each yr
#                 catch_vector_use = catchvec ## matrix made at bottom of 2_makeCatches
#)
# ## write setup 
#setup(dir =  paste0(getwd(), "/Code/Tier3/", AYR, "/Projections"),
#      # sdir =  here('projection'),
#      data_file = 'setup.dat', ## name of new file
#      data = mod_2023 , ## model basis
#      nproj = 6
#)


## Execute proj module ----
# test out Ormseth's previous version for constency
setwd(paste0(getwd(), "/Code/Tier3/", AYR, "/Projections/prg_AKSK_14_2_2020")) 
shell('main')

## copy over Ormseth's work and update files
setwd(paste0(getwd(), "/Code/Tier3/", AYR, "/Projections/prg_AKSK_14_2_2023")) 
shell('main')


## compile tables - see Cole's report.R ----
### The projection model results
## ## Use R to process output into easy file to create the harvest
## ## table in report.xlsx.

rec_table1 <-
  read.table(paste0(getwd(),'/percentdb.out')) %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), year=as.numeric(V3), metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% (this_year+1:2) & scenario==1 &
           metric %in% c('SSBMean','SSBFofl', 'SSBFabc', 'SSBF100', 'Fofl', 'Fabc')) %>%
  arrange(year, metric) %>%
  pivot_wider(names_from=year, values_from=value)
rec_table1[3:6,3:4] <- rec_table1[3:6,3:4]

rec_table2 <-
  read.table(paste0(getwd(),'/alt2_proj.out'), header=TRUE) %>%
  filter(Year %in% (this_year+1:2)) %>%
  pivot_longer(cols=c(-Stock, -Year), names_to='metric', values_to='value') %>%
  pivot_wider(names_from=Year, values_from=value)
rec_table1$scenario <- rec_table2$Stock <- NULL
rec_table2[,2:3] <- rec_table2[,2:3]
rec_table <- bind_rows(rec_table1, rec_table2)

## change order to match SAFE format & magnitudes
rec_table <-rec_table[c(11,6,3,4,5,2,1,1,9,8,8),] 

# rec_table[c(1:5,9:11),2:3] <-formatC(rec_table[c(1:5,9:11),2:3] , format="d", big.mark=",") 
write_csv(rec_table, paste0(getwd(), '/rec_table.csv'))

## load last year's values and make full safe
previous_rec_table <- read_csv(paste0(getwd(),'/rec_table_old.csv'))

previous_rec_table[,c('2023','2024')] <- apply(previous_rec_table[,c('2023','2024')],2,
                                                 FUN = function(x) as.numeric(gsub(",","",x)))

## correct order error in previous table

# previous_rec_table[4:5,] <- previous_rec_table[5:4,]
rec_table[c(1:5,9:11),2:3] <- round(rec_table[c(1:5,9:11),2:3])
rec_table[c(6:8),2:3] <- round(rec_table[c(6:8),2:3],digits = 2)
safe0 <- rbind(c(rep(0.2,3)),
               c(rep('3a',3)),
               cbind(previous_rec_table[,2:3], 
                     rec_table[,2:3])) 


rownames(safe0) <-c('M', 
                    'Tier',
                    "Projected total (3+) biomass (t)",
                    "Projected Female spawning biomass (t)",
                    "B100%",
                    "B40%",
                    "B35%",
                    "FOFL",
                    "maxFABC",
                    "FABC",
                    "OFL (t)",
                    "maxABC (t)",
                    "ABC (t)"
)
# xtable(prettyNum(c(rec_table[,3]),big.mark=","))

status = matrix(NA, nrow = 5, ncol = 4)
# colnames(status) <- c(2020,2021,2021,2022)
rownames(status) <- c('blank','Status','Overfishing','Overfished','Approaching Overfished')
status[2,] <- c(this_year-1,this_year,this_year,this_year+1)
status[1,c(1,3)] <- status[2,c(2,4)] <- status[3,c(2,4)] <- 'no'
status = data.frame(status)
names(status) = names(safe0)

safe01 = rbind(safe0,status) 
# safe = noquote(apply(safe0, 2, function(x) prettyNum(x, big.mark = ",")))
# safe[safe > 10e6] <- safe[safe>10e6]/1000
## round & clean up values
safe01[8:10,] <- sapply(safe01[8:10,],  FUN = function(x) sprintf("%.2f",as.numeric(x)))
safe01[c(3:7,11:13),] <- sapply(safe01[c(3:7,11:13),],  FUN = function(x) prettyNum(round(as.numeric(x),0),big.mark=','))
safe01$item <- rownames(safe01)

safe02 <- safe01%>% select(item, y1 = "2023", y2 = "2024", y3 = "2024", y4="2025")
write.csv(safe02, paste0(getwd(), "/T3_ex_table.csv"))

safe::main_table(data=safe02, year=2023, tier=3, 
                 c1=catchvec[4,2], c2=catchvec[5,2], c3=catchvec[6,2])

save(safe02, file = here::here('tables','safe_table.rdata')) ## this can be used in RMD

# Example figure/table codes ----
# Figures ----
ggplot2::theme_set(ggsidekick::theme_sleek( base_size = 10))
theme_replace(text= element_text(family = "roboto condensed", size = 10),
              title = element_text(size = 10))

require(MetBrewer)
require(ggtext)
require(showtext)

font_add_google("roboto condensed")
showtext_opts(dpi = 520)
showtext_auto(enable = TRUE)

#* Projection plots ----
## notes from CMM
## ### Jim says to use the means from the bigfile. But I don't
## think this works it's missing a bunch of stuff.
## bigout <- read.table('projection/2019_Projections/bigfile.out', header=TRUE) %>%
##   filter(Alternative==1 & Yr %in% (this_year+1:2)) %>%
##   select(-Spp, -Alternative) %>% group_by(Yr) %>%
##   summarize_all(mean) %>% pivot_longer(c(-Yr), names_to='metric') %>% pivot_wider(names_from=Yr)
## ## It's missing B0/B40/B35 so get that from this file. I think if
## ## if I update proj this will not be necessary. Try that next year
## B0 <- strsplit(readLines('projection/2019_Projections/percentiles.out',
##   n=3)[3], ' ')[[1]][1:3] %>% as.numeric()
## rec_table <- rbind(bigout, data.frame(metric=c('SB0', 'SB40', 'SB35'),
##                                    '2020'=B0, '2021'=B0, check.names=FALSE))
pdt <- data.frame(read.table("bigfile.out", header=TRUE))
pdt.long <- pivot_longer(pdt, cols=c(-Alternative, -Spp, -Yr), names_to='metric') %>%
  mutate(Alternative=factor(Alternative)) %>% group_by(Yr, Alternative, metric) %>%
  summarize(med=median(value), lwr=quantile(value, .1), upr=quantile(value, .9), .groups='drop')
# g <- ggplot(pdt.long, aes(Yr,  med, ymin=lwr, ymax=upr, fill=Alternative, color=Alternative)) +
#   facet_wrap('metric', scales='free_y') + ylim(0,NA) +
#   geom_ribbon(alpha=0.4) + 
#   labs(x='Year', y='Estimated 80% CI')


## load previous BASE model
# remotes::install_github("r4ss/r4ss", ref = 'e588b878c06f3a60fe661e5d6e0a6d096d19d57a' )
## getting morph error otherwise
# mod_2020 <- r4ss::SS_output(here('2020_files','model_runs','Run06_francis_tuning'))
# SSplotComparisons(SSsummarize(list(base17,mod_2020) ) )

#* Fig 1 catch/totbio plot ----
## per report.xlsx/Fig1, looks like biomass is from the assessment thru 2016 then values from proj
## the figure caption indicates these are for 3+ but the model was run using age 0 as the summary biomass
## had to rerun with the summary age updated
fig1a <- mod_2023$timeseries %>% select(Yr, Bio_smry) %>%
  merge(.,mod_2023$catch %>% select(Yr, Obs), by = 'Yr') %>%
  filter(Yr != 2020) %>%
  mutate(catch_over_biomass  = Obs/Bio_smry)

fig1b <- data.frame(Yr = catchvec[,1],
                    Bio_smry = pdt %>% filter(Yr < max(catchvec[,1])+1) %>% group_by(Yr) %>%
                      summarise(Bio_smry = round(mean(Tot_biom),2)) %>% select(Bio_smry) ,
                    Obs = catchvec[,2]) %>%
  mutate(catch_over_biomass  = Obs/Bio_smry)
fig1 <- rbind(fig1a, fig1b)


## plot with diff colors for extrapolated and forecasted catches
ggplot(subset(fig1), 
       aes(x = Yr, y = catch_over_biomass)) +
  geom_line(lwd = 1, col = 'grey77') + 
  geom_point(data = subset(fig1, Yr > 2022),
             lwd = 1,  col = 'blue', pch = 1) +
  geom_point(data = subset(fig1, Yr %in% c(2020,2021,2022)),
             lwd = 1,  col = 'blue', pch = 16) +
  scale_x_continuous(labels = seq(1960,2025,5), 
                     breaks = seq(1960,2025,5))+
  scale_y_continuous(limits = c(0,0.08),
                     breaks = seq(0,0.08,0.01), 
                     labels = seq(0,0.08,0.01))+
  labs(x = 'Year', y = 'Catch/Summary Biomass (age 3+)')

ggsave(last_plot(), height = 5, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-Fig1_catchvsbio.png')))

#* Fig1.2 for ppt----
## show a line for the mean, remove the projected catches and indicate %change
diff<- round(-100*(fig1$catch_over_biomass[fig1$Yr==2019]-fig1$catch_over_biomass[fig1$Yr==2023])/
               fig1$catch_over_biomass[fig1$Yr==2019])

ggplot(subset(fig1), 
       aes(x = Yr, y = catch_over_biomass)) +
  geom_hline(aes(yintercept = mean(fig1$catch_over_biomass)
  ),linetype = 'dashed', col = 'grey88') +
  geom_text(check.overlap = T, aes(x = 2021, y = 0.027, label = paste0(diff,'%'))) +
  geom_line(lwd = 1, col = 'grey77') +  
  geom_point(data = subset(fig1, Yr %in% c(2019,2023))) +
  # ggsidekick::theme_sleek(base_size = 18) +
  scale_x_continuous(limits = c(1960,2023),
                     labels = seq(1960,2023,5), 
                     breaks = seq(1960,2023,5))+
  scale_y_continuous(limits = c(0,0.07),
                     breaks = seq(0,0.75,0.01), 
                     labels = seq(0,0.75,0.01))+
  labs(x = 'Year', y = 'Catch/Summary Biomass (age 3+)')

ggsave(last_plot(), height = 5, width = 8, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-Fig1_catchvsbio_ppt.png')))


#* index plot ----
# index <- read.csv(here('data','2021-09-15-index.csv'))
index <- read.csv(here('data',paste0(date_use,'-ss_survey_index.csv'))) %>%
  mutate(lci = obs-se_log*obs, uci = se_log*obs+obs) %>%
  mutate(lmyr = year %in% interpyr) %>%
  filter(year <2023)
# index %>% filter(  YEAR != 2019) %>% summarise(mb=mean(BIOM), sdb = sd(BIOM)) %>% mutate(mb+sdb)

## load prior index to check for changes
index21 <- read.csv( "C:/Users/maia.kapur/Work/assessments/2021/bsai-flathead/data/2021-09-22-ss_survey_index.csv") %>%
  mutate(lci = obs-se_log*obs, uci = se_log*obs+obs)  %>%
  mutate(lmyr = NA)

ggplot(index, aes(x = year, y = obs/1000)) +
  geom_line(lwd = 1, col = 'grey77') +
  # geom_point() +
  geom_point(data = subset(index, lmyr == T), pch = 4, color = 'grey66') +
  geom_point(data = subset(index, lmyr == F), color = 'grey66')+
  geom_point(data = subset(index, year == 2021), pch = 4, size = 2, color = 'blue') +
  geom_point(data = subset(index, year == 2022), color = 'blue') +
  geom_point(data = subset(index, year == 2022), color = 'blue') +
  scale_x_continuous(labels = seq(1980,2025,5),
                     breaks = seq(1980,2025,5))+
  scale_y_continuous(limits = c(0,1000) ) +
  labs(x = 'Year', y = 'Survey Biomass (1000 mt)')+
  geom_ribbon(aes(ymin =lci/1000, ymax = uci/1000 ),alpha = 0.1)

ggsave(last_plot(), height = 6, width = 10, dpi = 520,
       file = here('figs',paste0(Sys.Date(),'-index_wCVs.png')))


#* catches vs TAC ----

## NOTE: there seemed to be some input errors with TACs in years 2016 and 2019
## based on the data downloadable on AKFIN under "BSAI groundfish specifications".
## This came to my attention because the catches used in the base 2020 model were above the TACs for those years.


cbpal <- c("#999999", "#E69F00", "#56B4E9", "#009E73" ,"#F0E442", "#0072B2", "#D55E00" )
cbpal <- c("#E69F00", "#56B4E9", "#009E73" ,'black','grey66' )
cbpal <- c("#E69F00", "#56B4E9", 'black','grey66' )

catch <- read.csv(here('data','2023-09-27-catch.csv'))
mgmt0 <- read.csv(here('data','2021-11-12-BSAI_harvest_specs_1986_2021new.csv'), header = F)[,-1]
mgmt1 <- rbind(mgmt0[1:2,],as.numeric(gsub(",", "", mgmt0[3,]))) ## make harvest specs numeric (remove comma)
mgmt<- mgmt1%>%
  t() %>%  data.frame(.) %>%
  pivot_wider(.,names_from = 'X2', 
              values_from = 'X3') %>%
  mutate(Yr = X1) %>%
  select(Yr, TAC, ABC, OFL)

mod_2020$catch %>%filter(Yr == 2019)

## extrapolated catches
xtrayrs <- data.frame(Yr = sppcatch$Yr,
                      Catch = sppcatch$catch)

merge(mgmt, mod_2020$catch %>% 
        select(Yr, Catch = Obs),
      by ='Yr', all.x = TRUE)  %>%
  merge(., xtrayrs, by = 'Yr',all = T) %>%
  mutate(c1= Catch.x, c2= Catch.y) %>%
  data.frame(.) %>%
  select(-Catch.x, -Catch.y) %>%
  reshape2::melt(., id = 'Yr') %>%
  mutate(value = as.numeric(value), Yr = as.numeric(Yr)) %>%
  filter(variable != 'OFL') %>%
  filter(Yr >2016)

ggplot(., aes(x = Yr,y = value, color = variable, group = variable)) +
  geom_line(lwd = 1.1) +
  
  scale_x_continuous(limits = c(1995,2025),
                     breaks =  seq(1995,2025,5),
                     labels = seq(1995,2025,5)) +
  # scale_color_manual(values = cbpal,
  #                    labels = c('TAC','ABC','OFL',"Catches in Base Model", "Catches used in Projections" ))+
  # annotate('text', x = rep(2020.5,4), y = c(47500,40000,30000, 4500),
  #          label = c('OFL','ABC','TAC','Catches'),
  #          color = c(cbpal[3:1],'grey44'), size = 6)+
  scale_color_manual(values = cbpal,
                     labels = c('TAC','ABC',"Catches in Base Model", "Catches used in Projections" ))+
  annotate('text', x = rep(2020.5,3), 
           y = c(75000,30000, 4500),
           label = c('ABC','TAC','Catches'),
           color = c(cbpal[2:1],'grey44'), size = 6)+
  labs(x = 'Year', y = 'Catch or Harvest Spex (t)', color = '')

ggsave(last_plot(), file = here("figs","harvest_spex_vs_catch.png"),
       height = 7, width  = 12, unit = 'in', dpi = 520)



## 2020 ssb plot with current b40 ----

png(here("figs","SSB_2020_vsB40_2022.png"), width = 7, height = 5, unit = 'in', res = 520)
SSplotTimeseries(mod_2020, subplot = 7)
abline(h = 81463, lty = 'dashed')
text(x = 2015, y = 86000, label = expression('B'[40]*'=81,463 mt'))
dev.off()


## Tables ----



#* Table 1 catches by spp, using the observer data ----
## Observer data on species-specific extrapolated weight in each haul was summed
## over hauls within each year and used to calculate the proportion of the total 
## Hippoglossoides spp. catch that was flathead sole or Bering flounder.
## Proportions were multiplied by the total Hippoglossoides spp. (flathead sole
## and Bering flounder combined) catches reported by AKFIN to obtain total catch 
## of flathead sole separately from that of Bering flounder.


## MSK: Not sure where CCM got the values for < 1992. I copied in his table from
## the last BSAI assessment and only updated the last few years here.

totals <- read.csv(here('data',paste0(date_use,'-catch.csv'))) %>%
  group_by(year=YEAR) %>%
  summarize(catch=sum(TONS), .groups='drop')

## manually add the projected catch

catch_proportions <- readRDS(here('data',paste0(date_use,'-catches_observer.RDS'))) %>%
  group_by(year, species) %>%
  summarize(weight=sum(weight)/1000, .groups='drop') %>%
  pivot_wider(names_from=species, values_from=weight,values_fill=0) %>%
  mutate(prop_bf=Bering_flounder/(Bering_flounder+flathead_sole),
         prop_fs=1-prop_bf
  ) %>% 
  filter(year>=1995 | year==1992) %>%
  merge(.,totals, by='year', all = T) %>% 
  mutate(total = round(catch),
         FHS = round(prop_fs*catch),
         BF = round(prop_bf*catch)) %>%
  
  select(year, total, FHS, BF)

write.csv(catch_proportions, file = here('tables',paste0(Sys.Date(),'-catch_proportions.csv')), row.names=FALSE)


#* Table 2 ----
## EBS/AI survey data, CV by SPP.
## total (all regions, both spp), AI (both spp), EBS combo ("hippo spp"), EBS flathead, EBS flounder, w CVs
## note that for this table in 2020 cole did NOT show any imputed AI values; the "totals" are as used in assessment

index_ai <- read.csv(here('data',paste0(date_use,'-biomass_survey_ai.csv'))) %>%
  mutate(species=gsub(" ", "_",COMMON_NAME), survey = 'AI', cv = BIOMASS_VAR /TOTAL_BIOMASS) %>%
  select(year=YEAR, species, biomass=TOTAL_BIOMASS,
         variance = BIOMASS_VAR, survey) %>% mutate(cv = round(sqrt(log(1+variance/biomass^2)),5))

index_ebs_spp <- read.csv(here('data',paste0(date_use,'-biomass_survey_ebs_by_species.csv')))%>%
  mutate(species=gsub(" ", "_",COMMON_NAME), survey = 'ebs') %>%
  select(year=YEAR,species, biomass=BIOMASS,
         variance = VARBIO, survey) %>%mutate(cv = round(sqrt(log(1+variance/biomass^2)),5))

index_ebs <-  read.csv(here('data',paste0(date_use,"-biomass_survey_ebs.csv"))) %>%
  select(year=YEAR, biomass=BIOMASS,
         variance=VARBIO) %>% cbind(survey='ebs') %>% mutate(cv = round(sqrt(log(1+variance/biomass^2)),5)) %>%
  select(year, ebs_total = biomass, ebs_total_cv = cv)


ebs2 <- index_ebs_spp %>% select(year, species, biomass, cv) %>%
  pivot_wider(names_from=species, values_from=c(biomass, cv)) %>% 
  select(year, ebs_flathead = biomass_flathead_sole, ebs_flathead_cv = cv_flathead_sole, 
         ebs_bering = biomass_Bering_flounder, ebs_bering_cv=cv_Bering_flounder)

index_ai %>%
  pivot_wider(names_from=survey, values_from=c(biomass, cv)) %>%
  select(-variance, -species) %>%
  merge(., SS_index %>% select(year, obs, se_log), by = 'year',all = T) %>%
  select(year, total = obs, cv_total = se_log, biomass_AI, cv_AI) %>%
  merge(., index_ebs) %>%
  merge(., ebs2) %>%
  write.csv(., file = here('tables',paste0(Sys.Date(),'-survey_by_spp.csv')), row.names=FALSE)



#* Table 3 NBS ----
index_nbs <- read.csv(here('data',paste0(date_use,"-biomass_survey_NBS_by_species.csv"))) %>%
  mutate(species=gsub(" ", "_",COMMON_NAME), survey = 'NBS', cv = VARBIO /BIOMASS) %>%
  select(year=YEAR, species, biomass=BIOMASS,
         variance = VARBIO, survey) %>% mutate(cv = round(sqrt(log(1+variance/biomass^2)),5))


## make a total column, tot var is sum of squares
nbs2 <- index_nbs %>% 
  group_by(year) %>%
  summarise(totbio = sum(biomass),
            var2 = sum(variance),
            cv=round(sqrt(log(1+var2/totbio^2)),5)) %>%
  select(year, 
         nbs_total = totbio, 
         nbs_total_cv = cv)

index_nbs %>% select(year, species, biomass, cv) %>%
  pivot_wider(names_from=species, values_from=c(biomass, cv)) %>%
  merge(., nbs2, by = 'year') %>%
  select(year, nbs_total, nbs_total_cv, biomass_flathead_sole, cv_flathead_sole, biomass_Bering_flounder, cv_Bering_flounder) %>%
  
  write.csv(., file = here('tables',paste0(Sys.Date(),'-NBS_survey_by_spp.csv')), row.names=FALSE)

# https://rdrr.io/github/drmjc/mjcbase/man/prettyNum.html
mjcbase::prettyNum(index_nbs[,c(3,4)], big.mark = ',')

apply(nbs2,2,mjcbase::prettyNum, big.mark = ',')
