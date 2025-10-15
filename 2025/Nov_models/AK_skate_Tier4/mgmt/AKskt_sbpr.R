# Tier 4 calculations developed by K Omori and J Sullivan
# adapted for AK skates Sept 2025 by C Tribuzio

# Setup----
libs <- c("dplyr", "tidyr", "ggplot2")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 10) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

AYR <- 2025

#get spbr functions
source(here::here(AYR, 'Nov_models', 'AK_skate_Tier4', 'R', 'T4_sbpr_fun.R'))

# Calculate values for AK skates----
# TODO need to reparameterize function for Gompertz growth, big diff in Linf
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

#Selectivity set up 
# see AKskt_LLselectivity.R for origination and Kand W 2005
# define the linear ramp up, starting at 0.5 at age = 0, ending at 1 at age = 10
srupval <- 0.25
srupage <- 0
erupage <- 7
erupval <- 1
startramp <- seq(srupval, erupval, (erupval - srupval)/(erupage-srupage))

srdnval <- 1
srdnage <- 15
erdnage <- 26
erdnval <- 0.75
endramp <- seq(srdnval, erdnval, (erdnval - srdnval)/(erdnage-srdnage))

topwidth <- (srdnage - erupage)-1
selexF <- c(startramp, rep(1, topwidth), endramp)
length(selexF)
ageselex <- selexF %>% 
  bind_cols(0:26)
colnames(ageselex) <- c('Proportion', 'Age')
ggplot(ageselex, aes(x = Age, y = Proportion))+
  geom_point()+
  geom_line()+
  theme_bw()

# theme_bw()# maturity curve set up
# ignoring the built in function and making curve based on Matta MS Thesis
ages <- 0:26
mata_fem <- -15.280
matb_fem <- 1.574
maa <- 1/(1+exp(-(mata_fem+matb_fem*ages)))

# run the functions to calc F rates
res <- run_spr_fun(F_vec = seq(0, 1, by = 0.001),
                   R0 = 1,
                   ages = 0:26,
                   sex_ratio = 0.5,
                   M = data_inputs$natM,
                   # Length-at-age
                   Linf = data_inputs$vb_Linf, # cm
                   k = data_inputs$vb_k,
                   t0 = data_inputs$vb_t0,
                   # Weight-at-length
                   alpha =  data_inputs$lw_a,
                   beta = data_inputs$lw_b,
                   # Maturity
                   #a50 = data_inputs$mat_a50,
                   #delta = data_inputs$mat_sigma,
                   maa = maa,
                   # Fishery selectivity
                   #Fdelta = 0.69,
                   #Fa50 = 13.50,
                   selex = selexF)
res$F40
res$F35

plots <- plot_spr_ypr(res)
plots$ypr_spr
plots$spr_F
plots$ypr_F
plots$spr_ypr_F

# Tier 4 harvest recommendations----
AKsk_rema <- read_csv(here::here(AYR, 'Nov_models', 'AK_skate_Tier5', 'mgmt', 'Tier5_AKskt_alloutput.csv')) %>% 
  filter(year == AYR) %>% 
  rename(rbiom = pred) %>% 
  select(year, rbiom, strata) %>% 
  mutate(rbiom = round(rbiom, 0))

T4regionrecs <- AKsk_rema %>% 
  mutate(OFL = round(rbiom * res$F35, 0),
         ABC = round(rbiom * res$F40, 0),
         FOFL = res$F35,
         FABC = res$F40,
         model_name = c("Tier 4 EBS", "Tier 4 NBS")) %>% 
  select(!strata)

T4combrecs <- read_csv(here::here(AYR, 'Nov_models', 'AK_skate_Tier5', 'mgmt', 'Tier5_AKskt_alloutput.csv')) %>% 
  filter(year == AYR) %>% 
  group_by(year) %>% 
  summarise(rbiom = round(sum(pred), 0),
            OFL = round(rbiom * res$F35, 0),
            ABC = round(rbiom * res$F40, 0)) %>% 
  filter(year == AYR) %>% 
  mutate(FOFL = res$F35,
         FABC = res$F40,
         model_name = "Tier 4 combined")

# all T5 options----
T4recs_combined <- T4regionrecs %>% 
  bind_rows(T4combrecs) 
write_csv(T4recs_combined, here::here(AYR, 'Nov_models', 'AK_skate_Tier4', 'mgmt', 'Tier4_AKskt_options.csv'))
