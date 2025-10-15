# Tier 4 sbpr derivation of selectivity curve
# Cindy Tribuzio
# Sept 24, 2025
# based on Kotwicki and Weinberg 2005 underbag study
# see email from Stan Kotwicki 9 Sept 2025
# https://www.adfg.alaska.gov/static/home/library/PDFs/afrb/kotwv11n2.pdf

# Setup----
libs <- c("tidyverse", "janitor")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

AYR <- 2025

# get data----
L_dat <- read_csv(here::here(AYR, 'Nov_models', 'AK_skate_Tier4', 'data', 'fisherylcompall_AKskt2023.csv')) %>% 
  clean_names() %>% 
  select(c(sex, length, frequency, fmp_gear)) %>% 
  filter(fmp_gear != 'POT')

# get proportions by size----
gear_tots <- L_dat %>% 
  group_by(fmp_gear) %>% 
  summarise(gtot = sum(frequency))

tot_lengths <- L_dat %>% 
  group_by(length, fmp_gear) %>% 
  summarise(ntotal = sum(frequency),
            lprop = ntotal/alllength)

ggplot(tot_lengths, aes(x = length, y = lprop, color = fmp_gear))+
  geom_point()+
  geom_line()

# convert data to ages----
#convert to ages using same parameters as Tier 3 model, sexes combined
L0 <- 23.95
Linf <- 104.77
g <- -1.90
k <- 0.36

#A parameters
A1 <- 0 # age 0's are caught in the data
A2 <- 15 # mean age at which Linf is achieved

lage_dat <- L_dat %>%
  mutate(
    expr = 1 - ((length^g - L0^g)/(Linf^g - L0^g)) * (1 - exp(-k*(A2 - A1))),
    age = ifelse(expr > 0,
                    round(A1 - (1/k) * log(expr), 0),
                    NA_real_)
  )

# change the -1 ages to be zero, some small females in the data
lage_dat <- lage_dat %>% 
  mutate(age = if_else(age < 0, 0, age),
         age = if_else(is.na(age), 20, age))

# mean length at age----
mlage <- lage_dat %>% 
  group_by(age) %>% 
  summarise(mlength = mean(length))


#proportions by ages ----
tot_ages <- lage_dat %>% 
  group_by(fmp_gear) %>% 
  summarise(totage = sum(frequency))
age_dat <- lage_dat %>% 
  group_by(age, fmp_gear) %>% 
  summarise(nages = sum(frequency),
            .groups = 'drop') %>% 
  left_join(tot_ages, by = 'fmp_gear') %>% 
  mutate(aprop = nages/totage)

ll_scaled_age <- age_dat %>%
  filter(fmp_gear == 'HAL') %>% 
  mutate(scprop = (aprop - min(aprop))/(max(aprop)-min(aprop)))

t_scaled_age <- age_dat %>%
  filter(fmp_gear == 'TRW') %>% 
  mutate(scprop = (aprop - min(aprop))/(max(aprop)-min(aprop)))

scaled_age <- ll_scaled_age %>% 
  bind_rows(t_scaled_age)

ggplot(scaled_age, aes(x = age, y = scprop, color = fmp_gear))+
  geom_point()+
  geom_line()

# base selectivity curve----
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
plot(selexF) 
