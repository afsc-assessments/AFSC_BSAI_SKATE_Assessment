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
KW_dat <- read_csv(here::here(AYR, 'Nov_models', 'AK_skate_Tier4', 'data', 'Kotwicki_underbag_alllength.csv')) %>% 
  clean_names() %>% 
  filter(species_code == 471,
         length >= 150) %>% 
  select(c(sex, length, frequency, length_type)) %>% 
  rename(tl_mm = length)

# get proportions by sex and size----
tot_lengths <- KW_dat %>% 
  filter(sex != 3) %>% 
  group_by(sex, tl_mm) %>% 
  summarise(ntotal = sum(frequency), .groups = 'drop')
l_sex <- KW_dat %>% 
  filter(sex != 3) %>% 
  group_by(sex, tl_mm, length_type) %>% 
  summarise(nlengths = sum(frequency), .groups = 'drop') %>% 
  left_join(tot_lengths, by = c('sex', 'tl_mm')) %>% 
  mutate(lprop = nlengths/ntotal)

ggplot(l_sex, aes(x = tl_mm, y = lprop, colour = as.factor(sex)))+
  geom_point()+
  geom_line()+
  facet_grid(length_type~.)

#paired t-test for sexes----
pt_rdat <- l_sex %>% 
  filter(length_type == 'r') %>% 
  select(sex, tl_mm, nlengths) %>% 
  mutate(sex = if_else(sex == 1, 'Male', 'Female')) %>% 
  pivot_wider(names_from = sex, values_from = nlengths)

t.test(pt_rdat$Male, pt_rdat$Female,
       alternative = "two.sided",
       mu = 0, 
       paired = TRUE,   
       var.equal = TRUE,
       conf.level = 0.95)
# no sig diff in retained proportions between sexes
#Paired t-test
#data:  pt_rdat$Male and pt_rdat$Female
#t = -2.5733, df = 47, p-value = 0.01329
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  -4.7513939 -0.5819394
#sample estimates:
#  mean difference 
#-2.666667 

pt_alldat <- tot_lengths %>% 
  select(sex, tl_mm, ntotal) %>% 
  mutate(sex = if_else(sex == 1, 'Male', 'Female')) %>% 
  pivot_wider(names_from = sex, values_from = ntotal)

t.test(pt_alldat$Male, pt_alldat$Female,
       alternative = "two.sided",
       mu = 0, 
       paired = TRUE,   
       var.equal = TRUE,
       conf.level = 0.95)
# no significant difference in the sizes caught by sex
#Paired t-test
#data:  pt_alldat$Male and pt_alldat$Female
#t = -3.0241, df = 58, p-value = 0.003711
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  -5.126591 -1.042900
#sample estimates:
#  mean difference 
#-3.084746 

# convert to ages----
KW_dat <- KW_dat %>% 
  mutate(tl_cm = tl_mm/10,
         sex = if_else(sex == 1, 'Male', 
                       if_else(sex == 2, 'Female', "Unknown"))) #put length in same units as parameters
# convert to ages using sex specific age parameters
# Gompertz (aka Schnute) age parameters
# parameters from Matta and Gunderson 2007
L0 <- c(21.90, 22.54, 22.50)
k <- c(0.23, 0.19, 0.21)
g <- c(1.63, 1.68, 1.64)
Linf <- c(111.26, 120.51, 115.99)

params <- tibble::tibble(
  sex = c("Male", "Female", "Unknown"),
  L0    = L0,    # vector length 3
  Linf  = Linf,
  g     = g,
  k     = k
)

#A parameters
A1 <- 0 # age 0's are caught in the data
A2 <- 15 # mean age at which Linf is achieved

KW_dat2 <- KW_dat %>%
  left_join(params, by = "sex")

lasex_dat <- KW_dat2 %>%
  mutate(
    expr = 1 - ((tl_cm^g - L0^g)/(Linf^g - L0^g)) * (1 - exp(-k*(A2 - A1))),
    sexage = ifelse(expr > 0,
                    round(A1 - (1/k) * log(expr), 0),
                    NA_real_)
  )

# there was one very large individual whose length produces impossible age estimates, so creating a 10+ age bin
lasex_dat <- lasex_dat %>% 
  mutate(sexage = if_else(sexage < 10, sexage, 10),
         sexage = if_else(is.na(sexage), 10, sexage))

#convert to ages using same parameters as Tier 3 model, sexes combined
L0 <- 23.95
Linf <- 104.77
g <- -1.90
k <- 0.36

#A parameters
A1 <- 0 # age 0's are caught in the data
A2 <- 15 # mean age at which Linf is achieved

lage_dat <- KW_dat %>%
  mutate(
    expr = 1 - ((tl_cm^g - L0^g)/(Linf^g - L0^g)) * (1 - exp(-k*(A2 - A1))),
    sexage = ifelse(expr > 0,
                    round(A1 - (1/k) * log(expr), 0),
                    NA_real_)
  )

# change the -1 ages to be zero, some small females in the data
lage_dat <- lage_dat %>% 
  mutate(sexage = if_else(sexage < 0, 0, sexage),
         sexage = if_else(is.na(sexage), 20, sexage))


#proportions by ages ----
tot_ages <- lage_dat %>% 
  group_by(sexage) %>% 
  summarise(ntotal = sum(frequency), .groups = 'drop')
age_dat <- lage_dat %>% 
  group_by(sexage, length_type) %>% 
  summarise(nages = sum(frequency), .groups = 'drop') %>% 
  left_join(tot_ages, by = c('sexage')) %>% 
  mutate(lprop = nages/ntotal)

ggplot(age_dat, aes(x = sexage, y = lprop))+
  geom_point()+
  geom_line()+
  facet_grid(length_type~.)

# base selectivity curve----
# define the linear ramp up, starting at 0.5 at age = 0, ending at 1 at age = 10
srupval <- 0.5
srupage <- 0
erupage <- 10
erupval <- 1
startramp <- seq(srupval, erupval, (erupval - srupval)/(erupage-srupage))

srdnval <- 1
srdnage <- 15
erdnage <- 26
erdnval <- 0.5
endramp <- seq(srdnval, erdnval, (erdnval - srdnval)/(erdnage-srdnage))

topwidth <- (srdnage - erupage)-1
selexF <- c(startramp, rep(1, topwidth), endramp)
length(selexF)
plot(selexF) 
