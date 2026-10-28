# Exploration of changes in AK skate length and weight
# October 2026
# BLUF not enough survey data to do anything useful 
# No linked LW data from fishery

# Setup ----
#devtools::install_github("afsc-gap-products/akfishcondition")
#devtools::install_github("afsc-gap-products/gapindex")

# load required packages and code
libs <- c('tidyverse', 'purr', 'broom')

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# Current assessment year
AYR <- 2026

############################
##############################
# read in fishery data----
dat_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'data')
LW_dat <- read_csv(here::here(dat_path, paste0('confidential_NORPAC_skates_', AYR, '.csv')))

# Calculate residuals----
# 1. Fit models for all combinations simultaneously
lw_models <- LW_dat %>%
  filter(!is.na(WEIGHT_G), !is.na(LENGTH_MM)) %>%
  # Group by your tracking variables (Sex, Species, and Area)
  group_by(SPECIES_CODE, SURVEY_CODE, SEX) %>% 
  # Nest the data for each group
  nest() %>% 
  # Fit the log-transformed linear model to each nested group
  mutate(model = map(data, ~ lm(log(WEIGHT_G) ~ log(LENGTH_MM), data = .x)))

# 2. View a clean, scannable summary table of the parameters (a and b)
# This replaces the traditional summary() output for multi-groups
model_coefficients <- lw_models %>% 
  mutate(coefs = map(model, broom::tidy)) %>% 
  unnest(coefs) %>% 
  select(SPECIES_CODE, SURVEY_CODE, SEX, term, estimate, std.error, p.value)

# Calculate residuals for each group
skate_residuals <- lw_models %>%
  # Apply broom::augment to each model-data pair
  mutate(augmented = map2(model, data, broom::augment)) %>%
  # Flatten the nested data frames
  unnest(augmented) %>%
  # Rename the default residual column for clarity
  dplyr::rename(condition_residual = .resid)

skate_summary <- skate_residuals %>%
  dplyr::select(YEAR, SPECIES_CODE, SURVEY_CODE, SEX, .std.resid) |> 
  as.data.frame()

# make a plot----
ggplot(skate_summary, aes(x = as.factor(YEAR), y = .std.resid))+
  geom_boxplot()+
  facet_grid(SURVEY_CODE~SPECIES_CODE+SEX)


############################
##############################
# read in GAP data----
dat_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'data')
LW_dat <- read_csv(here::here(dat_path, paste0('AKLEOskate_GAPLW_', AYR, '.csv')))

# Calculate residuals----
# 1. Fit models for all combinations simultaneously
lw_models <- LW_dat %>%
  filter(!is.na(WEIGHT_G), !is.na(LENGTH_MM)) %>%
  # Group by your tracking variables (Sex, Species, and Area)
  group_by(SPECIES_CODE, SURVEY_CODE, SEX) %>% 
  # Nest the data for each group
  nest() %>% 
  # Fit the log-transformed linear model to each nested group
  mutate(model = map(data, ~ lm(log(WEIGHT_G) ~ log(LENGTH_MM), data = .x)))

# 2. View a clean, scannable summary table of the parameters (a and b)
# This replaces the traditional summary() output for multi-groups
model_coefficients <- lw_models %>% 
  mutate(coefs = map(model, broom::tidy)) %>% 
  unnest(coefs) %>% 
  select(SPECIES_CODE, SURVEY_CODE, SEX, term, estimate, std.error, p.value)

# Calculate residuals for each group
skate_residuals <- lw_models %>%
  # Apply broom::augment to each model-data pair
  mutate(augmented = map2(model, data, broom::augment)) %>%
  # Flatten the nested data frames
  unnest(augmented) %>%
  # Rename the default residual column for clarity
  dplyr::rename(condition_residual = .resid)

skate_summary <- skate_residuals %>%
  dplyr::select(YEAR, SPECIES_CODE, SURVEY_CODE, SEX, .std.resid) |> 
  as.data.frame()

# make a plot----
ggplot(skate_summary, aes(x = as.factor(YEAR), y = .std.resid))+
  geom_boxplot()+
  facet_grid(SURVEY_CODE~SPECIES_CODE+SEX)
