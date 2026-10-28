# Model 14_2d1 to the proposed model for 2026
# developed by C Tribuzio October 2026

#TODO
# update LOO
# run M sensitivies and growth option, not for doc, but for presentation

# setup ----
libs <- c("r4ss", "here", "tidyverse")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Default with no version downloads the latest release
# r4ss::get_ss3_exe()

AYR <- 2026

exe_loc <- here::here(paste0(AYR, '/Nov_models/AK_skate_Tier3/ss.exe'))

##########
# Model 14_2d1----
# base model
M14_2d1_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M14_2d1')

r4ss::run(dir = M14_2d1_path, skipfinished = FALSE, exe = exe_loc)

M14_2d1_out <- SS_output(M14_2d1_path, printstats = FALSE, verbose = FALSE)
M14_2d1ramp <- SS_fitbiasramp(replist = M14_2d1_out, plot = T)

# plots the results
SS_plots(M14_2d1_out)

# summary tables of results
replist142d1 <- SS_output(dir = M14_2d1_path,
                          verbose = FALSE) 
SSexecutivesummary(replist142d1)

##########
# Model 25_3----
# From Sept 2025, but this version fixed the mis-specified CV young/old parameters, so not a new model
# Q = 1
M25_3_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M25_3')

r4ss::run(dir = M25_3_path, skipfinished = FALSE, exe = exe_loc)

M25_3_out <- SS_output(M25_3_path, printstats = FALSE, verbose = FALSE)
M25_3ramp <- SS_fitbiasramp(replist = M25_3_out, plot = T)

# plots the results
SS_plots(M25_3_out)

# summary tables of results
replist253 <- SS_output(dir = M25_3_path,
                          verbose = FALSE) 
SSexecutivesummary(replist253)

# Compare models ----
# bridge
datapath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt')
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("M14_2d1",
                                     "M25_3"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/BSAI_skates")
model_comp <- SSsummarize(bridge_out)
plotpath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'model_comparison')
if (!dir.exists(plotpath)) dir.create(plotpath, recursive = TRUE)
SSplotComparisons(model_comp,
                  print = TRUE,
                  plotdir = here::here(plotpath),
                  legendlabels = c("M14_2d1",
                                   "M25_3"
                                   ))

# length comp comparison in one plot
# 1. Map the model names to the bridge_out elements and bind them together
model_names <- c("M14_2d1", "M25_3")

length_comps <- map2_dfr(bridge_out, model_names, function(model, name) {
  # Extract the length composition database if it exists
  if (!is.null(model$lendbase)) {
    return(model$lendbase %>% mutate(Model_Label = name))
  } else {
    return(NULL)
  }
})

# Fleet 1 - LGL
flt1plot_data <- length_comps %>% 
  filter(
    Fleet == unique(length_comps$Fleet)[1] # Selects your first fleet
  )

flt1fig <- ggplot(flt1plot_data) +
  # Draw the observed data (same for both models, pick one so it doesn't duplicate)
  geom_bar(data = filter(flt1plot_data, Model_Label == "M14_2d1"),
           aes(x = Bin, y = Obs), 
           stat = "identity", fill = "grey75", color = "grey60", alpha = 0.7) +
  # Overlay line fits for both models
  geom_line(aes(x = Bin, y = Exp, color = Model_Label), linewidth = 0.7) +
  # Facet grid by Year to see the yearly progression
  facet_wrap(~Yr) +
  # Style and Color setup
  scale_color_manual(values = c("M14_2d1" = "royalblue", "M25_3" = "red")) +
  labs(
    x = "Length Bin (cm)", 
    y = "Proportion", 
    color = "Model Fit",
    title = 'Longline Fleet'
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = -20, r = 0, b = 0, l = 0), 
    plot.title = element_text(margin = margin(t = 10, b = 5)),
    legend.background = element_blank(),
    panel.spacing = unit(0, "lines"), 
    strip.text = element_text(size = 8, margin = margin(t = 2, r = 0, b = 2, l = 0)),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9)
  )
flt1fig
ggsave(path = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'model_comparison'),
       "Flt1_lengcomp_compare.png", plot = flt1fig, dpi=600, width = 8, height = 6)

# Fleet 2 - TWL
flt2plot_data <- length_comps %>% 
  filter(
    Fleet == unique(length_comps$Fleet)[2] # Selects your first fleet
  )

flt2fig <- ggplot(flt2plot_data) +
  # Draw the observed data (same for both models, pick one so it doesn't duplicate)
  geom_bar(data = filter(flt2plot_data, Model_Label == "M14_2d1"),
           aes(x = Bin, y = Obs), 
           stat = "identity", fill = "grey75", color = "grey60", alpha = 0.7) +
  # Overlay line fits for both models
  geom_line(aes(x = Bin, y = Exp, color = Model_Label), linewidth = 0.7) +
  # Facet grid by Year to see the yearly progression
  facet_wrap(~Yr) +
  # Style and Color setup
  scale_color_manual(values = c("M14_2d1" = "royalblue", "M25_3" = "red")) +
  labs(
    x = "Length Bin (cm)", 
    y = "Proportion", 
    color = "Model Fit",
    title = 'Trawl Fleet'
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = -20, r = 0, b = 0, l = 0), 
    plot.title = element_text(margin = margin(t = 10, b = 5)),
    legend.background = element_blank(),
    panel.spacing = unit(0, "lines"), 
    strip.text = element_text(size = 8, margin = margin(t = 2, r = 0, b = 2, l = 0)),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9)
  )
flt2fig
ggsave(path = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'model_comparison'),
       "Flt2_lengcomp_compare.png", plot = flt2fig, dpi=600, width = 6, height = 6)

# Fleet 3 - Survey
flt3plot_data <- length_comps %>% 
  filter(
    Fleet == unique(length_comps$Fleet)[3] # Selects your first fleet
  )
flt3fig <- ggplot(flt3plot_data) +
  # Draw the observed data (same for both models, pick one so it doesn't duplicate)
  geom_bar(data = filter(flt3plot_data, Model_Label == "M14_2d1"),
           aes(x = Bin, y = Obs), 
           stat = "identity", fill = "grey75", color = "grey60", alpha = 0.7) +
  # Overlay line fits for both models
  geom_line(aes(x = Bin, y = Exp, color = Model_Label), linewidth = 0.7) +
  # Facet grid by Year to see the yearly progression
  facet_wrap(~Yr) +
  # Style and Color setup
  scale_color_manual(values = c("M14_2d1" = "royalblue", "M25_3" = "red")) +
  labs(
    x = "Length Bin (cm)", 
    y = "Proportion", 
    color = "Model Fit",
    title = 'Survey Fleet'
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = -20, r = 0, b = 0, l = 0), 
    plot.title = element_text(margin = margin(t = 10, b = 5)),
    legend.background = element_blank(),
    panel.spacing = unit(0, "lines"), 
    strip.text = element_text(size = 8, margin = margin(t = 2, r = 0, b = 2, l = 0)),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9)
  )
flt3fig
ggsave(path = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'model_comparison'),
       "Flt3_lengcomp_compare.png", plot = flt3fig, dpi=600, width = 6, height = 6)

# selectivity plots
model_names <- c("M14_2d1", "M25_3")

# Extract the length selectivity curves
sel_curves <- map2_dfr(bridge_out, model_names, function(model, name) {
  # Extract length-based selectivity table
  if (!is.null(model$sizeselex)) {
    return(model$sizeselex %>% 
             filter(Factor == "Lsel") %>% # "Asel" isolates the final derived selectivity
             mutate(Model_Label = name))
  } else {
    return(NULL)
  }
})

# Pivot data into a clean structure for ggplot
# SS3 stores bin sizes as numeric column headers (e.g., '15', '20', '25'...)
# We reshape these into a single "Bin" column
plot_sel <- sel_curves %>%
  tidyr::pivot_longer(
    cols = matches("^[0-9]"), 
    names_to = "Bin", 
    values_to = "Selectivity"
  ) %>%
  mutate(Bin = as.numeric(Bin)) %>%
  # Filter out retained columns if you only want total selectivity
  filter(Fleet == unique(Fleet)) # Or pick a specific fleet by name string

# Generate the plot
sel_fig <- ggplot(plot_sel) +
  geom_line(aes(x = Bin, y = Selectivity, color = Model_Label), linewidth = 1) +
  facet_wrap(.~Fleet) +
  scale_color_manual(values = c("M14_2d1" = "royalblue", "M25_3" = "red")) +
  labs(
    x = "Length Bin (cm)",
    y = "Selectivity",
    color = "Model",
    title = "Length Selectivity Comparison"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  filename = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'model_comparison', 'Flt_selectivity_compare.png'),
  plot = sel_fig, 
  dpi = 600, width = 6, height = 4
)

# FLimit model run----
# as per guidelines: "For models developed in Stock Synthesis, 
# replace last year’s catch in the data file with last year’s OFL, 
# set maximum phase to 0 in the starter file, and re-run SS from the 
# *.par file (the answer will be listed in the report file as “F_20yy,” 
# where 20yy is the previous year).  
# AK skates has two fleets, so we use the proportion of catch over the 
# last 5 years to proportion the OFL, not sure if it matters

lastyrOFL <- read_csv(here::here(AYR, 'safe', 'tables', 'Tables_OFLABCTAC.csv')) |> 
  clean_names() |> 
  filter(year == AYR - 1) |> 
  pull(ofl)

m5yrcatchprop <- read_csv(here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'data', 'gear5yrmean_2026.csv'))

gearofl <- m5yrcatchprop |> 
  mutate(gofl = round(mprop * lastyrOFL, 0))

# delete catch in AYR, change catch in AYR-1 to the gofl
# in .par file
# 1 # 0=use init values in control file; 1=use ss.par
# 0 # Turn off estimation for parameters entering after this phase

##########
# Model 25_3----
# From Sept 2025, but this version fixed the mis-specified CV young/old parameters, so not a new model
# Q = 1
M25_3flim_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M25_3_Flimit')

r4ss::run(dir = M25_3flim_path, skipfinished = FALSE, exe = exe_loc)

M25_3flim_out <- SS_output(M25_3flim_path, printstats = FALSE, verbose = FALSE, covar = F)

f_values <- M25_3flim_out$derived_quants[grep("F_", rownames(M25_3flim_out$derived_quants)), ]

Flimit <- f_values |> 
  filter(Label == paste0('F_', AYR-1))

write_csv(Flimit, here::here(M25_3flim_path, paste0('FLimit_', AYR, '.csv')))

# github summary tables----
 # N at age tables
naa142d1 <- replist142d1$natage |>
  filter(Era == 'TIME',
         `Beg/Mid` == 'B') |> 
  select(!c(Area, Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph, Seas, Time, 'Beg/Mid', Era))
write_csv(naa142d1, here::here(M14_2d1_path, 'tables', paste0('M14_2d1_NAA_', AYR, '.csv')))

naa253 <- replist253$natage |>
  filter(Era == 'TIME',
         `Beg/Mid` == 'B') |> 
  select(!c(Area, Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph, Seas, Time, 'Beg/Mid', Era))
write_csv(naa253, here::here(M25_3_path, 'tables', paste0('M25_3_NAA_', AYR, '.csv')))

# biomass summary
ssb142d1 <- replist142d1$derived_quants %>%
  filter(stringr::str_detect(Label, "^SSB_")) %>% 
  mutate(
    Year = as.numeric(stringr::str_extract(Label, "\\d+")),
    SSB = Value,
    SSB_CV = StdDev / Value
  ) %>%
  select(Year, SSB, SSB_CV)

totbio142d1 <- replist142d1$timeseries %>%
  filter(Era == "TIME") %>% # Filters for the main historical model period
  select(Yr, Bio_all)      # Yr matches Year; Bio_all is total biomass

biosumm142d1 <- ssb142d1 %>%
  inner_join(totbio142d1, by = c("Year" = "Yr")) %>%
  select(Year, SSB, SSB_CV, Bio_all)
write_csv(biosumm142d1, here::here(M14_2d1_path, 'tables', paste0('M14_2d1_biomass_summary_', AYR, '.csv')))

ssb253 <- replist253$derived_quants %>%
  filter(stringr::str_detect(Label, "^SSB_")) %>% 
  mutate(
    Year = as.numeric(stringr::str_extract(Label, "\\d+")),
    SSB = Value,
    SSB_CV = StdDev / Value
  ) %>%
  select(Year, SSB, SSB_CV)

totbio253 <- replist253$timeseries %>%
  filter(Era == "TIME") %>% # Filters for the main historical model period
  select(Yr, Bio_all)      # Yr matches Year; Bio_all is total biomass

biosumm253 <- ssb253 %>%
  inner_join(totbio253, by = c("Year" = "Yr")) %>%
  select(Year, SSB, SSB_CV, Bio_all)
write_csv(biosumm253, here::here(M25_3_path, 'tables', paste0('M25_3_biomass_summary_', AYR, '.csv')))

# F rate summary
frate_fleet142d1 <- replist142d1$timeseries |> 
  filter(Era == 'TIME') |> 
  select(year = Yr, LGL_F = 'F:_1', TWL_F = 'F:_2')

total_f142d1 <- replist142d1$derived_quants %>%
  filter(str_detect(Label, "^F_")) %>%
  mutate(
    year = as.numeric(str_extract(Label, "\\d+")),
    Total_F = Value
  ) %>%
  select(year, Total_F)

Fsumm_142d1 <- frate_fleet142d1 |> 
  inner_join(total_f142d1)
write_csv(Fsumm_142d1, here::here(M14_2d1_path, 'tables', paste0('M14_2d1_F_summary_', AYR, '.csv')))

frate_fleet253 <- replist253$timeseries |> 
  filter(Era == 'TIME') |> 
  select(year = Yr, LGL_F = 'F:_1', TWL_F = 'F:_2')

total_f253 <- replist253$derived_quants %>%
  filter(str_detect(Label, "^F_")) %>%
  mutate(
    year = as.numeric(str_extract(Label, "\\d+")),
    Total_F = Value
  ) %>%
  select(year, Total_F)

Fsumm_253 <- frate_fleet253 |> 
  inner_join(total_f253)
write_csv(Fsumm_253, here::here(M25_3_path, 'tables', paste0('M25_3_F_summary_', AYR, '.csv')))

# recruitment summary table
rec_142d1 <- replist142d1$derived_quants %>%
  # Matches "Recr_2020" but ignores management/benchmark labels like "Recr_Virgin"
  filter(str_detect(Label, "^Recr_\\d+")) %>%
  mutate(
    # Parse the year from the label text
    Year = as.numeric(str_extract(Label, "\\d+")),
    Recruits = Value,
    St_Dev = StdDev,
    # CV = Standard Deviation / Estimate
    CV = StdDev / Value,
    # 95% Confidence Intervals (Normal approximation)
    Lower_95 = Value - (1.96 * StdDev),
    Upper_95 = Value + (1.96 * StdDev)
  ) %>%
  select(Year, Recruits, St_Dev, CV, Lower_95, Upper_95) %>%
  arrange(Year)
write_csv(rec_142d1, here::here(M14_2d1_path, 'tables', paste0('M14_2d1_recruit_summary_', AYR, '.csv')))

rec_253 <- replist253$derived_quants %>%
  # Matches "Recr_2020" but ignores management/benchmark labels like "Recr_Virgin"
  filter(str_detect(Label, "^Recr_\\d+")) %>%
  mutate(
    # Parse the year from the label text
    Year = as.numeric(str_extract(Label, "\\d+")),
    Recruits = Value,
    St_Dev = StdDev,
    # CV = Standard Deviation / Estimate
    CV = StdDev / Value,
    # 95% Confidence Intervals (Normal approximation)
    Lower_95 = Value - (1.96 * StdDev),
    Upper_95 = Value + (1.96 * StdDev)
  ) %>%
  select(Year, Recruits, St_Dev, CV, Lower_95, Upper_95) %>%
  arrange(Year)
write_csv(rec_253, here::here(M25_3_path, 'tables', paste0('M25_3_recruit_summary_', AYR, '.csv')))

#length comp summary tables
# N at length tables
nal142d1 <- replist142d1$natlen |>
  filter(Era == 'TIME',
         `Beg/Mid` == 'B') |> 
  select(!c(Area, Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph, Seas, Time, 'Beg/Mid', Era))
write_csv(nal142d1, here::here(M14_2d1_path, 'tables', paste0('M14_2d1_NAL_', AYR, '.csv')))

nal253 <- replist253$natlen |>
  filter(Era == 'TIME',
         `Beg/Mid` == 'B') |> 
  select(!c(Area, Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph, Seas, Time, 'Beg/Mid', Era))
write_csv(nal253, here::here(M25_3_path, 'tables', paste0('M25_3_NAL_', AYR, '.csv')))
