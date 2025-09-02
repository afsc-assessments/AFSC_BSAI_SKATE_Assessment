# BSAI skate harvest projections document figs ----
# Updated 8/22/2024 by C. Tribuzio

# 18.1 Tier 3 catch to biomass ratio----
T3biodat <- read_csv(paste0(getwd(), '/', LAYR, "/Tier3/Output/M14_2dBioall_summary.csv"))
T3catch <- read_csv(paste0(getwd(), '/', AYR, "/Tier_3/M14_2dSS3catch_updated.csv")) %>%  #file created by hand from data_aksk14_2_2023.ss and added updated 2023-2025 values
  clean_names()

# see Maia's code here for next time:
# https://github.com/afsc-assessments/bsai-fhs/blob/main/2023/R/3_runProjections_makeFigsTables.R

CBratio <- T3biodat %>% 
  left_join(T3catch) %>% 
  drop_na() %>% 
  mutate(ratio = catch/Bio_all)

ggplot(CBratio, aes(x = year, y = ratio))+
  geom_line()

ggplot(subset(CBratio), 
       aes(x = year, y = ratio)) +
  geom_line(lwd = 1, col = 'grey77') + 
  geom_point(data = subset(CBratio, year > 2023),
             lwd = 1,  col = 'blue', pch = 1) +
  geom_point(data = subset(CBratio, year %in% c(2023)),
             lwd = 1,  col = 'blue', pch = 16) +
  scale_x_continuous(labels = seq(1950,2025,5), 
                     breaks = seq(1950,2025,5))+
  scale_y_continuous(limits = c(0,0.08),
                     breaks = seq(0,0.08,0.01), 
                     labels = seq(0,0.08,0.01))+
  labs(x = 'Year', y = 'Catch/Summary Biomass (age 0+)')+
  theme_bw()+
  theme(panel.grid = element_blank())
ggsave(last_plot(), height = 5, width = 8, dpi = 520,
       file = paste0(getwd(), '/', AYR, '/Tier_3/BSAIskate_Fig18_1.png'))

# 18.2 Tier 5 catch to biomass ratio----
Osk_catch <- read_csv(paste0(getwd(), "/Data/", AYR, "/confidential_CAS_SKpart", AYR, ".csv")) %>% 
  filter(NAMES != "ALASKA SKATE") %>% 
  group_by(YEAR) %>% 
  summarise(catch = sum(CATCH_WEIGHT)) %>% 
  clean_names()
Osk_rema <- read_csv(paste0(getwd(), '/', LAYR, "/Tier5/Output/Tier5_m20_output.csv")) %>% 
  group_by(year) %>% 
  summarise(Oskbiom = sum(pred))

T5CB <- Osk_catch %>% 
  left_join(Osk_rema) %>% 
  mutate(ratio = catch/Oskbiom) %>% 
  filter(year < AYR)

ggplot(subset(T5CB), 
       aes(x = year, y = ratio)) +
  geom_line(lwd = 1, col = 'grey77') + 
  scale_x_continuous(labels = seq(2000,2025,5), 
                     breaks = seq(2000,2025,5))+
  scale_y_continuous(limits = c(0,0.08),
                     breaks = seq(0,0.08,0.01), 
                     labels = seq(0,0.08,0.01))+
  labs(x = 'Year', y = 'Catch/combined rema biomass')+
  theme_bw()+
  theme(panel.grid = element_blank())
ggsave(last_plot(), height = 5, width = 8, dpi = 520,
       file = paste0(getwd(), '/', AYR, '/Tier5/BSAIskate_Fig18_2.png'))












