# Cold pool and temp explorations
# potentially to inform the risk tables
#library(remotes)
#install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
#devtools::install_github("afsc-gap-products/coldpool")
#NOTE got firewalled in downloading those
# manual download of .rda files from https://github.com/afsc-gap-products/coldpool/tree/main/data


# Setup ----
libs <- c("tidyverse", "janitor", 'viridis', 'patchwork', 'broom')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

AYR <- 2026

#load ----
coldpooldat <- load(here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'risk_table', 'cold_pool', 'cold_pool_index.rda'))
AKbio_dat <- read_csv(here::here(AYR, 'Nov_models', "Oskate_Tier5", 'data', paste0('Oskate_GAPbiomass_', AYR, '.csv'))) |> 
  filter(species_code == 471,
         survey == 98)

# plot cpi vs biomass----
cindex <- cold_pool_index |> 
  select(YEAR, AREA_LTE2_KM2, AREA_LTE1_KM2, AREA_LTE0_KM2) |> 
  clean_names()
AKdat <- AKbio_dat |> 
  select(year, biomass) |> 
  left_join(cindex) |> 
  pivot_longer(!c(year, biomass))

stats_labels <- AKdat %>%
  group_by(name) %>%
  do(tidy(lm(value ~ I(biomass/1000), data = .))) %>%
  filter(term == "I(biomass/1000)") %>%
  mutate(
    t_stat = (estimate - 1) / std.error,
    p_val_vs_1 = 2 * pt(abs(t_stat), df = max(AKdat$biomass) - 2, lower.tail = FALSE), # Quick df proxy or adjust to your true df
    sig_label = paste0("Slope: ", round(estimate, 2), 
                       "\nSig. diff from 1? ", ifelse(p_val_vs_1 < 0.05, "Yes", "No"),
                       " (p = ", round(p_val_vs_1, 3), ")")
  )

cpi_plot <- ggplot(AKdat, aes(x = biomass/1000, y = value, color = name))+
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, size = 1.2, alpha = 0.5, show.legend = FALSE) +
  geom_point(size = 5, show.legend = F) +
  geom_line(linewidth = 1.5, show.legend = F)+
  # Dynamically add the custom significance label to each facet panel
  #geom_text(data = stats_labels, aes(x = -Inf, y = Inf, label = sig_label), 
  #          hjust = -0.1, vjust = 1.2, inherit.aes = FALSE, size = 4, color = "black") +
  scale_color_viridis(discrete = T)+
  facet_grid(name~., scales = 'free')+
  labs(x = "Biomass (t)", y = "Cold Pool Size (km2)")+
  theme_bw()+
  # Optional refinement to make the white background perfectly clean
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.5)) 
ggsave(path = here::here(AYR, 'Nov_models', "AK_skate_Tier3", 'mgmt', 'risk_table', 'cold_pool') ,
       "AK_coldpool.png",cpi_plot,dpi=600,width = 6, height = 8)

# plot cpi over time----
cp2dat <- AKbio_dat |> 
  select(year, biomass) |> 
  left_join(cindex) |> 
  select(year, biomass, area_lte2_km2) |> 
  pivot_longer(!(year))
ts_plot <- ggplot(cp2dat, aes(x = year, y = value, color = name))+
  geom_point(size = 5) +
  geom_line(linewidth = 1.5)+
  scale_color_manual(values = c("#003f5c", "#ff6361")) +
  labs(x = "Year", y = "Value", color = '')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.5)) 
ggsave(path = here::here(AYR, 'Nov_models', "AK_skate_Tier3", 'mgmt', 'risk_table', 'cold_pool') ,
       "ts_bioindex.png", ts_plot,dpi=600,width = 6, height = 4)

# correlation stats----
cordat <- AKbio_dat |> 
  select(year, biomass) |> 
  left_join(cindex) 

cor_result <- cor.test(cordat$biomass, cordat$area_lte0_km2, method = "pearson")
cor_result$p.value    # Sig neg correlation
cor_result$estimate


# all biomass plots----
# AI survey dat
AI_dat <- read_csv(here::here(AYR, 'Nov_models', "Oskate_Tier5", 'data', paste0('Oskate_GAPbiomass_', AYR, '.csv'))) |> 
  filter(species_code == 471, 
         survey == 52,
         year >= 2010) |> 
  clean_names() |> 
  select(year, biomass) |> 
  rename(AIbiomass = biomass)

cordatAI <- AI_dat |> 
  select(year, AIbiomass) |> 
  left_join(cindex) 

cor_resultAI <- cor.test(cordatAI$AIbiomass, cordatAI$area_lte0_km2, method = "pearson")
cor_resultAI$p.value    # Sig neg correlation
cor_resultAI$estimate

cp3dat <- AKbio_dat |> 
  select(year, biomass) |> 
  left_join(cindex) |> 
  select(year, biomass, area_lte2_km2) |> 
  rename(EBSbiomass = biomass) |> 
  left_join(AI_dat) |> 
  pivot_longer(!year)

ggplot(cp3dat, aes(x = year, y = value, color = name))+
  geom_point(size = 5) +
  geom_line(linewidth = 1.5)+
  scale_color_viridis(discrete = T) +
  labs(x = "Year", y = "Value", color = '')+
  facet_grid(name~., scales = 'free')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.5)) 
