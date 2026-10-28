# Ratio of AK skates to leopard skates in AI
# potentially to inform the risk tables
# for discussion of shortening AI time series for these species, or some other alternative

# Setup ----
libs <- c("tidyverse", "janitor", 'RODBC', 'viridis', 'patchwork')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

AYR <- 2026

# bring in data----
# AI trawl survey biomass
AKleo_dat <- read_csv(here::here(AYR, 'Nov_models', "Oskate_Tier5", 'data', paste0('Oskate_GAPbiomass_', AYR, '.csv'))) |> 
  filter(species_code %in% c(471, 477), 
         survey == 52,
         year >= 2010) |> 
  clean_names()


# Total biomass ratio by year----
bratio <- AKleo_dat |> 
  select(year, biomass, common_name) |> 
  pivot_wider(names_from = common_name, values_from = biomass) |> 
  clean_names() |> 
  mutate(biom_ratio = alaska_skate/leopard_skate)
avg_dat <- bratio |> 
  filter(year <= 2016)

bioplot <- ggplot(bratio, aes(x = year, y = biom_ratio))+
  geom_point(size = 5, color = '#2f4f4f') +
  geom_line(linewidth = 1.5, color = '#2f4f4f') +
  geom_point(data = avg_dat, aes(x = year, y = biom_ratio, size = 5), color = "#ffa600", show.legend = F)+
  geom_hline(aes(yintercept = 0.245), linetype = 'dashed', color = "#ffa600", linewidth = 1.5)+
  labs(x = "Year", y = "Alaska skate:leopard skate")+
  theme_bw()+
  # Optional refinement to make the white background perfectly clean
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.5)) 

# Proportion of hauls----
hprop <- AKleo_dat |> 
  select(year, n_haul, n_weight, common_name) |> 
  mutate(haul_prop = n_weight/n_haul)
propplot <- ggplot(hprop, aes(x = year, y = haul_prop, color = common_name))+
  geom_point(size = 5) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("#003f5c", "#ff6361")) +
  labs(x = "Year", y = "Proportion of Hauls", color = 'Species')+
  theme_bw() +
  # Optional refinement to make the white background perfectly clean
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.5)) 

# Total n observed----
dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

channel_akfin <- odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

GAP_catch <- sqlQuery(channel_akfin, query = ("
                select * from gap_products.akfin_catch_v
                where species_code in (471, 477)
                and survey_definition_id in (52)")) %>% 
  clean_names() |> 
  filter(year >= 2010)

write_csv(GAP_catch, here::here(AYR, 'Nov_models', "Oskate_Tier5", 'data', paste0('Oskate_GAPcatch_', AYR, '.csv'))) 

ncatch <- GAP_catch |> 
  group_by(year, common_name) |> 
  summarise(tot_n = sum(count, na.rm = T))

catchplot <- ggplot(ncatch, aes(x = year, y = tot_n, color = common_name)) +
  geom_point(size = 5) +
  geom_line(linewidth = 1.5) +
  # Manually define your two high-contrast colors here:
  scale_color_manual(values = c("#003f5c", "#ff6361")) +
  labs(x = "Year", y = "Total Animals Observed", color = "Species") +
  theme_bw() +
  # Optional refinement to make the white background perfectly clean
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black', linewidth = 0.5)) 

# Plot----
plot_ratios <- (bioplot / propplot / catchplot) +
  plot_layout(guides = "collect")
ggsave(path = here::here(AYR, 'Nov_models', "AK_skate_Tier3", 'mgmt', 'risk_table', 'AKLeo_ratio') ,
       "AK_Leo_ratios.png",plot=plot_ratios,dpi=600,width = 6, height = 8)
