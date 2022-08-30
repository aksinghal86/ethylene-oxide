library(shiny)
library(tidyverse)
library(ggiraph) 
library(geomtextpath)
library(sf)
# library(terra)
library(mapdeck)
library(reactable)

# Data files
emissions <- read_rds('data/combined-eto.rds') 
cancer <- st_read('data/neighbor-tracts.shp')

# latlong <- data.frame(geom(centroids(cancer)))
cancer <- cancer %>% 
  filter(pt_cancer_ > 0) %>% 
  mutate(log_pt_cancer = log(pt_cancer_)) 
  

emissions_for_map <- emissions %>% 
  filter(total_emissions_epa > 1)

# cancer <- data.frame(cancer) %>% 
#   mutate(lat = latlong$y, lon = latlong$x, 
#          log_pt_cancer = log(pt_cancer_)) %>% 
#   filter(pt_cancer_ > 0) 


emissions_for_plot <- emissions %>% 
  mutate(emissions = pmax(total_emissions_nei, total_emissions_tri, total_emissions_epa, na.rm=T)) %>% 
  group_by(eisid) %>% 
  mutate(n = n()) %>% 
  arrange(year) %>% 
  filter(!is.na(emissions), !is.na(eisid), n >= 2)
top50_sites <- emissions_for_plot %>%
  summarize(max_emissions = max(emissions)) %>% 
  ungroup() %>% 
  arrange(-max_emissions) %>% 
  slice(1:50) %>% 
  pull(eisid)
emissions_for_plot <- emissions_for_plot %>% 
  filter(eisid %in% top50_sites, 
         !is.na(site_name)) %>% 
  mutate(site_name = str_squish(str_replace_all(toupper(site_name), '[:punct:]|PLANT|LLC|CORPORATION|CORP', '')),
         site_name = paste0(site_name, ', ', state))


emissions_for_table <- emissions %>% 
  filter(eisid %in% top50_sites, 
         !is.na(site_name),
         eisid != "16431311") %>% 
  group_by(eisid) %>% 
  arrange(year) %>% 
  mutate(reference_emissions = total_emissions_epa) %>%
  fill(reference_emissions, .direction = 'downup') %>%
  mutate(
    reported_emissions = case_when(
      year %in% c(2014, 2017) ~ total_emissions_epa,
      TRUE ~ pmax(total_emissions_epa, total_emissions_nei, total_emissions_tri, na.rm=T)
    ),
    emissions_source = case_when(year %in% c(2014, 2017) ~ "NEI/EPA", TRUE ~  "TRI"), 
    weight = reported_emissions/reference_emissions,
    cancer_source = case_when(year %in% c(2014, 2017) ~  "NATA/Air Tox", TRUE ~ "Estimated"), 
    temp = as.numeric(pt_cancer_risk)
  ) %>% 
  ungroup() %>% 
  arrange(eisid, year) %>% 
  fill(temp, .direction = 'downup') %>% 
  mutate(
    est_pt_cancer = temp * weight, 
    site_name = str_squish(str_replace_all(toupper(site_name), '[:punct:]|PLANT|LLC|CORPORATION|CORP', '')), 
    city = str_to_title(city)
  ) %>% 
  filter(!is.na(reported_emissions), reported_emissions > 1, !is.na(est_pt_cancer)) %>% 
  select(site_name, city, state, year, census_tract, reported_emissions, emissions_source,  est_pt_cancer, cancer_source)  
