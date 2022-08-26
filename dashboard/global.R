library(shiny)
library(tidyverse)
library(ggiraph) 
library(geomtextpath)
library(sf)
library(terra)
library(mapdeck)


# Data files
emissions <- read_rds('data/combined-eto.rds') 
cancer <- vect('data/neighbor-tracts.shp')

# latlong <- data.frame(geom(centroids(cancer)))
cancer <- st_as_sf(cancer) %>% 
  mutate(log_pt_cancer = log(pt_cancer_)) %>% 
  filter(pt_cancer_ > 0)

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
