library(sf)
library(tidyverse)

emissions <- read_rds('data/processed/eto/combined-eto.rds') %>%
  mutate(emissions = pmax(total_emissions_epa, total_emissions_nei, total_emissions_tri, na.rm = T), 
         zip = str_extract(zip, '[0-9]{5}')) %>% 
  filter(!is.na(census_tract), emissions > 10) %>% 
  group_by(zip, year, state) %>% 
  summarize(n_facilities = n(), 
            facilities = paste(site_name, collapse = '; '), 
            total_emissions = sum(emissions))

zips <- readxl::read_xlsx('/Users/asinghal/Downloads/TRACT_ZIP_122020.xlsx')

cancer <- st_read('data/processed/eto/neighbor-tracts.shp') %>% 
  st_drop_geometry() %>% 
  left_join(zips %>% select(geoid = TRACT, zip = ZIP), by = 'geoid') %>% 
  group_by(zip, year, state) %>% 
  summarize(n_census_tracts = n(), 
            census_tracts = paste(geoid, collapse = '; '), 
            population = sum(as.numeric(str_replace_all(population, ',', ''))), 
            max_pt_cancer_risk = max(pt_cancer_), 
            max_total_cancer_risk = max(total_canc)) %>% 
  left_join(emissions, by = c('zip', 'year','state')) %>% 
  arrange(state, year)

write_csv(cancer, 'data/temp/eto-emissions-cancer-population.csv')
