library(shiny)
library(tidyverse)
library(sf)
library(terra)
library(mapdeck)

# Data files
emissions <- read_rds('data/combined-eto.rds')
cancer <- vect('data/neighbor-tracts.shp')

plaintiff_tracts <- as.character(
  c(42077000101, 42077000102, 42077000400, 42077000500, 42077000600, 42077000700,
    42077000800, 42077000900, 42077001000, 42077001200, 42077001600, 42077001700,
    42077001800, 42077005702, 42077005703, 42077005704, 42077005901, 42077005902,
    42077006701, 42077006800, 42077009100, 42077009200, 42077009300, 42077009400,
    42077009500, 42077009600, 42077009700, 42095010300, 42095010700, 42095010800,
    42095010900, 42095011000, 42095016300, 42095017702, 42095017703)
)


# latlong <- data.frame(geom(centroids(cancer)))
cancer <- st_as_sf(cancer) %>% 
  mutate(log_pt_cancer = log(pt_cancer_), 
         plaintiff_tracts = ifelse(geoid %in% plaintiff_tracts, '#FFFFFF', '#808080')) %>% 
  filter(pt_cancer_ > 0)

emissions <- emissions %>% 
  filter(total_emissions_epa > 1)
# cancer <- data.frame(cancer) %>% 
#   mutate(lat = latlong$y, lon = latlong$x, 
#          log_pt_cancer = log(pt_cancer_)) %>% 
#   filter(pt_cancer_ > 0) 
