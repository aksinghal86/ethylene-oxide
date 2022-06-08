library(shiny)

library(here)
library(tidyverse)
library(sf)
library(terra)

library(mapdeck)

# Data files
emissions <- read_rds(here('data/processed/eto/combined-eto.rds'))
cancer <- vect(here('data/processed/eto/neighbor-tracts.shp'))

# latlong <- data.frame(geom(centroids(cancer)))
cancer <- st_as_sf(cancer) %>% 
  mutate(log_pt_cancer = log(pt_cancer_)) %>% 
  filter(pt_cancer_ > 0)
# cancer <- data.frame(cancer) %>% 
#   mutate(lat = latlong$y, lon = latlong$x, 
#          log_pt_cancer = log(pt_cancer_)) %>% 
#   filter(pt_cancer_ > 0) 
