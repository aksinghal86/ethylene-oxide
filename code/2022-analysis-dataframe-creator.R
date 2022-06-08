library(tidyverse)
source('code/2022-analysis/2022-analysis-functions.R')

## Emissions data --------------------------------------------------------------
#TRI ETO data (from 2014 to 2020)
tri <- get_tri_emissions('ethylene oxide', 2014:2020) 
write_rds(tri, 'data/processed/eto/tri-eto.rds')

# NEI ETO data (2014 and 2017)
nei <- get_nei_emissions('ethylene oxide', 2014:2020)
write_rds(nei, 'data/processed/eto/nei-eto.rds')

# NATA/Air Tox Screen ETO data (2014 and 2017)
epa <- get_epa_emissions('ethylene oxide', 2014:2020)
write_rds(epa, 'data/processed/eto/epa-eto.rds')

## Cancer data -----------------------------------------------------------------
cancer <- get_epa_cancer('ethylene oxide', 2014:2020)
# Save as parquet file -- it's a huge dataset. 
arrow::write_parquet(cancer, 'data/processed/eto/epa-eto-cancer-all-tracts.parquet') 

## Combined emissions and cancer data ------------------------------------------
eto <- get_pollutant_data('ethylene oxide', 2014:2020, with_tri = T, with_cancer = T)
write_rds(eto, 'data/processed/eto/combined-eto.rds')

## Neighboring census tracts ---------------------------------------------------
eto <- read_rds('data/processed/eto/combined-eto.rds') %>% 
  mutate_at(vars(pt_cancer_risk, total_cancer_risk), as.numeric) %>% 
  filter(pt_cancer_risk > 10)

cancer <- arrow::read_parquet('data/processed/eto/epa-eto-cancer-all-tracts.parquet') %>%
  mutate_at(vars(pt_cancer_risk, total_cancer_risk), as.numeric)

nbrs <- neighbor_tracts(eto, cancer, parallel = F, within = 25)
terra::writeVector(nbrs, 'data/processed/eto/neighbor-tracts.shp', overwrite = T)





# 
# ## Old code --------------------------------------------------------------------
# #### Combine ETO data ---------------------------------------------------------
# # Combine the three emissions data while giving preference to NATA emissions
# # NATA appears to have modified emissions and since those are the emissions 
# # ultimately used in the models to estimate cancer risk, it is the most 
# # appropriate one to use. 
# 
# 
# replicate_nata <- function(nata_data, years) { 
#   # replicate nata data (eisid, naics, site_name, etc.) for which 
#   # data aren't available. Set total_emissions to NA since they are not available
#   # for these years
#   years %>% 
#     map_dfr(function (y) { 
#       nata_data %>% 
#         mutate(year = y, 
#                total_emissions = NA, 
#                data_source = NA)
#     })
# }
# 
# harmonize_cols <- function(nei_col, tri_col) { 
#   # combine columns where both nei and tri data are available. 
#   # nei data are given preference, i.e., 
#   # nei data selected if available; otherwise tri data selected
#   case_when(
#     is.na(nei_col) ~ tri_col,
#     TRUE ~ nei_col
#   )
# }
# 
# combined_eto <- nata %>% 
#   bind_rows(ats_eto) %>% 
#   bind_rows(replicate_nata(nata, c(2015:2016, 2018:2020))) %>% 
#   full_join(nei %>% select(year, state, eisid, total_emissions_nei = total_emissions, 
#                            latitude_nei = latitude, longitude_nei = longitude), 
#             by = c('year', 'state', 'eisid')) %>% 
#   full_join(tri %>% select(year, state, eisid, triid, frsid, total_emissions_tri = total_emissions, 
#                            latitude_tri = latitude, longitude_tri = longitude), 
#             by = c('year', 'state', 'eisid')) %>% 
#   mutate(latitude = harmonize_cols(latitude_nei, latitude_tri), 
#          longitude = harmonize_cols(longitude_nei, longitude_tri)) %>% 
#   select(-c(latitude_nei, latitude_tri, longitude_nei, longitude_tri))
# arrow::write_parquet(combined_eto, 'data/processed/eto-data.parquet')
# 
# 
# ## NATA/AirToxScreen cancer Risk Estimates--------------------------------------
# combined_eto <- arrow::read_parquet('data/processed/eto-data.parquet')
# ats_eto <- combined_eto %>% filter(year %in% c(2014, 2017))
# # convert to spatial for next step
# sp_ats_eto <- terra::vect(ats_eto, geom = c("longitude", "latitude")) 
# 
# # Processed only for NATA 2014 data right now. 
# nata_cancer <- arrow::read_parquet('data/temp/nata-cancer-2014.parquet') %>% 
#   filter(pollutant_name == 'ETHYLENE OXIDE')
# sp_nata_cancer <- spatialize_nata_cancer(nata_cancer) 
# terra::writeVector(sp_nata_cancer, 'data/processed/shape-files/nata-cancer-risk.shp', overwrite = T)
# 
# ats_cancer <- arrow::read_parquet('data/temp/ats-cancer-2017.parquet') %>% 
#   filter(pollutant_name == 'ETHYLENE OXIDE')
# sp_ats_cancer <- spatialize_nata_cancer(ats_cancer)
# terra::writeVector(sp_ats_cancer, 'data/processed/shape-files/ats-cancer-risk.shp', overwrite = T)
# 
# sp_cancer <- rbind(sp_nata_cancer, sp_ats_cancer)
# terra::writeVector(sp_cancer, 'data/processed/shape-files/combined-cancer-risk.shp', overwrite = T)
# 
# 
# # Meat of the work. Find the intersection, i.e., are lat/long inside the 
# # census tract polygon in the census tract. If so, then the EtO emitting facility
# # is in the census tract. 
# sp_dat2014 <- terra::intersect(sp_nata_cancer, sp_ats_eto[sp_ats_eto$year == 2014]) 
# sp_dat2017 <- terra::intersect(sp_nata_cancer, sp_ats_eto[sp_ats_eto$year == 2017])
# 
# sp_dat1 <- sf::st_as_sf(rbind(sp_dat2014, sp_dat2017)) %>% 
#   janitor::clean_names() %>% 
#   select(-ends_with("_1"))
# 
# # Convert lat,lon geometry to columns and save
# dat <- sp_dat1 %>% 
#   bind_cols(data.frame(sf::st_coordinates(sp_dat1)) %>% rename(longitude = X, latitude = Y)) %>%
#   mutate_at(vars(total_emissions, total_cancer_risk, pt_cancer_risk), as.numeric) %>% 
#   mutate(scaling_factor = total_cancer_risk/total_emissions) %>% 
#   sf::st_drop_geometry()
# write_csv(dat, 'data/processed/eto-emissions-and-cancer-risk.csv')
# 
# # add census tract geometry and save
# sp_dat2 <- sf::st_as_sf(spatialize_nata_cancer(dat)) %>%
#   janitor::clean_names() %>%
#   select(-contains('_x')) %>%
#   rename_at(vars(contains('_y')), ~ str_replace(.x, '_y', '')) 
# sp_dat2 <- rmapshaper::ms_simplify(sp_dat2)
# sp_dat2 <- terra::vect(sp_dat2)
# terra::writeVector(sp_dat2, 'data/processed/shape-files/eto-emissions-and-cancer-risk-with-census-tract-geom.shp',
#                    overwrite = T)
