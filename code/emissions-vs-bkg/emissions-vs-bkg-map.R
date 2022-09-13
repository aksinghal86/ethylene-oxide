library(tidyverse)
library(sf)
library(terra)
library(ggspatial)
# library(tmap)
library(progress)

# Custom Functions 
source('code/2022-analysis/2022-analysis-functions.R')


### Create data files (unhash to create and save files)


# emissions <- get_epa_emissions('ethylene oxide', 2017) 
# cancer <- get_epa_cancer('ethylene oxide', 2017)
# 
# write_csv(emissions, 'code/emissions-vs-bkg/data/emissions-2017.csv')
# write_csv(cancer, 'code/emissions-vs-bkg/data/cancer-2017.csv')

cancer <- read_csv('code/emissions-vs-bkg/data/cancer-2017.csv') 

# Estimate ethylene oxide exposure concentration from cancer using EPA's IUR for 
# EtO of 9.1e-3 per ppb


# the best estimate for ambient air ethylene oxide concentrations for the year 2017 
# are 2018 measurements collected by the EPA around the US. Ethylene oxide data 
# were not collected for the year 2017. 

# https://www.epa.gov/sites/default/files/2019-11/documents/data_summary_stations.pdf

bkg <- 0.297

### Caculate fractional contribution from emissions relative to ambient air concentrations

cancer <- cancer %>% 
  mutate(pt_cancer_risk = ifelse(pt_cancer_risk == 0, sort(unique(pt_cancer_risk))[2], pt_cancer_risk), 
         ec = pt_cancer_risk * 1e-6 / 9.1e-3, 
         bkg = bkg, 
         pct_contribution = ec/bkg*100, 
         log_pct_contribution = log(pct_contribution)) %>% 
  filter(!epa_region == 'Entire US')

mainland_cancer <- cancer %>% filter(!state %in% c('AK', 'HI', 'VI', 'PR'))
ak_cancer <- cancer %>% filter(state == 'AK') 
hi_cancer <- cancer %>% filter(state == 'HI') 
vi_cancer <- cancer %>% filter(state == 'VI') 
pr_cancer <- cancer %>% filter(state == 'PR')

### Create maps
spatialize_with_progress <- function(df) { 
  pb$tick()
  st_as_sf(spatialize_tracts(df))
}

pb <- progress_bar$new(total = 5)
sp_dfs <- list(mainland_cancer, ak_cancer, hi_cancer, vi_cancer, pr_cancer) %>% 
  map(~ spatialize_with_progress(.))
names(sp_dfs) <- c('Mainland USA', 'Alaska', 'Hawaii', 'Virgin Islands', 'Puerto Rico')


create_map <- function(sp_df, name, trans = 'log') {
  pal <- colorRampPalette(c('#3a2f6b', '#36669c', '#41a0ae', '#3ec995', '#77f07f'))

  ggplot() + 
    geom_sf(data = sp_df, aes(fill = pct_contribution), color = NA) +
    theme_minimal() + 
    scale_fill_gradientn(trans = trans, 
                         colors = pal(10), 
                         name = '% contribution', 
                         labels = scales::number_format(accuracy = 0.001)) +
    labs(title = '% contribution of emissions relative to background emissions', 
         subtitle = name) + 
    annotation_scale(location = 'bl') +
    annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_fancy_orienteering) 
}

## Test (unhash to run)
# pr <- sp_dfs$`Puerto Rico`
# create_map(pr, 'Puerto Rico', trans = 'identity') 
# create_map(pr, 'Puerto Rico', trans = 'log') 

maps <- map2(sp_dfs, names(sp_dfs), create_map)
fnames <- paste0('code/emissions-vs-bkg/out/', names(sp_dfs), ' log scale.pdf')
walk2(fnames, maps, ggsave, height = 8, width = 8)

maps <- map2(sp_dfs, names(sp_dfs), create_map, trans = 'identity')
fnames <- paste0('code/emissions-vs-bkg/out/', names(sp_dfs), ' identity.pdf')
walk2(fnames, maps, ggsave, height = 8, width = 8)




