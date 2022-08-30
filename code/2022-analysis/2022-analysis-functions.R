library(tidyverse)
library(sf)
library(terra)
library(foreach)

## Data cleaners ---------------------------------------------------------------
assign_census_tract <- function(dat, tracts_file = 'data/processed/shape-files/census-tracts.shp') { 
  message('Reading census tract file') 
  tracts <- st_read(tracts_file, quiet = T)
  
  message('Converting provided data set to SpatVector form')
  sp_dat <- st_as_sf(dat, coords = c('longitude', 'latitude'), crs = st_crs(tracts))

  message('Finding the intersection between lat/lon and census tract to assign census tract to each location and assigning census tract.', 
          'Will take some time...')
  # Find the intersection. Returns a sparse matrix. Each element of a sparse 
  # matrix contains the index of tracts data frame. The data frame will be smaller
  # than the original provided because some coordinates are not associated with 
  # census tracts, e.g., facilities offshore on federal lands. 
  m <- sf::st_intersects(sp_dat, tracts)
  found_tracts <- tracts[unlist(m), ] %>% 
    st_drop_geometry() %>% 
    select(census_tract = geoid)
  
  # Filter data same as tracts so that the sizes are same, i.e., only keep those 
  # entries for which a census tract was located. 
  dat_contains_tract <- dat[lengths(m)>0, ] %>% bind_cols(found_tracts)
  dat_no_tract <- dat[lengths(m) == 0, ]
  final <- dat_contains_tract %>% bind_rows(dat_no_tract)
  
  return(final)
}

create_facility_db <- function(nei_folder = 'data/raw/nei/', save_to = 'data/processed/') { 
  # NEI database is the most harmonized and clean database. Use this to create
  # a facilities database. Helpful for referencing facilities when analyzing. 
  # This is a one-time function every time a new NEI database is released. 
  files <- fs::dir_ls(nei_folder, regexp = 'nei.+\\.csv')
  
  message('Reading 2017 NEI emissions file')
  f2017 <- read_csv(files[str_detect(files, '2017')], col_types = cols(.default = 'c'), name_repair = 'minimal') %>%
    janitor::clean_names() %>%
    distinct(eisid = eis_facility_id, agencyid = agency_facility_id, triid = tri_facility_id,
             site_name, address, city, zip = zip_code, fips = fips_code, county, state, naics_code, naics_description,
             latitude = site_latitude, longitude = site_longitude)
  message('Reading 2014 NEI emissions file')
  f2014 <- read_csv(files[str_detect(files, '2014')], col_types = cols(.default = 'c'), name_repair = 'minimal') %>% 
    janitor::clean_names() %>% 
    distinct(eisid = eis_facility_site_id, agencyid = alt_agency_id, site_name = facility_site_name,
             address = location_address_text, city = locality, zip = address_postal_code, fips = state_and_county_fips_code,
             county = county_name, state = st_usps_cd, naics_code = naics_cd, naics_description,
             latitude = latitude_msr, longitude = longitude_msr)
  
  message('Creating a unique facilities dataframe')
  facilities <- bind_rows(f2017, f2014) %>% 
    distinct(eisid, .keep_all = T) %>% 
    filter(!is.na(latitude)) %>% 
    assign_census_tract()

  write_csv(facilities, paste0(save_to, 'facilities-database.csv'))
  rm(f2017, f2014, facilities)
  gc()
  
  message("Data saved in ", nei_folder, ' as facilities-database.csv')
}

clean_nei_data <- function(nei_folder = 'data/raw/nei/', save_to = 'data/processed/') { 
  files <- fs::dir_ls(nei_folder, regexp = 'nei.+\\.csv')
  message('Following files have NEI data ', files)
  
  sanitize <- function(d, f) {
    year <- as.numeric(str_extract(f, '[0-9]{4}'))
    
    if (year == 2017 & !str_detect(f, 'unknown')) { 
      d %>%
        janitor::clean_names() %>%
        select(eisid = eis_facility_id, pollutant = pollutant_desc, total_emissions, units = emissions_uom) %>% 
        mutate(year = year) 
    } else if (year == 2014 & !str_detect(f, 'tribes')) { 
      d %>% 
        janitor::clean_names() %>% 
        select(eisid = eis_facility_site_id, pollutant = pollutant_desc, total_emissions, units = uom) %>% 
        mutate(year = year) 
    } 
  }
  
  message('Cleaning and combining NEI data. May take some time depending on the size of files...')
  facilities <- read_csv('data/processed/facilities-database.csv', col_types = cols(.default = 'c'))
  dat <- files %>% 
    map(function (fname) { 
      message('Reading ', fname)
      read_csv(fname, col_types = cols(.default = 'c'), name_repair = 'minimal') %>%
        sanitize(fname) 
    }) %>% 
    bind_rows() %>% 
    left_join(facilities, by = 'eisid') %>% 
    mutate(pollutant = str_to_title(pollutant)) %>% 
    mutate_at(vars(total_emissions, latitude, longitude), as.numeric) %>%
    filter(total_emissions > 0, !is.na(total_emissions)) 
  
  arrow::write_parquet(dat, paste0(save_to, 'nei-data.parquet'), compression = 'gzip')
  rm(dat)
  gc()
  
  message("Processed NEI Data saved in ", nei_folder, " as 'nei-data.parquet'")
}

clean_nata2014_emissions <- function(epa_folder = 'data/raw/AirToxScreen/') { 
  read_csv(paste0(epa_folder, 'Nata_aermod_2014_facility_group.txt'), col_types = cols(.default = 'c'), name_repair = 'minimal') %>% 
    janitor::clean_names() %>% 
    select(eisid = eis_id, pollutant = nei_poll_desc, total_emissions = nei_emis_tons) %>%
    mutate(year = 2014, units = 'tons', pollutant = str_to_title(pollutant))
}


clean_ats2017_emissions <- function(epa_folder = 'data/raw/AirToxScreen/') { 
  ats_emissions <- read_csv(paste0(epa_folder, 'point_fac_2017_emissions.txt'), col_types = cols(.default = 'c'), name_repair = 'minimal') 
  ats_emissions %>% 
    pivot_longer(`1,1,2,2-Tetrachloroethane (Year 2017 tons)`:`p-Xylene (Year 2017 tons)`, 
                 names_to = 'pollutant', values_to = 'total_emissions') %>%
    janitor::clean_names() %>% 
    select(eisid = facility_id, pollutant, total_emissions) %>% 
    mutate(year = 2017, 
           units = case_when(str_detect(pollutant, 'tons') ~ 'tons'), 
           pollutant = str_to_title(str_replace(pollutant, ' \\(Year.+\\)$', '')))
}

clean_epa_emissions_data <- function(epa_folder = 'data/raw/AirToxScreen/', save_to = 'data/processed/') { 
  facilities <- read_csv('data/processed/facilities-database.csv', col_types = cols(.default = 'c'))
  nata2014 <- clean_nata2014_emissions(epa_folder = epa_folder)
  ats2017 <- clean_ats2017_emissions(epa_folder = epa_folder)
  
  dat <- bind_rows(nata2014, ats2017) %>% 
    left_join(facilities, by = 'eisid') %>% 
    mutate_at(vars(total_emissions, latitude, longitude), as.numeric) %>%
    filter(total_emissions>0, !is.na(total_emissions))
  
  arrow::write_parquet(dat, paste0(save_to, 'epa-emissions-data.parquet'), compression = 'gzip')
  rm(dat)
  gc()
  
  message("Data saved in ", epa_folder, " as 'epa-emissions-data.parquet'")
}

clean_tri_data <- function(tri_folder = 'data/raw/tri/', save_to = 'data/processed/') {
  files <- fs::dir_ls(tri_folder, regexp = 'tri.+\\.csv')
  message('Following files have TRI data ', files)

  sanitize <- function(d, f) {
    year <- as.numeric(str_extract(f, '[0-9]{4}'))
  
    if (between(year, 2014, 2019)) {
      d %>%
        select(1:3, 34, 36, 44:48, 59, 62, 82, 88, 91, 98, 101, 111) %>%
        rename_all(~ str_replace(., '^[0-9]+. ', '')) %>%
        rename(casrn = `CAS #/COMPOUND ID`) %>%
        janitor::clean_names()
    } else if (between(year, 2020, 2021)) {
      d %>%
        select(1:3, 34, 37, 46:50, 61, 64, 84, 90, 93, 100, 103, 113) %>%
        rename_all(~ str_replace(., '^[0-9]+. ', '')) %>%
        rename(casrn = `CAS#`) %>%
        janitor::clean_names()
    }
  }
  
  message('Cleaning and combining TRI data. May take some time depending on the size of files...')
  facilities <- read_csv('data/processed/facilities-database.csv', col_types = cols(.default = 'c'))
  dat <- files %>%
    map(function (fname) {
      message('Reading ', fname)
      read_csv(fname, col_types = cols(.default = 'c'), name_repair = 'minimal') %>%
        sanitize(fname)
    }) %>% 
    bind_rows() %>%
    rename(units = unit_of_measure, triid = trifd, frsid = frs_id, 
           pollutant = chemical, total_emissions = total_releases) %>%
    rename_at(vars(contains('x5_'), contains('x8_')), str_replace, 'x[0-9]_[0-9]_', '') %>% 
    left_join(facilities, by = 'triid') %>% 
    mutate_at(vars(pollutant), str_to_title) %>%
    mutate_at(vars(year, fugitive_air:treatment_on_site, latitude, longitude), as.numeric) %>% 
    filter(total_emissions > 0, !is.na(total_emissions), units == 'Pounds') %>%
    mutate(units = 'Lbs')

  arrow::write_parquet(dat, paste0(save_to, 'tri-data.parquet'), compression = 'gzip')

  rm(dat)
  gc()
  message("Processed TRI Data saved in ", tri_folder, " as 'tri-data.parquet'")
}


clean_cancer_by_year <- function(yr, epa_folder = 'data/raw/AirToxScreen/') { 
  file <- fs::dir_ls(epa_folder, regexp = paste0(yr, '.+cancer.+\\.txt'))
  
  message('Processing ', yr, ' cancer file...')
  cancer <- read_csv(file, col_types = cols(.default = 'c'), name_repair = 'minimal')
  cancer %>% 
    pivot_longer(`1,1,2-TRICHLOROETHANE`:`PAHPOM`, names_to = 'pollutant', values_to = 'pt_cancer_risk') %>% 
    mutate(pollutant = str_to_title(pollutant), 
           year = yr, 
           units = "per million") %>% 
    janitor::clean_names() %>% 
    rename(total_cancer_risk = contains('total'), fips = contains('fi_ps'), census_tract = tract)
}

clean_epa_cancer_data <- function(epa_folder = 'data/raw/AirToxScreen/') { 
  cancer <- c(2014, 2017) %>% map_dfr(clean_cancer_by_year)
  arrow::write_parquet(cancer, 'data/processed/epa-cancer.parquet')
}



## Data fetchers ---------------------------------------------------------------

get_nei_emissions <- function(pollutants, years) {
  pollutants <- str_to_title(pollutants)
  message('Reading NEI emissions...')
  arrow::read_parquet('data/processed/nei-data.parquet') %>% 
    filter(pollutant %in% pollutants, year %in% years) %>% 
    mutate(total_emissions = ifelse(tolower(units) %in% c('ton', 'tons'), total_emissions * 2000, total_emissions), 
           units = 'lbs')
}

get_tri_emissions <- function(pollutants, years) {
  pollutants <- str_to_title(pollutants)
  message('Reading TRI emissions...')
  arrow::read_parquet('data/processed/tri-data.parquet') %>% 
    filter(pollutant %in% pollutants, year %in% years) %>% 
    mutate(total_emissions = ifelse(tolower(units) %in% c('ton', 'tons'), total_emissions * 2000, total_emissions), 
           units = 'lbs')
}

get_epa_emissions <- function(pollutants, years) {
  pollutants <- str_to_title(pollutants)
  message('Reading EPA emissions...')
  arrow::read_parquet('data/processed/epa-emissions-data.parquet') %>%
    filter(pollutant %in% pollutants, year %in% years) %>% 
    mutate(total_emissions = ifelse(tolower(units) %in% c('ton', 'tons'), total_emissions * 2000, total_emissions),
           units = 'lbs')
}

get_epa_cancer <- function(pollutants, years) { 
  pollutants <- str_to_title(pollutants)
  message('Reading EPA cancer data...')
  arrow::read_parquet('data/processed/epa-cancer.parquet') %>% 
    filter(pollutant %in% pollutants, year %in% years)
}

get_pollutant_data <- function(pollutants, years, with_tri = F, with_cancer = F) { 
  # Interesting note: can use individual functions to fetch data but it is 
  # 3x to 4x slower than using vectorization as done here
  
  message('Reading data files...')
  pollutants <- str_to_title(pollutants)
  emission_files <- if(with_tri) {
    paste0('data/processed/', c('nei-data', 'tri-data', 'epa-emissions-data'), '.parquet') 
  } else { 
    paste0('data/processed/', c('nei-data', 'epa-emissions-data'), '.parquet') 
  }

  emissions <- emission_files %>% map_dfr(function (f) {
    arrow::read_parquet(f) %>%
      mutate(data_source = str_extract(f, 'nei|tri|epa'))
  }) %>%
    filter(pollutant %in% pollutants, year %in% years) %>% 
    select(eisid:data_source) %>%
    mutate(
      total_emissions = case_when(
        tolower(units) %in% c('ton', 'tons') ~ total_emissions * 2000,
        tolower(units) %in% c('gram', 'grams') ~ total_emissions/453.59,
        TRUE ~ total_emissions),
      units = 'lbs') 
  
  if (with_cancer) { 
    message("Attaching cancer data...")
    message('This can take a few minutes. Please be patient.')
    cancer <- arrow::read_parquet('data/processed/epa-cancer.parquet') %>%
      filter(pollutant %in% pollutants, year %in% years) %>%
      select(year, census_tract, pt_cancer_risk, total_cancer_risk)
    emissions <- emissions %>% 
      pivot_wider(names_from = data_source, values_from = total_emissions, names_prefix = 'total_emissions_') %>%
      left_join(cancer, by = c('year', 'census_tract'))
  }
 
  return(emissions)
}

## Spatial functions -----------------------------------------------------------
spatialize_tracts <- function(dat, tracts_file = 'data/processed/shape-files/census-tracts.shp', colname = 'census_tract') { 
  tracts <- terra::vect(tracts_file)
  sp_dat <- terra::merge(tracts[, c('geoid')], dat, by.x = 'geoid', by.y = colname)
  rm(tracts)
  gc()
  return(sp_dat)
}

neighbor_tracts <- function(emissions, cancers, within = 25, parallel = F, ncores = 1) { 
  message('Spatializing census tracts...')
  
  sp_emissions <- spatialize_tracts(emissions %>% distinct(census_tract))
  sp_cancers <- spatialize_tracts(cancers)
  
  if(!parallel) { 
    message('Finding neighbors for ', nrow(sp_emissions), ' unique census tracts without parallel processing. May take a few minutes...')
    s1 <- centroids(sp_emissions)
    s2 <- centroids(sp_cancers)
    dists <- distance(s1, s2) 
  }
  
  if (parallel) {
    s1 <- st_centroid(st_as_sf(sp_emissions))
    s2 <- st_centroid(st_as_sf(sp_cancers))

    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    L <- nrow(s1)
    inds <- split(seq_len(L), sort(rep_len(seq_len(ncores), L)))

    dists <- foreach(l = seq_along(inds), .combine = rbind, .packages = 'sf') %dopar% {
      sf::st_distance(s1[inds[[l]], ], s2)
    }
  }
  d1 <- dists < (within*1000)
  sp_cancers[as.logical(apply(d1, 2, sum)), ]
}
