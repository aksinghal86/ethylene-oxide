library(odbc)
library(tidyverse)
library(DBI)
library(dbplyr)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "172.16.192.69",
                 Database = "DEV",
                 UID = "a.singhal",
                 PWD = rstudioapi::askForPassword("Database password"), 
                 port = 1443)
table <- tbl(con, 'tri_1a_hist')
eto <- table %>% filter(chemical_name == 'ETHYLENE OXIDE')
colnames(eto)

eto <- eto %>% 
  select(year = reporting_year, triid = trifd, facility_name:facility_state, zip = facility_zip_code, parent_company_name:frs_facility_id, 
         cas_number, chemical_name, 
         unit_of_measure, t_fugitive_air_emissions, t_stack_air_emissions, t_air_emissions, t_on_site_releases) %>% 
  data.frame() %>% 
  tibble()

eto <- eto %>% 
  mutate(zip = str_extract(str_pad(zip, 5, 'left', 0), '[0-9]{5}'), 
         across(c(t_fugitive_air_emissions, t_stack_air_emissions, t_air_emissions, t_on_site_releases), as.numeric))


psum <- purrr::partial(sum, na.rm = T)

eto_smry <- eto %>% 
  group_by(year, zip, state = facility_state) %>% 
  summarize(facilities = paste0(facility_name, collapse = ', '), 
            total_emissions = psum(t_air_emissions), 
            units = 'lbs') %>% 
  filter(total_emissions > 0)
write_csv(eto_smry, 'code/epi-analysis/data/eto-emissions-by-zip-1987-2018.csv')
