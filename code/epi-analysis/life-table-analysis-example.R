
# https://www.cdc.gov/niosh/ltas/default.html
# https://cran.r-project.org/web/packages/LTASR/LTASR.pdf

library(LTASR)
library(tidyverse)

#Import example person file
person <- person_example %>%
  mutate(dob = as.Date(dob, format='%m/%d/%Y'),
         pybegin = as.Date(pybegin, format='%m/%d/%Y'),
         dlo = as.Date(dlo, format='%m/%d/%Y'))

#Import default rate object
rateobj <- us_119ucod_19602020
rates <- rateobj["rates"] %>% 
  as.data.frame()


#Stratify person table
py_table <- get_table(person, rateobj)

SMR <- smr_minor(py_table, rateobj)

