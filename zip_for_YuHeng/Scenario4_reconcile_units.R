# downloaded google sheet on 11/07/2025

library(readxl)
library(tidyverse)

metadata <- read_excel("data/Scenario4_carnivore_intraspecificComp.xlsx", 
                       sheet = "Paper_metadata") |> 
  dplyr::rename(Paper = paper)

wolf_dat <- read_excel("data/Scenario4_carnivore_intraspecificComp.xlsx", 
                       sheet = "gray wolf")

combine_dat <- full_join(wolf_dat, metadata, by = "Paper")

## There is some clean up in paper names that needs to happen! this is just proof of concept

combine_dat2 <- combine_dat |> 
  mutate(TL_wolves = (as.numeric(`number of social groups in study`) * as.numeric(mean_group_size))) |> 
  mutate(wolves_p_km2 = TL_wolves / as.numeric(`study area (km^2)`))

hist(combine_dat2$wolves_p_km2)
