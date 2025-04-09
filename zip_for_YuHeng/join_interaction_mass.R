# load packages
library(tidyverse)
library(dplyr)

# read input data
canid_mass_yhl <- read_csv("data/canid_preyItems_mass_YHL.csv")
all_canids_filtered <- read_csv(file = "data/canids_filteredGlobi.csv", quote = "none")

# clean up mass df
canid_mass_cleaned <- canid_mass_yhl %>% 
  dplyr::rename(notes = `...10`) %>% 
  filter(!is.na(species)) %>% 
  filter(Mass_kg != "#N/A") %>% 
  mutate(Mass_kg = as.numeric(Mass_kg))

# join 
canid_full <- canid_mass_cleaned %>% 
  rename(targetTaxonName = species) %>% 
  left_join(all_canids_filtered, relationship = "many-to-many") %>% ## there are instances of cannibalism 
  select(sourceTaxonName, interactionTypeName, targetTaxonName, Mass_kg, Mass_source, decimalLatitude, decimalLongitude, localityName, sourceCitation)

write_csv(canid_full, "data/canid_interactions_mass_location.csv")