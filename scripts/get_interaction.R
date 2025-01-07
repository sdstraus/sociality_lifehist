# Date created: 21 July 2023
# Date updated: 20 March 2024
# Updated by: Sam Straus


# clear environment
rm(list=ls()) 

# # LIBRARIES # #

library("tibble")
library('rglobi')
library(googlesheets4)

library(tidyverse)
library("dplyr")
# 
# # # FUNCTIONS # #
# source("scripts/functions.R")


# PLAYING WITH GLOBI #------------------

interaction_types <- get_interaction_types()$interaction
interaction_types

# applies get_interactions(taxon = "scientific name", interaction.type = c(list))
# e.g.
test <- get_interactions(taxon = "Lycalopex culpaeus", interaction.type = c("preysOn", "kills"))
## ok, for some reason records are duplicated...

# get the count of the number of unique taxon names, to start
# unique() should account for duplicates
num.prey <- length(unique(test$target_taxon_name))

# PULL INTERACTION TYPES #------------------------

## need to write a loop that does:
# pulls a list of interactions types (eats, preysOn, kills, pollinates, acquiresNutrientsFrom)

# Setting up for loop
molerats <- read_sheet("https://docs.google.com/spreadsheets/d/1sMZ-3EmizxqmBXtAy9CUUO0NXhhF_wQr-M39CHHe0FE/edit#gid=1742969443")

# create empty df to fill with num unique food items
new_df <- data.frame(species = molerats$SPECIES, num.foods = rep(0, times = length(species)))

# create list of interaction types
interaction_types <- c("eats", "preysOn", "kills", "pollinates", "acquiresNutrientsFrom")

for(sp in 1:length(new_df$species)){
  sp_name <- new_df$species[sp]
  temp_df <- get_interactions(taxon = sp_name, interaction.type = c("eats", "preysOn", "kills", "pollinates", "acquiresNutrientsFrom"))
  new_df$num.foods[sp] <- length(unique(temp_df$target_taxon_name))
}


## Test
sp_name <- new_df$species[1]
test <- get_interactions(taxon = sp_name, 
                         interaction.type = c("eats", "preysOn", "kills", "pollinates", "acquiresNutrientsFrom"))

get_interactions_by_taxa(sourcetaxon = molerats$SPECIES[1]) # tried older name


## try with bees, maybe more info
sp_list_bees <- c("Xylocopa virginica", "Lasioglossum calceatum",  "Apis mellifera", 
                  "Lasioglossum umbripenne", "Xylocopa violacea")

new_df <- data.frame(species = sp_list_bees, num.foods = rep(0, times = length(sp_list_bees)))

for(sp in 1:length(sp_list_bees)){
  sp_name <- sp_list_bees[sp]
  temp_df <- get_interactions(taxon = sp_name, interaction.type = c("eats", "preysOn", "kills", "pollinates", "acquiresNutrientsFrom"))
  new_df$num.foods[sp] <- length(unique(temp_df$target_taxon_name))
}



#### Canids ######

sp_list_canids <- c("Otocyon megalotis", "Vulpes pallida" , "Vulpes zerda", "Cuon alpinus", 
                    "Canis adustus", "Canis mesomelas", "Canis simensis", "Lycaon pictus", 
                    "Vulpes rueppelli", "Vulpes chama", "Urocyon littoralis", "Vulpes velox", 
                    "Canis rufus", "Vulpes macrotis", "Vulpes lagopus", "Canis latrans", 
                    "Urocyon cinereoargenteus")

# , "Vulpes vulpes", "Canis lupus"

new_df <- data.frame(species = sp_list_canids, num.foods = rep(0, times = length(sp_list_canids)))

for(sp in 1:length(sp_list_canids)){
  if(sp == 1){  
    sp_name <- sp_list_canids[sp]
    temp_df <- get_interactions(taxon = sp_name, interaction.type = c("eats", "preysOn", "kills", "acquiresNutrientsFrom"))
    new_df$num.foods[sp] <- length(unique(temp_df$target_taxon_name))
    diet_items_df <- temp_df
  } else {
    sp_name <- sp_list_canids[sp]
    temp_df <- get_interactions(taxon = sp_name, interaction.type = c("eats", "preysOn", "kills", "acquiresNutrientsFrom"))
    new_df$num.foods[sp] <- length(unique(temp_df$target_taxon_name))
    diet_items_df <- rbind(diet_items_df, temp_df)
  }
}


# Split the target_taxon_path column by " | " and extract the last element
diet_items_df$finest_taxon <- sapply(str_split(diet_items_df$target_taxon_path, " \\| "), function(x) tail(x, n=1))

preyTaxa_perCanid <- diet_items_df %>% 
  group_by(source_taxon_name, finest_taxon) %>% 
  summarize(recordsPerTax = n()) %>% 
  ungroup()

numPreyItems_perCanid <- preyTaxa_perCanid %>% 
  group_by(source_taxon_name) %>% 
  summarise(nTax = n()) %>% 
  ungroup()
  

#########

library(taxize)
gnr_resolve(sci = 'Dialictus umbripenne') %>% View()

# OTHER SOURCES OF DATA #-----------------
usgs_poldat <- read.csv("data/NPWRC_only_PollinatorLibraryData04.15.2020.csv")
View(usgs_poldat)

which(usgs_poldat$Insect_Scientific_Name == sp_list_bees[5]) # only honey bees


braz_bees <- read.csv("data/Brazilian_bees_crop_pollinators.csv")
which(usgs_poldat$Insect_Scientific_Name == sp_list_bees[5]) # only honey bees
