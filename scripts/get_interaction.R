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

diet_items_test <- get_prey_of(sp_list_canids)


# Split the target_taxon_path column by " | " and extract the last element
diet_items_df$finest_taxon <- sapply(str_split(diet_items_df$target_taxon_path, " \\| "), function(x) tail(x, n=1))


# took about 10 min to run  
# this worked but don't need to repeat everytime, just load RDS

# t1 <- classification(diet_items_df$finest_taxon, db = "ncbi")
# saveRDS(t1, "data/diet_items_taxize.RDS")

diet_items_taxize <- readRDS("data/diet_items_taxize.RDS")


tst <- extract_taxa(diet_items_taxize)

#### clean up taxonomy ----------------------------------

# All of these teps have been completed and don't need to be repeated (for now)

# # in terminal
# the input file on this code can't be uploaded to GitHub because it's too big, but the output file is there for the downstream steps

# time mlr --csv filter '$sourceTaxonName == "Otocyon megalotis" || $sourceTaxonName == "Vulpes pallida" || $sourceTaxonName == "Vulpes zerda" || $sourceTaxonName == "Cuon alpinus" || $sourceTaxonName == "Canis adustus" || $sourceTaxonName == "Canis mesomelas" || $sourceTaxonName == "Canis simensis" || $sourceTaxonName == "Lycaon pictus" || $sourceTaxonName == "Vulpes rueppelli" || $sourceTaxonName == "Vulpes chama" || $sourceTaxonName == "Urocyon littoralis" || $sourceTaxonName == "Vulpes velox" || $sourceTaxonName == "Canis rufus" || $sourceTaxonName == "Vulpes macrotis" || $sourceTaxonName == "Vulpes lagopus" || $sourceTaxonName == "Canis latrans" || $sourceTaxonName == "Urocyon cinereoargenteus" || $sourceTaxonName == "Vulpes vulpes" || $sourceTaxonName == "Canis lupus"' interactions.csv > selected_species.csv
# 

all_canids <- read_csv("data/selected_species.csv")

all_canids_filtered <- all_canids %>%
  filter(interactionTypeName == "eats" | interactionTypeName == "preysOn") %>%
  filter(!is.na(decimalLatitude)) %>%
  filter(!is.na(decimalLongitude)) %>%
  select(sourceTaxonName, interactionTypeName, targetTaxonName,
         targetTaxonPathNames, decimalLatitude, decimalLongitude)

table(all_canids_filtered$sourceTaxonName)

write_csv(all_canids_filtered, file = "data/canids_filteredGlobi.csv", quote = "none")

## resolve taxonomic names of target taxa

target_taxa <- unique(all_canids_filtered$targetTaxonName)

target_taxa_filtered <- tibble(target_taxa) %>%
  filter(str_detect(target_taxa, " ")) %>%
  filter(!str_detect(target_taxa, "egg")) %>%
  filter(!str_detect(target_taxa, "Domestic")) %>%
  filter(!str_detect(target_taxa, "adult"))%>%
  filter(!str_detect(target_taxa, "Anthropogenic")) %>%
  filter(!str_detect(target_taxa, "beef")) %>%
  filter(!str_detect(target_taxa, "Bird")) %>%
  filter(!str_detect(target_taxa, "bird"))  %>%
  filter(!str_detect(target_taxa, "bone"))  %>%
  filter(!str_detect(target_taxa, "contents")) %>%
  filter(!str_detect(target_taxa, "ear")) %>%
  filter(!str_detect(target_taxa, "feather")) %>%
  filter(!str_detect(target_taxa, "hair"))  %>%
  filter(!str_detect(target_taxa, "fur"))  %>%
  filter(!str_detect(target_taxa, "garbage"))  %>%
  filter(!str_detect(target_taxa, "Human"))  %>%
  filter(!str_detect(target_taxa, "Introduced"))  %>%
  filter(!str_detect(target_taxa, "Invertebrate"))  %>%
  filter(!str_detect(target_taxa, "invertebrate")) %>%
  filter(!str_detect(target_taxa, "invert")) %>%
  filter(!str_detect(target_taxa, "herbivore")) %>%
  filter(!str_detect(target_taxa, "mammal")) %>%
  filter(!str_detect(target_taxa, "seal")) %>%
  filter(!str_detect(target_taxa, "deer")) %>%
  filter(!str_detect(target_taxa, "reindeer")) %>%
  filter(!str_detect(target_taxa, "caribou")) %>%
  filter(!str_detect(target_taxa, "undetermined")) %>%
  filter(!str_detect(target_taxa, "lots")) %>%
  filter(!str_detect(target_taxa, "meat")) %>%
  filter(!str_detect(target_taxa, "grass")) %>%
  filter(!str_detect(target_taxa, "gull")) %>%
  filter(!str_detect(target_taxa, "prey")) %>%
  filter(!str_detect(target_taxa, "ungulate")) %>%
  filter(!str_detect(target_taxa, "boot")) %>%
  filter(!str_detect(target_taxa, "plastic")) %>%
  filter(!str_detect(target_taxa, "bufavirus")) %>%
  filter(!str_detect(target_taxa, "rodent")) %>%
  filter(!str_detect(target_taxa, "wood")) %>%
  filter(!str_detect(target_taxa, "scrap")) %>%
  filter(!str_detect(target_taxa, "stomach")) %>%
  filter(!str_detect(target_taxa, "carrion")) %>%
  filter(!str_detect(target_taxa, "fruit")) %>%
  filter(!str_detect(target_taxa, "brown")) %>%
  filter(!str_detect(target_taxa, "Rodent")) %>%
  filter(!str_detect(target_taxa, "ex")) %>%
  filter(!str_detect(target_taxa, "baby")) %>%
  filter(!str_detect(target_taxa, "snake")) %>%
  filter(!str_detect(target_taxa, "fiber")) %>%
  filter(!str_detect(target_taxa, "and")) %>%
  filter(!str_detect(target_taxa, "intestine"))


## Pseudiacris regilla == Pseudacris regilla
which(target_taxa_filtered$target_taxa == "Pseudiacris regilla") # 553
target_taxa_filtered$target_taxa[553] <- "Pseudacris regilla"

# library(taxize)
# 
# t2 <- classification(target_taxa_filtered$target_taxa, db = "ncbi")

# saveRDS(t2, file = "data/resolved_taxa.RDS")

resolved_taxa <- readRDS(file = "data/resolved_taxa.RDS")


# test
cleaned_names <- extract_taxa(resolved_taxa)

## exclude plants

metazoan_cleaned_names <- cleaned_names %>% 
  filter(kingdom == "Metazoa")


#### read in movement profile data
mps <- read_csv("data/raw_bodyMass.csv") %>% 
  filter(!is.na(scientific_name.x)) %>% 
  rename(species = scientific_name.x) %>% 
  filter(!is.na(Mass_kg)) %>% 
  filter(Mass_source != "")

has_mass <- mps$species

test <- metazoan_cleaned_names %>% 
  filter(species %in% has_mass)

meta_mass <- left_join(metazoan_cleaned_names, mps)


## something to repeat at the bottom
preyTaxa_perCanid <- diet_items_df %>% 
  group_by(source_taxon_name, finest_taxon) %>% 
  summarize(recordsPerTax = n()) %>% 
  ungroup()

numPreyItems_perCanid <- preyTaxa_perCanid %>% 
  group_by(source_taxon_name) %>% 
  summarise(nTax = n()) %>% 
  ungroup()


#### functions ####

extract_taxa <- function(input_df){
  cleaned_names <- tibble(species = as.character(), genus = as.character(),
                          family = as.character(), order = as.character(), 
                          class = as.character(), kingdom = as.character())
  
  for(i in 1:length(input_df)){
    t3 <- as.data.frame(input_df[i])
    if(length(t3) != 3){    ## skip those that don't have all the info
      next
    } else {
      colnames(t3) <- c("name","rank", "id")
      
      sp_df <- t3 %>% filter(rank == "species")
      sp_tmp <- sp_df$name
      
      genus_df <- t3 %>% filter(rank == "genus")
      genus_tmp <- genus_df$name
      
      family_df <- t3 %>% filter(rank == "family")
      family_tmp <- family_df$name
      
      order_df <- t3 %>% filter(rank == "order")
      order_tmp <- order_df$name
      
      class_df <- t3 %>% filter(rank == "class")
      class_tmp <- class_df$name
      
      kingdom_df <- t3 %>% filter(rank == "kingdom")
      kingdom_tmp <- kingdom_df$name
      
      new_row <- tibble(species = sp_tmp, genus = genus_tmp,
                        family = family_tmp, order = order_tmp, 
                        class = class_tmp, kingdom = kingdom_tmp)
      
      cleaned_names <- rbind(cleaned_names, new_row)
    }
  }
  return(cleaned_names)
}

