# install.packages("rgbif")
library(rgbif)
library(usethis) #edit_r_environ
library(readr) #read_tsv

# mapping
library(leaflet)
library(terra)
library(raster) #read tif files


library(dplyr) #load last

## Set GBIF credentials in r_environ
# https://docs.ropensci.org/rgbif/articles/gbif_credentials.html

edit_r_environ() #restart R after editing

#### test: single species ####

# get taxon key (usageKey)
taxonKey <- name_backbone("Heterocephalus glaber")$usageKey

gbif_download <- occ_download(
  pred("taxonKey", taxonKey),
  pred("hasCoordinate", TRUE), 
  pred("hasGeospatialIssue", FALSE), # remove GBIF default geospatial issues
  format = "SIMPLE_CSV")

occ_download_wait(gbif_download) 

d <- occ_download_get('0029363-231120084113126') %>%
  occ_download_import()

d <- occ_download_get(gbif_download) %>%
  occ_download_import()

#H_glaber <- read.csv("0029363-231120084113126.csv")

H_glaber <- read_tsv("0029363-231120084113126.csv", quote="") 



#### entire species list ####

species_list <- c("Heterocephalus glaber", "Fukomys damarensis", "Cryptomys hottentotus", 
                  "Fukomys mechowii", "Georychus capensis", "Fukomys anselli", 
                  "Heliophobius argenteocinereus", "Bathyergus suillus", "Bathyergus janetta", 
                  "Spalax ehrenbergi")
sociality <- c("social", "social", "social", "social", "solitary", "social", "solitary", "solitary", "solitary", "solitary")
body_mass_g <- c(34, 150, 90, 598, 265, 86, 161, 780, 385, 325)


spp.sociality <- data.frame(species_list, sociality,body_mass_g)

# match the names 
gbif_taxon_keys <- species_list %>% 
  name_backbone_checklist() %>% # match to backbone 
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey)

# download the data
gbif_download <- occ_download(
  pred_in("taxonKey", gbif_taxon_keys), # important to use pred_in
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV"
)

occ_download_wait(gbif_download) 

d <- occ_download_get(gbif_download) %>%
  occ_download_import()

## how many per species?
table(d$species)

## mapping
library(leaflet)
occ_coords <- d %>% 
  select(species, decimalLatitude, decimalLongitude) %>% 
  rename(latitude = decimalLatitude,
         longitude = decimalLongitude)

m <- leaflet(occ_coords)
m %>% addTiles() %>% 
  addCircles()

## remove presumed zoo occurrences 
# remove anything western hemisphere
# remove european points - any coords higher than 42

occ_coords_filtered <- occ_coords %>%
  filter(longitude > 0) %>% 
  filter(latitude < 42)

m2 <- leaflet(occ_coords_filtered)
m2 %>% addTiles() %>% 
  addCircles()

table(occ_coords_filtered$species)

#### get precip data ####
# download from: https://www.worldclim.org/data/worldclim21.html

# tif files are broken up by month (see readme). starting with first month to test
precip_jan <- raster("data/wc2.1_5m_prec/wc2.1_5m_prec_01.tif")
plot(precip_jan)

# Define your desired extent
min(occ_coords_filtered$longitude)
xmin <- 10
max(occ_coords_filtered$longitude)
xmax <- 47
min(occ_coords_filtered$latitude)
ymin <- -37
max(occ_coords_filtered$latitude)
ymax <- 40

# Create an extent object
clip_extent <- extent(xmin, xmax, ymin, ymax)

# Crop the raster based on the defined extent
clipped_jan<- crop(precip_jan, clip_extent)
plot(clipped_jan)
points(occ_coords_filtered$longitude, occ_coords_filtered$latitude, col = "red", pch = 20)

occ_coords_filtered2 <- left_join(occ_coords_filtered, spp.sociality, by = join_by("species" == "species_list"))
occ_coords_filtered2 <- occ_coords_filtered2 %>% 
  arrange(species)
write.csv(occ_coords_filtered2, "data/molerat_occurrence_sociality.csv")

#occ_coords_filtered2$sociality[which(occ_coords_filtered2$species=="Heliophobius argenteocinereus")] <- "solitary"


raster_df <- as.data.frame(clipped_jan, xy = TRUE)
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = wc2.1_5m_prec_01)) +
  scale_fill_gradient(low = "white", high = "blue") +  # Adjust the color gradient as needed
  theme_minimal()+
  geom_point(data = occ_coords_filtered2, aes(x = longitude, y = latitude, color = sociality))
