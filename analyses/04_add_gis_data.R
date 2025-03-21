# Header #############################################################
#
# Author: Romain Frelat
# Email:  romain.frelat@fondationbiodiversite.fr
#
# Date: 2025-03-20
#
# Script Description: create shapefile and get gis information



# Libraries etc -----------------------------------------------------------
library(terra)
library(sf)
library(here)

read_folder <- here("data", "02_clean")
data_folder <- here("data", "gis")
extdata_folder <- here("~/OneDrive/Documents/Data/")

## Read data -----
df_steli <- readRDS(file.path(read_folder, "steli.rds"))
df_atlas <- readRDS(file.path(read_folder, "atlas.rds"))

coocol <- c("decimalLongitude", "decimalLatitude")
coo <- rbind(df_steli[,coocol], df_atlas[,coocol])

coo <- coo[!duplicated(coo),]
dim(coo) # 263427 unique coordinates

# create vector spatial layer
shp <- st_as_sf(coo, coords = coocol, crs = 4326)

# add an id
id_coo <- paste(coo[,1], coo[,2], sep="_")
shp$id_coo <- id_coo

# Export shapefile, if needed 
# st_write(shp, here(data_folder,"shape_all.shp"), append=FALSE)


## Get and extract GIS data --------------------

# administrative regions -----------
gadm <- vect(here(extdata_folder,"gadm", "gadm41_FRA_2.shp"))
gadm_points <- extract(gadm, vect(shp)) # take some time to compute ...
# table(gadm_points$NAME_1, useNA="ifany")

# elevation ------------
# the best source of data ae EU scale is Copernicus GLO-30
# https://dataspace.copernicus.eu/explore-data/data-collections/copernicus-contributing-missions/collections-description/COP-DEM
# imported from Google Earth Engine
glo30 <- read.csv(here(data_folder, "gee_copdem_elevation.csv"))
glo30 <- glo30[order(glo30$FID),]
# glo30$first[glo30$first<0] <- 0


# Bioclimatic regions ---------
# Metzger et al. 2013 https://doi.org/10.1111/geb.12022 
gens <-  rast(here(data_folder, "eu_croped_gens_v3.tif"))
meta_gens <- read.csv(here(data_folder, "GEnS_v3_classification.csv"))
gens_points <- extract(gens, shp)
gens_points$gens_name <- meta_gens$GEnZname[match(gens_points$eu_croped_gens_v3, meta_gens$GEnS_seq)]


# population density 2006 ---------
# https://www.eea.europa.eu/en/datahub/datahubitem-view/5884f314-84a9-4f53-a745-d94d7a53e8b1?activeAccordion=1083668%2C761
pop <-  rast(here(data_folder, "Grid_ETRS89_LAEA_1K_ref_GEOSTAT_2006.tif"))
# get the vector in ETRS89_LAEA projection
shp_3035 <- st_transform(shp, crs = crs(pop))
# extract the values of pop. density
pop_points <- extract(pop, shp_3035)
# NA means 0
pop_points$Grid_ETRS89_LAEA_1K_ref_GEOSTAT_2006[is.na(pop_points$Grid_ETRS89_LAEA_1K_ref_GEOSTAT_2006)] <- 0

# CORINE land cover 2018 -----------------
# https://land.copernicus.eu/en/products/corine-land-cover/clc2018
clc <- rast(here(extdata_folder,"Corine", "u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"))
# extract the values of land cover
clc_points <- extract(clc, shp_3035)
# table(clc_points$LABEL3)


## Summarize and export -----------------
coo_3035 <- st_coordinates(shp_3035)

gis_info <- data.frame(
  "longitude"=coo$decimalLongitude,
  "latitude"=coo$decimalLatitude,
  "x_ETRS89_LAEA"=coo_3035[,1],
  "y_ETRS89_LAEA"=coo_3035[,2],
  "region"=gadm_points$NAME_1,
  "departement"=gadm_points$NAME_2,
  "elevation_m"= glo30$first,
  "GEnS_v3_bioclim" = gens_points$gens_name,
  "popdensity_hab_per_km2" = pop_points$Grid_ETRS89_LAEA_1K_ref_GEOSTAT_2006,
  "CLC2018_landcover" = clc_points$LABEL3
)

write.csv(gis_info, here(read_folder, "gis_info.csv"))
