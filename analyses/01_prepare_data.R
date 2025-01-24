# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2025-01-24
#
# Script Description: prepare data


# Libraries etc -----------------------------------------------------------
library(here)

# Data wrangling
library(data.table)
library(readxl)
library(rgbif)
library(stringr)

# Spatial analysis
library(sf)

# Paths
fun_folder <- here("functions")
read_folder <- here("data/01_raw")
write_folder <- here("data/02_clean")

# Own functions
source(file.path(fun_folder, "format_data.R"))


# Read data ---------------------------------------------------------------

df_steli <- fread(file.path(read_folder,
                            "STELI_data_FR_DMS.csv"))

df_atlas <- fread(file.path(read_folder,
                            "France Opportunistics data (Opie)/odonata_202410091558.csv"))


# Standardize column names ------------------------------------------------

names_key <- read_excel(file.path(here("data"),
                                  "column_names.xlsx"),
                        sheet = 1)
names_key <- data.table(names_key)


df_steli <- rename_cols(key = names_key,
                        dtable = df_steli,
                        nam = "STELI")

df_atlas <- rename_cols(key = names_key,
                        dtable = df_atlas,
                        nam = "Atlas")


# Clean data in columns ---------------------------------------------------

## Species names -----

df_steli[scientificName == "Calopteryx groupe splendens",
         scientificName := "Calopteryx splendens"]
df_steli[scientificName == "Leste groupe sponsa",
         scientificName := "Lestes sponsa"]
df_steli[scientificName == "Leste groupe viridis",
         scientificName := "Lestes viridis"]
df_steli[scientificName == "Agrion Porte-coupe/vander Linden",
         scientificName := "Enallagma cyathigerum"]
df_steli[scientificName == "Aeschne groupe cyanea",
         scientificName := "Aeshna cyanea"]
df_steli[scientificName == "Aeschne groupe cyanea",
         scientificName := "Aeshna cyanea"]

# Check against GBIF backbone
steli_names <- name_backbone_checklist(unique(df_steli$scientificName))
atlas_names <- name_backbone_checklist(unique(df_atlas$scientificName))

# Check match types (ideally, exact only)
unique(steli_names$matchType)
# STELI has some non-species names
steli_names$verbatim_name[steli_names$matchType == "NONE"]

unique(atlas_names$matchType)

steli_names <- data.table(steli_names[, c("canonicalName",
                                          "verbatim_name",
                                          "genus",
                                          "family",
                                          "rank",
                                          "usageKey")])
atlas_names <- data.table(atlas_names[, c("canonicalName",
                                          "verbatim_name",
                                          "genus",
                                          "family",
                                          "rank",
                                          "usageKey")])

df_steli <- steli_names[df_steli,
                        on = c("verbatim_name" = "scientificName")]
df_atlas <- steli_names[df_atlas,
                        on = c("verbatim_name" = "scientificName")]

setnames(df_steli,
         old = c("canonicalName", "verbatim_name", "rank", "usageKey"),
         new = c("scientificName", "verbatimName", "taxonRank", "taxonID"))
setnames(df_atlas,
         old = c("canonicalName", "verbatim_name", "rank", "usageKey"),
         new = c("scientificName", "verbatimName", "taxonRank", "taxonID"))

## Dates -----
df_steli[, eventDate := as.IDate(eventDate, format =  "%d/%m/%Y")]
df_atlas[, eventDate := as.IDate(eventDate, format =  "%Y-%m-%d")]

## Times -----

# Hour and sampling effort with STELI
df_steli[, eventTime := as.ITime(eventTime)]

effort_char <- df_steli$samplingEffort # Some negative values
# -> I suspect start and end dates have been inverted
effort_char[effort_char == ""] <- NA

effort <- str_split(effort_char, ":")

effort_min <- lapply(effort,
                     function(e) {
                       as.numeric(e[1])*60+ as.numeric(e[2]) + as.numeric(e[3])/60
                     })
effort_min <- unlist(effort_min)

df_steli[, samplingEffort := effort_min]

## Coordinates -----
coord_atlas <- st_as_text(st_as_sf(df_atlas, coords = c("decimalLongitude",
                                                        "decimalLatitude"),
                                   na.fail = FALSE)$geometry)

df_atlas[, decimalCoordinates := coord_atlas]

setnames(df_steli,
         old = c("lon centroid site", "lat centroid site"),
         new = c("decimalLongitude", "decimalLatitude"))

## Reorder columns -----
cnames_steli <- names_key$Standard[names_key$Standard %in% colnames(df_steli)]
setcolorder(df_steli,
            cnames_steli)

cnames_atlas <- names_key$Standard[names_key$Standard %in% colnames(df_atlas)]
setcolorder(df_atlas,
            cnames_atlas)

# Write files -------------------------------------------------------------
write.table(df_steli,
            file = file.path(write_folder,
                             "steli.csv"),
            row.names = FALSE,
            qmethod = "double",
            sep = ",")
saveRDS(df_steli,
        file = file.path(write_folder,
                         "steli.rds"))

write.table(df_atlas,
            file = file.path(write_folder,
                             "atlas.csv"),
            row.names = FALSE,
            qmethod = "double",
            sep = ",")
saveRDS(df_atlas,
        file = file.path(write_folder,
                         "atlas.rds"))
