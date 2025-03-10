# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2025-03-10
#
# Script Description: prepare grid and discretize observations



# Libraries etc -----------------------------------------------------------
library(rnaturalearth)
library(sf)

library(dplyr)
library(ggplot2)
library(patchwork)

library(here)


# Path
fun_folder <- here("functions")
read_folder <- here("data/02_clean")
write_folder <- here("data/03_grid")

# Own functions
source(file.path(fun_folder, "format_data.R"))

grid_scale <- c(1000, 5000, 10000)


# Prepare France ----------------------------------------------------------
# Get France
fr_reg <- ne_states(country = "France", returnclass = "sf")

ggplot() +
  geom_sf(data = fr_reg)

# Metropolitan France
fr_metro <- fr_reg |> filter(type == "Metropolitan département")
ggplot() +
  geom_sf(data = fr_metro)

# Unite polygons
fr <- st_union(fr_metro)
ggplot() +
  geom_sf(data = fr)

# Crop France and change CRS
fr_buffer <- st_buffer(fr, dist = max(grid_scale))
bbox <- st_bbox(fr_buffer)

# Get France box
fr_box <- st_make_grid(bbox,
                       n = 1, what = "polygons")
ggplot() +
  geom_sf(data = fr) +
  geom_sf(data = fr_box, color = "darkred", fill = "transparent")

# Change CRS
fr_3035 <- st_transform(fr,
                        crs = 3035)
st_crs(fr)
st_crs(fr_3035)

# Make grids --------------------------------------------------------------
# Get number of grids to build
ngrid <- length(grid_scale)

# Initialize grids list
grids <- vector(mode = "list", length = ngrid)
scale_names <- paste("scale", grid_scale, sep = "_")
names(grids) <- scale_names

for (i in 1:ngrid) {
  gr <- grid_scale[i]
  grid <- make_grid(extent = fr_3035,
                    cellsize = gr)
  grids[[i]] <- grid

  # Save RDS
  scale_name <- names(grids)[i]
  saveRDS(grid,
          file.path(write_folder, paste0("grid_", scale_name, ".rds")))
}


# Overview grids

# We choose a subset of France to better visualize grids (also quicker)
# Get Nord department
nord <- fr_metro |>
  filter(name == "Nord")
# Change CRS
nord_3035 <- st_transform(nord,
                          crs = 3035)

glist <- vector(mode = "list", length = 3)
names(glist) <- names(grids)
for (i in 1:ngrid) {
  # Get grid
  gr <- grids[[i]]

  # Crop grid
  nord_grid <- st_crop(gr, nord_3035)

  # Plot
  g <- ggplot() +
    geom_sf(data = nord_3035) +
    geom_sf(data = nord_grid, fill = "transparent") +
    theme_void()
  glist[[i]] <- g

}

wrap_plots(glist)


# Add grid id to data -----------------------------------------------------

## Read data -----
df_steli <- readRDS(file = file.path(read_folder, "steli.rds"))
df_atlas <- readRDS(file.path(read_folder, "atlas.rds"))

sf_steli <- st_as_sf(df_steli,
                     coords = c("decimalLongitude", "decimalLatitude"))
st_crs(sf_steli) <- 4326

sf_atlas <- st_as_sf(df_atlas,
                     coords = c("decimalLongitude", "decimalLatitude"))
st_crs(sf_atlas) <- 4326

# Transform CRS
steli_3035 <- st_transform(sf_steli, crs = 3035)
atlas_3035 <- st_transform(sf_atlas, crs = 3035)

## Add grids -----
steli_grid <- steli_3035
atlas_grid <- atlas_3035
for(i in 1:ngrid) {
  print(paste(i, "------"))
  grid_id <- strsplit(names(grids)[i], "_")[[1]][2]
  grid_id <- paste("id", grid_id, sep = "_")
  steli_grid <- add_grid_id(steli_grid,
                            grids[[i]], idcol = grid_id)
  atlas_grid <- add_grid_id(atlas_grid,
                            grids[[i]], idcol = grid_id)
}

# Check data
# STELI
nobs <- st_drop_geometry(steli_grid) |>
  group_by(id_10000) |>
  summarize(nobs = n())

nobs_grid <- grids$scale_10000 |>
  right_join(nobs, by = c("id" = "id_10000"))

ggplot() +
  geom_sf(data = fr_3035) +
  geom_sf(data = nobs_grid, aes(fill = nobs)) +
  scale_fill_viridis_c(na.value = "lightgrey",
                       trans = "log")

# Atlas
nobs <- st_drop_geometry(atlas_grid) |>
  group_by(id_10000) |>
  summarize(nobs = n())

nobs_grid <- grids$scale_10000 |>
  right_join(nobs, by = c("id" = "id_10000"))

ggplot() +
  geom_sf(data = fr_3035) +
  geom_sf(data = nobs_grid, aes(fill = nobs)) +
  scale_fill_viridis_c(na.value = "lightgrey",
                       trans = "log")


# Write data --------------------------------------------------------------
write.table(st_drop_geometry(steli_grid),
            file = file.path(write_folder,
                             "steli.csv"),
            row.names = FALSE,
            qmethod = "double",
            sep = ",")
saveRDS(steli_grid,
        file = file.path(write_folder,
                         "steli.rds"))

write.table(st_drop_geometry(atlas_grid),
            file = file.path(write_folder,
                             "atlas.csv"),
            row.names = FALSE,
            qmethod = "double",
            sep = ",")
saveRDS(atlas_grid,
        file = file.path(write_folder,
                         "atlas.rds"))
