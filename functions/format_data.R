# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2025-01-24
#
# Script Description: helper functions to format and clean data


# Standardize -------------------------------------------------------------


#' Rename columns
#'
#' @param key a dataframe containing the correspondence between the new
#' (in column `Standard`) and old (in column `nam`) column names.
#' @param dtable A datatable for which columns must be renamed
#' @param nam Name of the column of `key` in which to search old names.
#'
#' @return No value, sets the column names of `dtable`
#' @export
rename_cols <- function(key, dtable, nam) {
  cols <- c("Standard", nam)
  newnames_df <- key[!is.na(key[[nam]]), ..cols]

  setnames(dtable,
           old = newnames_df[[nam]],
           new = newnames_df$Standard)
}


#' Make grid
#'
#' Overlay a grid over extent.
#' @param extent Spatial object to lay a grid over
#' @param cellsize Scale of the grid in meters (see `sf::st_make_grid`)
#' @param what Object to make the grid (see `sf::st_make_grid`)
#' @param square Is it a square grid? (see `sf::st_make_grid`)
#' @param flat_topped If hexagon, is the a flat surface facing top?
#' (see `sf::st_make_grid`)
#'
#' @returns A spatial dataframe object with the grid and a cell ID.
#' @export
make_grid <- function(extent, cellsize,
                      what = "polygons",
                      square = TRUE,
                      flat_topped = TRUE) {
  # Buffer zone
  buffer <- st_bbox(st_buffer(extent, cellsize))

  # Grid over all zone
  grid <- st_make_grid(buffer,
                       n = 1,
                       what = "polygons")
  grid <- st_make_grid(grid,
                       cellsize = cellsize,
                       what = what,
                       square = square,
                       flat_topped = flat_topped)

  # Restrict grid to countries
  grid <- grid[unique(unlist(st_intersects(extent, grid)))]

  # Add ID
  n <- nchar(length(grid))

  grid <- st_sf(geom = grid) |>
    mutate(id = paste0("g", sprintf(paste0("%0", n, "d"), 1:length(grid))))

  return(grid)
}

#' Add grid ID to spatial df
#'
#' @param obs Observations stored in spatial df
#' @param grid Grid spatial df. Must have a column named `id` that identifies
#' each unique grid cell.
#' @param idcol Name of the id column
#'
#' @returns The observation df with an additionnal column `id`.
#' @export
add_grid_id <- function(obs, grid, idcol = "id") {
  # Intersect data with grid
  datgrid <- st_intersects(obs,
                           grid)

  # Get grid id for each
  datgrid <- lapply(seq_along(datgrid),
                    function(i) {
                      index <- datgrid[[i]]
                      if (length(index) != 0) {
                        res <- grid[index, ]$id
                      } else {
                        res <- NA
                      }
                      return(res)
                    })

  # Add grid ID to occurrences
  # This assumes each observation is fount in no more than one grid
  datgrid <- unlist(datgrid)

  res <- obs
  res[[idcol]] <- datgrid

  return(res)
}

#' Get number of unique elements
#'
#' @param x element for which to get number of unique elements (vector, list,
#' data.frame...)
#'
#' @returns The number of unique elements in x.
f <- function(x){length(unique(x))}
