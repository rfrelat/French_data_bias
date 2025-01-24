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
