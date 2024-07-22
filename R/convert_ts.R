

#' Convert the 3D simulation time series data into diffuse, direct and total light intensity
#'
#' @param conv_factors a tibble as produced by the `make_conv_factors_ts` function
#' @param treescene_dir_file path to the file of the app output for direct light
#' @param treescene_diff_file path to the file of the app output for diffuse light
#' @param start_year the starting year of the agroforestry simulation.
#' @param lat Latitude of the field in °
#' @param lon Longitude of the field in °
#'
#' @import lubridate
#' @import dplyr
#' @import data.table
#' @import suncalc
#' @import tidyr

#' @details
#' The simulation output should be generated using the application available at [https://agroforestry.ugent.be]
#' for a full year. The output is restricted to the time period for which globrad data were provided for the `make_conv_factors_ts` function.

#' @return tibble with variables datetime, pos_x, pos_y, direct_rad, diffuse_rad, total_rad

#' @export
#'
#'
convert_data_ts <- function(conv_factors, treescene_dir_file, treescene_diff_file, start_year, lat, lon){

  origin <- lubridate::as_date(paste0(start_year, "-12-21"))

  Diffuse_tree <- data.table::fread(file = treescene_diff_file, sep = ",") %>%
    tidyr::pivot_longer(cols = starts_with("S", ignore.case = F), names_to = c("pos"), values_to = "diff_value") %>%
    tidyr::separate_wider_delim(pos, names = c("discard", "pos"), delim = " ") %>%
    tidyr::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
    dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y)) %>%
    dplyr::select(-discard)

  out <- data.table::fread(file = treescene_dir_file, sep = ",") %>%
    dplyr::mutate(solarnoon = suncalc::getSunlightTimes(date = as_date(day, origin = origin),
                                               lat = lat, lon = lon, keep = "solarNoon")$solarNoon,
                  diffnoon = lubridate::as_datetime(day*(24*3600) + 12*3600, origin = origin) - solarnoon) %>%
    dplyr::mutate(datetime = lubridate::as_datetime(day*(24*3600) + `time (s)`, origin = origin) - diffnoon) %>%
    dplyr::inner_join(conv_factors, by = join_by(datetime)) %>%
    dplyr::select(day, datetime, ConvFactorDirect, ConvFactorDiffuse, starts_with("S", ignore.case = F)) %>%
    tidyr::pivot_longer(cols = starts_with("S", ignore.case = F), names_to = c("pos"), values_to = "dir_value") %>%
    tidyr::separate_wider_delim(pos, names = c("discard", "pos"), delim = " ") %>%
    tidyr::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
    dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y)) %>%
    dplyr::mutate(direct_rad = dir_value*ConvFactorDirect) %>%
    dplyr::left_join(Diffuse_tree, join_by(day, pos_x, pos_y)) %>%
    dplyr::mutate(diffuse_rad = diff_value*ConvFactorDiffuse) %>%
    dplyr::mutate(total_rad = diffuse_rad + direct_rad) %>%
    dplyr::select(datetime, pos_x, pos_y, direct_rad, diffuse_rad, total_rad)

  return(out)
}


