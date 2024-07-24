

#' Convert the 3D simulation time series data into diffuse, direct and total light intensity
#'
#' @param conv_factors a tibble as produced by the `make_conv_factors_ts` function
#' @param treescene_dir_file path to the file of the app output for direct light
#' @param treescene_diff_file path to the file of the app output for diffuse light
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
convert_data_ts <- function(conv_factors, treescene_dir_file, treescene_diff_file, lat, lon){

  #1. create solarnoons for the lat and lon for an entire year
  origin = lubridate::as_date("2019-12-31")
  solarnoons = tibble::tibble(doy = 1:366,
                      solarnoon = suncalc::getSunlightTimes(date = as_date(1:366, origin = origin),
                                                   lat = lat, lon = lon, keep = "solarNoon")$solarNoon) %>%
    dplyr::mutate(diffnoon = as_datetime((doy*24*3600)+12*3600, origin = origin) - solarnoon)

  #2. read the diff file and pivot_longer
  Diffuse_tree <- data.table::fread(file = treescene_diff_file, sep = ",") %>%
    tidyr::pivot_longer(cols = starts_with("S", ignore.case = F), names_to = c("pos"), values_to = "diff_value") %>%
    tidyr::separate_wider_delim(pos, names = c("discard", "pos"), delim = " ") %>%
    tidyr::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
    dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y)) %>%
    dplyr::select(-discard)

  #3.  read the dir file and find the times per day
  b <- data.table::fread(file = treescene_dir_file, sep = ",")
  times <- b %>% filter(day == 0) %>% .$`time (s)`

  step1 <- tibble::tibble(doy = conv_factors$datetime %>% yday, year = conv_factors$datetime %>% year) %>% unique %>%
    dplyr::left_join(solarnoons, by = join_by(doy)) %>%
    dplyr::group_by(year, doy, diffnoon) %>%
    dplyr::reframe(`time (s)` = times) %>%
    dplyr::mutate(datetime = ymd(paste0(year, "-01-01")) + days(doy-1) + seconds(`time (s)`) - diffnoon) %>%
    dplyr::mutate(day = if_else(doy > 355, doy-356, doy + 10)) %>% select(datetime, day, `time (s)`) %>%
    dplyr::left_join(b, by = join_by(day, `time (s)`)) %>%
    dplyr::select(datetime, day, starts_with("S"))

  out <- step1 %>%
    tidyr::pivot_longer(cols = starts_with("S", ignore.case = F), names_to = c("pos"), values_to = "dir_value") %>%
    tidyr::separate_wider_delim(pos, names = c("discard", "pos"), delim = " ") %>%
    tidyr::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
    dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y)) %>%
    dplyr::left_join(conv_factors, by = join_by(datetime)) %>%
    dplyr::mutate(direct_rad = dir_value*ConvFactorDirect) %>%
    dplyr::left_join(Diffuse_tree, join_by(day, pos_x, pos_y)) %>%
    dplyr::mutate(diffuse_rad = diff_value*ConvFactorDiffuse) %>%
    dplyr::mutate(total_rad = diffuse_rad + direct_rad) %>%
    tidyr::drop_na() %>%
    dplyr::select(datetime, pos_x, pos_y, direct_rad, diffuse_rad, total_rad)

  return(out)
}


