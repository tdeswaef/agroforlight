

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
#' @import tidytable

#' @details
#' The simulation output should be generated using the application available at [https://agroforestry.ugent.be]
#' for a full year. The output is restricted to the time period for which globrad data were provided for the `make_conv_factors_ts` function.

#' @return tibble with variables datetime, pos_x, pos_y, total_rad

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

  #2. read the diff and dir files with trees
  a <- data.table::fread(file = treescene_diff_file, sep = ",")
  b <- data.table::fread(file = treescene_dir_file, sep = ",")
  times <- b %>% dplyr::filter(day == 0) %>% .$`time (s)`

  #3. make a combination of datetime and day times from app
  step1 <- tibble::tibble(doy = conv_factors$datetime %>% yday, year = conv_factors$datetime %>% year) %>% unique %>%
    dplyr::left_join(solarnoons, by = join_by(doy)) %>%
    dplyr::group_by(year, doy, diffnoon) %>%
    dplyr::reframe(`time (s)` = times) %>%
    dplyr::mutate(datetime = ymd(paste0(year, "-01-01")) + days(doy-1) + seconds(`time (s)`) - diffnoon) %>%
    dplyr::mutate(day = if_else(doy > 355, doy-356, doy + 10)) %>% select(datetime, day, `time (s)`)

  #4. direct rad conversion
  step2 <- step1 %>%
    dplyr::left_join(conv_factors, by = join_by(datetime)) %>%
    dplyr::left_join(b, by = join_by(day, `time (s)`)) %>%
    dplyr::select(datetime, ConvFactorDirect, starts_with("S"))  %>%
    tidyr::drop_na()  %>%
    dplyr::mutate(
      across(starts_with("S"), ~ .x* ConvFactorDirect, .names = "{.col}"))

  #5. diffuse rad conversion
  step3 <- step1 %>%
    dplyr::left_join(conv_factors, by = join_by(datetime)) %>%
    dplyr::left_join(a, by = join_by(day)) %>%
    dplyr::select(datetime, ConvFactorDiffuse, starts_with("S"))  %>%
    tidyr::drop_na()  %>%
    dplyr::mutate(across(starts_with("S"), ~ .x* ConvFactorDiffuse, .names = "{.col}")) %>%
    rename_with(~ str_split_i(., pattern = " ", i = 2), starts_with("S"))

  #6. sum of diff and dir + pivot data
  step4 <- step3 %>%
    dplyr::mutate(across(starts_with("S"), ~ .x + step2[[cur_column()]])) %>%
    dplyr::select(!starts_with("Con")) %>%
    tidytable::pivot_longer(cols = -datetime, names_to = c("pos"), values_to = "total_rad") %>%
    tidytable::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
	dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y))

  return(step4)
}


