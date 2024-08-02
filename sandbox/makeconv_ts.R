

#' Calculates conversion factors for a time series (year) simulation
#'
#' @param emptyscene_dir_file Path to the file of the app output for direct light
#' @param emptyscene_diff_file Path to the file of the app output for diffuse light
#' @param datetime Single or multiple DateTime. Can be a character in UTC (YYYY-MM-DD HH:mm:ss) or a POSIXct
#' @param globrad Incoming global radiation in W m-2
#' @param lat Latitude of the field in °
#' @param lon Longitude of the field in °
#'
#' @import readr
#' @import dplyr
#' @import magrittr
#' @import tidyr

#' @return a tibble with the variables datetime, ConvFactorDiffuse, ConvFactorDirect
#'
#' @details
#' The simulation output should be generated using the application available at [https://agroforestry.ugent.be]
#' for a full year starting at "`start_year`-12-21". The output is restricted to the time period for which `globrad` data are provided.
#'
#' @export
#'
#'
make_conv_factors_ts <- function(emptyscene_dir_file, emptyscene_diff_file,
                         datetime, globrad, lat, lon){

  #0. do an initial check for input data
  if(length(datetime) != length(globrad)) stop("datetime and globrad arguments have unequal length")

  #1. create solarnoons for the lat and lon for an entire year
  origin = as_date("2019-12-31")
  solarnoons = tibble(doy = 1:366,
                      solarnoon = getSunlightTimes(date = as_date(1:366, origin = origin),
                                                   lat = lat, lon = lon, keep = "solarNoon")$solarNoon) %>%
    dplyr::mutate(diffnoon = as_datetime((doy*24*3600)+12*3600, origin = origin) - solarnoon)

  #2. divide the radiation in diff and dir component and make it a function for interpolations
  diffdir_in <- diffdir_fun(datetime = datetime, globrad = globrad, lat = lat, lon = lon)
  diff_fun <- approxfun(x=diffdir_in$datetime, y=diffdir_in$rad_diff)
  dir_fun <- approxfun(x=diffdir_in$datetime, y=diffdir_in$rad_dir)

  #3. read the empty diff file and calculate the ref value
  DiffuseReference <- data.table::fread(file = emptyscene_diff_file, sep = ",") %>%
    pivot_longer(cols = starts_with("S")) %>% .$value %>% mean

  #4. read the empty dir file and find the times per day
  b <- data.table::fread(file = emptyscene_dir_file, sep = ",")
  times <- b %>% filter(day == 0) %>% .$`time (s)`

  #5. combine all data and return the conversion factors
  out <- tibble::tibble(doy = datetime %>% yday, year = datetime %>% year) %>% unique %>%
    dplyr::left_join(solarnoons, by = join_by(doy)) %>%
    dplyr::group_by(year, doy, diffnoon) %>%
    dplyr::reframe(`time (s)` = times) %>%
    dplyr::mutate(datetime = ymd(paste0(year, "-01-01")) + days(doy-1) + seconds(`time (s)`) - diffnoon) %>%
    dplyr::mutate(day = if_else(doy > 355, doy-356, doy + 10)) %>% select(datetime, day, `time (s)`) %>%
    dplyr::left_join(b, by = join_by(day, `time (s)`)) %>%
    dplyr::select(datetime, starts_with("S")) %>%
    tidyr::pivot_longer(cols = starts_with("S")) %>%
    dplyr::group_by(datetime) %>%
    dplyr::reframe(DirectReference = mean(value)) %>%
    dplyr::mutate(diff = diff_fun(datetime), dir = dir_fun(datetime)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(ConvFactorDiffuse = diff/DiffuseReference,
                  ConvFactorDirect = dplyr::case_when(.default = dir/DirectReference,
                                                      (dir == 0 | DirectReference == 0 ) ~ 0 )) %>%
    dplyr::select(datetime, ConvFactorDiffuse, ConvFactorDirect)

  return(out)
}


