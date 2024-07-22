

#' Calculates conversion factors for a time series (year) simulation
#'
#' @param emptyscene_dir_file Path to the file of the app output for direct light
#' @param emptyscene_diff_file Path to the file of the app output for diffuse light
#' @param datetime Single or multiple DateTime. Can be a character in UTC (YYYY-MM-DD HH:mm:ss) or a POSIXct
#' @param globrad Incoming global radiation in W m-2
#' @param lat Latitude of the field in °
#' @param lon Longitude of the field in °
#' @param start_year starting year of the 3-D time series simulation
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
                         datetime, globrad, lat, lon, start_year){

  if(length(datetime) != length(globrad)) stop("datetime and globrad arguments have unequal length")
  origin <- as_date(paste0(start_year, "-12-21"))

  DiffuseReference <- data.table::fread(file = emptyscene_diff_file, sep = ",") %>%
    dplyr::rowwise(day) %>%
    dplyr::summarise(mean = rowMeans(across(starts_with("S")))) %>% .$mean %>% mean()

  diffdir_in <- diffdir_fun(datetime = datetime, globrad = globrad, lat = lat, lon = lon)
  diff_fun <- approxfun(x=diffdir_in$datetime, y=diffdir_in$rad_diff)
  dir_fun <- approxfun(x=diffdir_in$datetime, y=diffdir_in$rad_dir)

  out <- data.table::fread(file = emptyscene_dir_file, sep = ",") %>%
    dplyr::rowwise(day, `time (s)`) %>%
    dplyr::summarise(DirectReference = rowMeans(across(starts_with("S")))) %>%
    ungroup() %>%
    dplyr::mutate(solarnoon = getSunlightTimes(date = as_date(day, origin = origin),
                                               lat = lat, lon = lon, keep = "solarNoon")$solarNoon,
                  diffnoon = as_datetime(day*(24*3600) + 12*3600, origin = origin) - solarnoon) %>%
    dplyr::mutate(datetime = as_datetime(day*(24*3600) + `time (s)`, origin = origin) - diffnoon) %>%
    dplyr::mutate(diff = diff_fun(datetime), dir = dir_fun(datetime)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(ConvFactorDiffuse = diff/DiffuseReference,
                  ConvFactorDirect = dplyr::case_when(.default = dir/DirectReference,
                                                      (dir == 0 | DirectReference == 0 ) ~ 0 )) %>%
    dplyr::select(datetime, ConvFactorDiffuse, ConvFactorDirect)

  return(out)

}


