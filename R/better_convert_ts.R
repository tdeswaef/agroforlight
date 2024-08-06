

#' Convert the 3D simulation time series data into diffuse, direct and total light intensity
#'
#' @param treescene_dir_file path to the file of the app output for direct light
#' @param treescene_diff_file path to the file of the app output for diffuse light
#' @param datetime Single or multiple DateTime. Can be a character in UTC (YYYY-MM-DD HH:mm:ss) or a POSIXct
#' @param globrad Incoming global radiation in W m-2
#' @param lat Latitude of the field in °
#' @param lon Longitude of the field in °
#' @param sensor_size surface area of a sensor in the sensor field in m²
#' @param inclination Inclination setting (Geography) of the field simulation in degrees
#' @param rotation Rotation setting (Geography) of the field simulation in degrees
#' @param out_components if the output should return direct and diffuse components of radiation (default = F, for speed)
#'
#' @import lubridate
#' @import dplyr
#' @import data.table
#' @import suncalc
#' @import tidyr
#' @import tidytable

#' @details
#' The simulation output should be generated using the application available at [https://agroforestry.ugent.be]
#' for a full year. The output is restricted to the time period for which globrad data were provided.
#' `sensor_size` is the result of (Sensor_Size_x x Sensor_Size_y) / (Sensors_Count_x x Sensors_Count_y)

#' @return tibble with variables datetime, pos_x, pos_y, total_rad

#' @export
#'
#'
convert_afl_ts <- function(treescene_dir_file, treescene_diff_file, datetime, globrad, lat, lon, sensor_size = 1, inclination = 0, rotation = 0, out_components = F){

  #1. create conversion factor functions
  beta = inclination * pi/180
  gamma = rotation * pi/180

  conv_factors <- calc_light_attr(datetime = datetime, globrad = globrad, lat = lat, lon = lon) %>%
    mutate(costheta = cos(theta)*cos(beta) + sin(theta)*sin(beta)*cos(phi - gamma)) %>%
    mutate(ConvFactor_Diff = rad_diff/(0.58*sensor_size), ConvFactor_Dir = rad_dir/(costheta*sensor_size)) %>%
    select(datetime, ConvFactor_Diff, ConvFactor_Dir)

  cf_diff_fun <- approxfun(conv_factors$datetime, conv_factors$ConvFactor_Diff)
  cf_dir_fun <- approxfun(conv_factors$datetime, conv_factors$ConvFactor_Dir)

  #2. create solarnoons for the lat and lon for an entire year for time aligning
  origin = lubridate::as_date("2019-12-31")
  solarnoons = tibble::tibble(doy = 1:366,
                              solarnoon = suncalc::getSunlightTimes(date = as_date(1:366, origin = origin),
                                                                    lat = lat, lon = lon, keep = "solarNoon")$solarNoon) %>%
    dplyr::mutate(diffnoon = as_datetime((doy*24*3600)+12*3600, origin = origin) - solarnoon)


  #3. read the diff and dir files with trees
  a <- data.table::fread(file = treescene_diff_file, sep = ",")
  b <- data.table::fread(file = treescene_dir_file, sep = ",")
  times <- b %>% dplyr::filter(day == 0) %>% .$`time (s)`

  #4. create a tibble with all datetime and day - times combinations
  step1 <- tibble::tibble(doy = datetime %>% yday, year = datetime %>% year) %>% unique %>%
    dplyr::left_join(solarnoons, by = join_by(doy)) %>%
    dplyr::group_by(year, doy, diffnoon) %>%
    dplyr::reframe(`time (s)` = times) %>%
    dplyr::mutate(datetime = ymd(paste0(year, "-01-01")) + days(doy-1) + seconds(`time (s)`) - diffnoon) %>%
    dplyr::mutate(day = if_else(doy > 355, doy-356, doy + 10)) %>% select(datetime, day, `time (s)`) %>%
    dplyr::mutate(cf_dir = cf_dir_fun(datetime), cf_dif = cf_diff_fun((datetime))) %>% drop_na()

  if(out_components){
    #5. calculate the direct radiation
    step2 <- step1 %>%
      dplyr::left_join(b, by = join_by(day, `time (s)`)) %>%
      dplyr::select(datetime, cf_dir, starts_with("S"))  %>%
      dplyr::mutate(across(starts_with("S"), ~ .x* cf_dir, .names = "{.col}")) %>%
      dplyr::select(!starts_with("cf_")) %>%
      dplyr::rename_with(~ str_split_i(., pattern = " ", i = 2), starts_with("S")) %>%
      tidytable::pivot_longer(cols = -datetime, names_to = c("pos"), values_to = "direct_rad")

    #6. Calculate the diffuse radiation
    step3 <- step1 %>%
      dplyr::left_join(a, by = join_by(day)) %>%
      dplyr::select(datetime, cf_dif, starts_with("S"))  %>%
      dplyr::mutate(dplyr::across(starts_with("S"), ~ .x* cf_dif, .names = "{.col}")) %>%
      dplyr::select(!starts_with("cf_")) %>%
      dplyr::rename_with(~ str_split_i(., pattern = " ", i = 2), starts_with("S")) %>%
      tidytable::pivot_longer(cols = -datetime, names_to = c("pos"), values_to = "diffuse_rad")

    #7. sum of diff and dir
    step4 <- step3 %>%
      dplyr::left_join(step2, by = join_by(datetime, pos)) %>%
      tidytable::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
      dplyr::mutate(total_rad = direct_rad + diffuse_rad) %>%
      dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y))
  } else {

    #5. calculate the direct radiation
    step2 <- step1 %>%
      dplyr::left_join(b, by = join_by(day, `time (s)`)) %>%
      dplyr::select(datetime, cf_dir, starts_with("S"))  %>%
      dplyr::mutate(across(starts_with("S"), ~ .x* cf_dir, .names = "{.col}"))

    #6. Calculate the diffuse radiation
    step3 <- step1 %>%
      dplyr::left_join(a, by = join_by(day)) %>%
      dplyr::select(datetime, cf_dif, starts_with("S"))  %>%
      dplyr::mutate(dplyr::across(starts_with("S"), ~ .x* cf_dif, .names = "{.col}"))

    #7. sum of diff and dir + pivot data
    step4 <- step3 %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("S"), ~ .x + step2[[dplyr::cur_column()]])) %>%
      dplyr::select(!starts_with("cf_")) %>%
      dplyr::rename_with(~ str_split_i(., pattern = " ", i = 2), starts_with("S")) %>%
      tidytable::pivot_longer(cols = -datetime, names_to = c("pos"), values_to = "total_rad") %>%
      tidytable::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
      dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y))
  }

  return(step4)
}


