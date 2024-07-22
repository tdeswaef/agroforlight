


#' Calculates conversion factors for a single simulation
#'
#' @param emptyscene_file Path to the file of the app output for an empty scene
#' @param datetime Single DateTime. Can be a character in UTC (YYYY-MM-DD HH:mm:ss) or a POSIXct
#' @param globrad Incoming global radiation in W m-2
#' @param lat Latitude of the field in °
#' @param lon Longitude of the field in °
#'
#' @return a tibble with two rows for following variables:
#' - `light`: "sunlight" and "diffuse light"
#' - `refvalue`: reference value used for light conversion
#' - `app_day`: Day used for the [https://agroforestry.ugent.be](https://agroforestry.ugent.be) app (0 = December 21)
#' - `app_hour`: Time of day used for the [https://agroforestry.ugent.be] app (12 = solar noon)
#' - `app_pheno`: Leaf growth stage for the [https://agroforestry.ugent.be] app (0 = no leaves, 1 = all leaves fully developed)
#' - `radiation`: direct and diffuse radiation fractions( W m-2)
#' - `convfactor`: conversion factor for direct light (sunlight) and diffuse light
#' @export
#'
#' @examples
#' make_conv_factors_1(emptyscene_file = "moment_180_12_100_ref.csv", lat = 50.8, lon = 3.8, globrad = 500, datetime = "2024-03-21 12:00:00")
make_conv_factors_1 <- function(emptyscene_file,
                                 datetime, globrad, lat, lon){

  if(length(datetime) != length(globrad)) stop("datetime and globrad arguments have unequal length")

  diffdir_in <- diffdir_fun(datetime = datetime, globrad = globrad, lat = lat, lon = lon) %>%
    mutate(sunlight = rad_dir, `diffuse light` = rad_diff) %>%
    select(sunlight, `diffuse light`) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "lighttype", values_to = "radiation")
  # diff_fun <- approxfun(x=diffdir_in$datetime, y=diffdir_in$rad_diff)
  # dir_fun <- approxfun(x=diffdir_in$datetime, y=diffdir_in$rad_dir)

  out <- data.table::fread(file = emptyscene_file, sep = ",") %>%
    dplyr::mutate(refvalue = rowMeans(across(starts_with("S")))) %>%
    dplyr::select(1, refvalue)
  out <- out %>% dplyr::mutate(app_day = str_extract_all(colnames(out)[1], "\\d+\\.?\\d*")[[1]][1] %>% as.numeric(),
                        app_hour = str_extract_all(colnames(out)[1], "\\d+\\.?\\d*")[[1]][2] %>% as.numeric(),
                        app_pheno = str_extract_all(colnames(out)[1], "\\d+\\.?\\d*")[[1]][3] %>% as.numeric()) %>%
    dplyr::mutate(lighttype = out[[1]]) %>%
    dplyr::select(lighttype, refvalue, app_day, app_hour, app_pheno) %>%
    dplyr::left_join(diffdir_in, join_by(lighttype)) %>%
    dplyr::mutate(convfactor = if_else(refvalue == 0, 0, radiation/refvalue)) %>%
    dplyr::mutate(datetime = datetime)

  est_app_day <- timetosolar(datetime)$day_for_app
  est_app_hour <- timetosolar(datetime)$time_for_app %>% round(digits = 2)
  if(!identical(est_app_day, out$app_day[1])) stop("the date of the app output and the datetime input are not identical")
  if(!all.equal(est_app_time, out$app_time[1])) stop("the time of the app output and the datetime input are not identical")

  return(out)

}

# make_conv_factors_1(emptyscene_file = "moment_180_12_100_ref.csv",
# lat = 50,
# lon = 3.8,
# globrad = 500,
# datetime = "2024-03-21")
