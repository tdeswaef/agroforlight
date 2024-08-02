#' Date and time conversion for input in agroforestry app
#'
#' convert the time in UTC to solar time where 12:00 is solar noon and
#' convert the date to solar day where 0 = December 21
#'
#' @param solar_day Day used as input in the app (integer between 0-365), where 0 is 21 December
#' @param solar_time Hour used as input in the app (0-23.99), where 12 corresponds to solar noon
#' @param year Year for which the output is needed
#' @param lat Latitude of the field in °
#' @param lon Longitude of the field in °
#'
#' @return `datetime`: in UTC
#'
#' @export
#'
#' @examples
#' convert_solar_utc(solar_day = 180, solar_time = 12, lat = 50.5, lon = 3.8)
convert_solar_utc <- function(solar_day, solar_time, year = 2020, lat, lon){
  origin <- origin <- lubridate::as_date(paste0(year(datetime)-1, "-12-31"))

  doy <- ifelse(solar_day < 11, solar_day + 366-10, solar_day - 10)
  date <- ymd(paste0(year, "-01-01")) + days(doy-1)
  solarnoon <- suncalc::getSunlightTimes(date = date,
                                         lat = lat, lon = lon, keep = "solarNoon")$solarNoon
  diffnoon <- date + hours(12) - solarnoon
  solar_seconds = solar_time*3600 %>% as.integer()
  datetime =  date + seconds(solar_seconds) - diffnoon
  return(datetime)

}
