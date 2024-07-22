

#' Date and time conversion for input in agroforestry app
#'
#' convert the time in UTC to solar time where 12:00 is solar noon and
#' convert the date to solar day where 0 = December 21
#'
#' @param datetime Single DateTime. Can be a character in UTC (YYYY-MM-DD HH:mm:ss) or a POSIXct
#' @param lat Latitude of the field in °
#' @param lon Longitude of the field in °
#'
#' @return a list with:
#' - `day_for_app`: the day number to input in the [https://agroforestry.ugent.be] app
#' - `time_for_app`: the Time of day to input in the [https://agroforestry.ugent.be] app
#' @export
#'
#' @examples
#' timetosolar("2024-06-21 12:00:00", lat = 50.5, lon = 3.8)
timetosolar <- function(datetime, lat, lon){
  origin <- origin <- lubridate::as_date(paste0(year(datetime)-1, "-12-31"))
  day <- yday(datetime)
  day_for_app <- ifelse(day >=356, day - 366+10, day + 10)

  solarnoon <- suncalc::getSunlightTimes(date = as_date(day, origin = origin),
                            lat = lat, lon = lon, keep = "solarNoon")$solarNoon
  diffnoon <- lubridate::as_datetime(day*(24*3600) + 12*3600, origin = origin) - solarnoon
  time_for_app <- (lubridate::as_datetime(datetime) + diffnoon)
  time_for_app <- lubridate::hour(time_for_app) + lubridate::minute(time_for_app)/60 %>% round(digits = 2)


  return(list(day_for_app = day_for_app, time_for_app = time_for_app))

}


