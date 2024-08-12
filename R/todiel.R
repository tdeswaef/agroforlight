

#' Approximate higher time resolution radiation from daily data
#'
#' @param date the dates for which you want the conversion in "YYYY-MM-DD" format. Should have the same length as `glob_rad`
#' @param glob_rad the daily radiation sum in MJ d-1. Should have the same length as `doy`
#' @param lat the latitude where the data are from
#' @param lon the longitude the data are from
#' @param timeres_out the time resolution in `h` requested for the output
#'
#' @return a tibble with variables `datetime` and `radiation`
#' @export
#'
day_to_time_rad <- function(date, glob_rad, lat, lon, timeres_out){

  # if(length(year) == 1 | length(year) == length(doy)){
  #   year <- tibble(year = year, doy = doy)
  # } else {
  #   stop("length(year) should be 1 or equal to length(doy)")
  # }
  if(length(date) != length(glob_rad)){
    stop("length(date) should be equal to length(glob_rad)")
  } else {
      rad = tibble(date = date, glob_rad = glob_rad)
    }


  times <- seq(0, 24-timeres_out, timeres_out)
  out <- expand_grid(date, times) %>%
    dplyr::mutate(datetime = ymd(date) + lubridate::hours(times)) %>%
    dplyr::mutate(theta = (suncalc::getSunlightPosition(date = datetime, lat = lat, lon = lon) %>% .$altitude)) %>%
    dplyr::mutate(wf_1 = dplyr::case_when(theta < 0 ~ 0,
                                          .default = theta)) %>%
    dplyr::group_by(date) %>%
    dplyr::reframe(wf_2 = wf_1/sum(wf_1), datetime = datetime) %>%
    dplyr::left_join(rad, by = join_by(date)) %>%
    dplyr::mutate(radiation = glob_rad * wf_2*1e6/3600) %>%
    dplyr::select(datetime, radiation)
  return(out)
}







