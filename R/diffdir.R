

#' Calculate the diffuse and direct components of solar radiation and solar position
#'
#' @param globrad Incoming global radiation in W m-2. Its length should match length of `datetime`.
#' @param lat Latitude of the field in °
#' @param lon Longitude of the field in °
#' @param datetime Single or multiple DateTime. Can be a character in UTC (YYYY-MM-DD HH:mm:ss) or a POSIXct
#'
#' @import magrittr
#' @import tibble
#' @import suncalc
#' @import dplyr
#' @import lubridate
#' @return
#' A tibble with:
#' - datetime: DateTime object
#' - globrad: global radiation in W m-2
#' - frac_diff: fraction of diffuse light in global radiation (0-1)
#' - rad_diff: diffuse component of global radiation in W m-2
#' - rad_dir: direct component of global radiation in W m-2
#' - theta: sun angle from the zenith in radians (0 at the zenith and PI/2 at the horizon)
#' - phi: sun azimuth in radians (direction along the horizon, measured from north to east), e.g. 0 is north and \eqn{\frac{\pi}{4}} northeast
#'
#' @export
#'
#' @examples
#' out <- calc_light_attr(globrad = 600, lat = 50.95, lon = 3.8, datetime = "2022-06-21 12:00:00")
calc_light_attr <- function(datetime, globrad, lat, lon){

  if(length(datetime) != length(globrad)) stop("datetime and globrad arguments have unequal length")

  out <- tibble::tibble(datetime = as_datetime(datetime, tz = "UTC"), globrad = globrad) %>%
    dplyr::mutate(theta = pi/2 - (suncalc::getSunlightPosition(date = datetime, lat = lat, lon = lon) %>% .$altitude),
           phi = (suncalc::getSunlightPosition(date = datetime, lat = lat, lon = lon) %>% .$azimuth) + pi,
           doy = datetime %>% yday(),
           Io = 1367*(1+0.033*cos(2*pi*(doy+10)/365)),
           So = Io * cos(theta),
           RsRso = globrad / So,
           R = 0.847 - 1.61 * cos(theta) + 1.04 * cos(theta) * cos(theta),
           K = (1.47 - R) / 1.66) %>%
    dplyr::mutate(frac_diff = dplyr::case_when(RsRso <= 0.22 ~ 1.0,
                                              RsRso <= 0.35 ~ 1 - 6.4 * (RsRso - 0.22)^2,
                                              RsRso <= K ~ 1.47 - 1.66 * RsRso,
                                              .default = R)) %>%
    dplyr::mutate(rad_diff = frac_diff*globrad, rad_dir = (1-frac_diff)*globrad) %>%
    dplyr::select(datetime, globrad, frac_diff, rad_diff, rad_dir, theta, phi)
  return(out)

}

#'

#' @references
#' {title = {Separating the diffuse and direct component of global radiation and its implications for modeling canopy photosynthesis {Part} {II}. {Calculation} of canopy photosynthesis},
#' volume = {38},
#' issn = {0168-1923},url = {https://www.sciencedirect.com/science/article/pii/0168192386900614},
#' doi = {10.1016/0168-1923(86)90061-4},language = {en},
#' number = {1},
#' journal = {Agricultural and Forest Meteorology},
#' author = {Spitters, C. J. T.},
#' month = oct,year = {1986},
#' pages = {231--242},
#' }
