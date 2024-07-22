

#' Write a phenology file for input to the 3-D simulation application
#'
#' @param filename Path to the filename where the phenology file will be written
#' @param doy_start_flush Day of the year of onset of leaf appearance
#' @param doy_stop_flush Day of the year when all leaves have fully developed
#' @param doy_start_fall Day of the year when leaves start to fall
#' @param doy_stop_fall Day of the year when all leaves have fallen
#' @param shape shape of the phenology development. See *details*
#'
#' @return
#' writes a csv file
#'
#' @details
#' The day of the year inputs are transformed into the proper day for input in the application available at [https://agroforestry.ugent.be],
#' where the 'solar year' starts on December 21. Therefore, in the output, the onset of leaf development will appear 11 days later.
#'
#' For now only a linear shape is available.
#'
#' @export
#'
#' @examples
#' make_phenology_file("pheno_01.csv", 120, 134, 280, 310)
make_phenology_file <- function(filename, doy_start_flush, doy_stop_flush,
                                doy_start_fall, doy_stop_fall, shape = "linear"){
  if(shape == "linear"){
    doy <- 1:366
    y0 <- rep(0, doy_start_flush+11)
    y1 <- 1/(doy_stop_flush-doy_start_flush)*((doy-11)-doy_start_flush)[(doy_start_flush+12):(doy_stop_flush + 11)]
    y2 <- rep(1, doy_start_fall-doy_stop_flush)
    y3 <- (-1/(doy_stop_fall-doy_start_fall)*((doy-11)-doy_start_fall) + 1)[(doy_start_fall+12):(doy_stop_fall + 11)]
    y4 <- rep(0, 366-(doy_stop_fall + 11))
    y <- tibble(y = c(y0, y1, y2, y3, y4))
  } else {
    stop("for now only a linear shape is possible")
  }
  write_csv(x = y, file = filename, col_names = F)
}
