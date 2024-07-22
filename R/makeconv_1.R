


#' Calculates conversion factors for a single simulation
#'
#' @param emptyscene_file Path to the file of the app output for an empty scene
#' @param datetime Single or multiple DateTime. Can be a character in UTC (YYYY-MM-DD HH:mm:ss) or a POSIXct
#' @param globrad Incoming global radiation in W m-2
#' @param lat Latitude of the field in °
#' @param lon Longitude of the field in °
#'
#' @return a tibble with two rows for following variables:
#' - `light`: "sunlight" and "diffuse light"
#' - `ConvFactor`: conversion factor for direct light (sunlight) and diffuse light
#' - `day`: day in th
#' - `hour`
#' - `pheno`
#' @export
#'
#' @examples
make_conv_factors_1 <- function(emptyscene_file,
                                 datetime, globrad, lat, lon){

  if(length(datetime) != length(globrad)) stop("datetime and globrad arguments have unequal length")

  diffdir_in <- diffdir_fun(datetime = datetime, globrad = globrad, lat = lat, lon = lon)
  diff_fun <- approxfun(x=diffdir_in$datetime, y=diffdir_in$rad_diff)
  dir_fun <- approxfun(x=diffdir_in$datetime, y=diffdir_in$rad_dir)

  out <- data.table::fread(file = emptyscene_file, sep = ",") %>%
    dplyr::mutate(ConvFactor = rowMeans(across(starts_with("S")))) %>%
    dplyr::select(1, ConvFactor)
  out <- out %>% mutate(day = str_extract_all(colnames(out)[1], "\\d+\\.?\\d*")[[1]][1] %>% as.numeric(),
                        hour = str_extract_all(colnames(out)[1], "\\d+\\.?\\d*")[[1]][2] %>% as.numeric(),
                        pheno = str_extract_all(colnames(out)[1], "\\d+\\.?\\d*")[[1]][3] %>% as.numeric()) %>%
    mutate(light = out[[1]]) %>%
    select(light, ConvFactor, day, hour, pheno)
  # dplyr::mutate(ConvFactorDiffuse = diff/DiffuseReference,

  TODO
  ConvFactorDirect = dplyr::case_when(.default = dir/DirectReference,
                                      (dir == 0 | DirectReference == 0 ) ~ 0 )) %>%
  dplyr::select(datetime, ConvFactorDiffuse, ConvFactorDirect)

  return(out)

}

emptyscene_file <- "moment_180_12_100_ref.csv"
