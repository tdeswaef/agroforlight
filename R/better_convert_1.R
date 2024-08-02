

#'  Convert the 3D simulation moment data into diffuse and direct light intensity
#'
#' @param treescene_file path to the file of the app output for single moment simulation of a scene with trees
#' @param globrad Incoming global radiation in W m-2
#' @param lat Latitude of the field in °
#' @param lon Longitude of the field in °
#' @param sensor_size surface area of a sensor in the sensor field in m²
#' @param inclination Inclination setting (Geography) of the field simulation in degrees
#' @param rotation Rotation setting (Geography) of the field simulation in degrees
#'
#' @import stringr
#' @return tibble with variables datetime, pos_x, pos_y, direct_rad, diffuse_rad, total_rad
#' @export
#'
convert_afl_1 <- function(treescene_file, globrad, lat, lon, sensor_size = 1, inclination = 0, rotation = 0){

  #1. read the tree scen output file
  out <- data.table::fread(file = treescene_file, sep = ",") %>%
    dplyr::mutate(app_day = stringr::str_extract_all(colnames(.)[1], "\\d+\\.?\\d*")[[1]][1] %>% as.numeric(),
                  app_hour = stringr::str_extract_all(colnames(.)[1], "\\d+\\.?\\d*")[[1]][2] %>% as.numeric(),
                  app_pheno = stringr::str_extract_all(colnames(.)[1], "\\d+\\.?\\d*")[[1]][3] %>% as.numeric()) %>%
    dplyr::mutate(lighttype = .[[1]]) %>%
    dplyr::select(lighttype, app_day, app_hour, app_pheno, starts_with("S", ignore.case = F))

  #2. create conversion factors
  beta = inclination * pi/180
  gamma = rotation * pi/180

  datetime <- convert_solar_utc(solar_day = unique(out$app_day), solar_time = unique(out$app_hour), lat = lat, lon = lon)

  conv_factors <- calc_light_attr(datetime = datetime, globrad = globrad, lat = lat, lon = lon) %>%
    mutate(costheta = cos(theta)*cos(beta) + sin(theta)*sin(beta)*cos(phi - gamma)) %>%
    mutate(ConvFactor_Diff = rad_diff/(0.58*sensor_size), ConvFactor_Dir = rad_dir/(costheta*sensor_size)) %>%
    select(ConvFactor_Diff, ConvFactor_Dir)

  cf <- tibble(lighttype = out$lighttype, cf = c(conv_factors$ConvFactor_Dir, conv_factors$ConvFactor_Diff))

  #3. calculate conversion for each position in the scene

  out <- out %>%
    dplyr::select(-starts_with("app")) %>%
    dplyr::left_join(cf, by = join_by(lighttype)) %>%
    dplyr::mutate(dplyr::across(starts_with("S"), ~ .x* cf, .names = "{.col}")) %>%
    dplyr::rename_with(~ str_split_i(., pattern = " ", i = 2), starts_with("S")) %>%
    tidyr::pivot_longer( cols = -c(lighttype, cf), names_to = c("pos"), values_to = "radiation") %>%
    # tidyr::separate_wider_delim(pos, names = c("discard", "pos"), delim = " ") %>%
    tidyr::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
    dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y)) %>%
    dplyr::select(-cf) %>%
    tidyr::pivot_wider(names_from = lighttype, values_from = radiation) %>%
    dplyr::mutate(direct_rad = sunlight, diffuse_rad = `diffuse light`) %>%
    dplyr::mutate(total_rad = direct_rad + diffuse_rad, datetime = datetime) %>%
    dplyr::select(datetime, pos_x, pos_y, direct_rad, diffuse_rad, total_rad)

  return(out)
}


