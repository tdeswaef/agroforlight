

#'  Convert the 3D simulation moment data into diffuse and direct light intensity
#'
#' @param conv_factors a tibble as produced by the `make_conv_factors_1` function
#' @param treescene_file path to the file of the app output for single moment simulation of a scene with trees
#'
#' @import stringr
#' @return tibble with variables datetime, pos_x, pos_y, direct_rad, diffuse_rad, total_rad
#' @export
#'
convert_data_1 <- function(conv_factors, treescene_file){


  #1. check if date time of conv_factors corresponds to tree scene

  out <- data.table::fread(file = treescene_file, sep = ",") %>%
    dplyr::mutate(app_day = stringr::str_extract_all(colnames(.)[1], "\\d+\\.?\\d*")[[1]][1] %>% as.numeric(),
                  app_hour = stringr::str_extract_all(colnames(.)[1], "\\d+\\.?\\d*")[[1]][2] %>% as.numeric(),
                  app_pheno = stringr::str_extract_all(colnames(.)[1], "\\d+\\.?\\d*")[[1]][3] %>% as.numeric()) %>%
    dplyr::mutate(lighttype = .[[1]])

  if(!identical(conv_factors$app_day, out$app_day)) stop("dates for conversion factors and tree scene do not match")
  if(!identical(conv_factors$app_hour, out$app_hour)) stop("times for conversion factors and tree scene do not match")

  #2. calculate conversion for each position in the scene

  out <- out %>%
    dplyr::select(lighttype, app_day, app_hour, app_pheno, starts_with("S", ignore.case = F)) %>%
    tidyr::pivot_longer( cols = starts_with("S", ignore.case = F), names_to = c("pos"), values_to = "value") %>%
    tidyr::separate_wider_delim(pos, names = c("discard", "pos"), delim = " ") %>%
    tidyr::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
    dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y)) %>%
    dplyr::select(-discard) %>%
    dplyr::left_join(conv_factors %>% select(datetime, lighttype, convfactor), by = join_by(lighttype)) %>%
    dplyr::mutate(radiation = value*convfactor) %>%
    #dplyr::mutate(datetime = solartotime(app_day, app_time)) %>%
    dplyr::select(datetime, lighttype, pos_x, pos_y, radiation) %>%
    tidyr::pivot_wider(names_from = lighttype, values_from = radiation) %>%
    dplyr::mutate(direct_rad = sunlight, diffuse_rad = `diffuse light`) %>%
    dplyr::mutate(total_rad = direct_rad + diffuse_rad) %>%
    dplyr::select(datetime, pos_x, pos_y, direct_rad, diffuse_rad, total_rad)

  return(out)
}

# treescene_file <- "moment_180_12_100_tree.csv"
