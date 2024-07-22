

convert_data_1 <- function(conv_factors, treescene_file){


  #1. check if date time of conv_factors corresponds to tree scene

  out <- out %>% mutate(app_day = str_extract_all(colnames(out)[1], "\\d+\\.?\\d*")[[1]][1] %>% as.numeric(),
                        app_hour = str_extract_all(colnames(out)[1], "\\d+\\.?\\d*")[[1]][2] %>% as.numeric(),
                        app_pheno = str_extract_all(colnames(out)[1], "\\d+\\.?\\d*")[[1]][3] %>% as.numeric()) %>%
    mutate(lighttype = out[[1]])

  if(!identical(conv_factors$app_day, out$app_day)) stop("dates for conversion factors and tree scene do not match")
  if(!identical(conv_factors$app_time, out$app_time)) stop("times for conversion factors and tree scene do not match")

  #2. calculate conversion for each position in the scene

  out <- out %>%
    select(lighttype, app_day, app_hour, app_pheno, starts_with("S", ignore.case = F)) %>%
    tidyr::pivot_longer( cols = starts_with("S", ignore.case = F), names_to = c("pos"), values_to = "value") %>%
    tidyr::separate_wider_delim(pos, names = c("discard", "pos"), delim = " ") %>%
    tidyr::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
    dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y)) %>%
    dplyr::select(-discard) %>%
    dplyr::left_join(conv_factors %>% select(datetime, lighttype, convfactor), by = join_by(lighttype)) %>%
    dplyr::mutate(radiation = value*convfactor) %>%
    #dplyr::mutate(datetime = solartotime(app_day, app_time)) %>%
    dplyr::select(datetime, lighttype, pos_x, pos_y, radiation)

  return(out)
}

# treescene_file <- "moment_180_12_100_tree.csv"
