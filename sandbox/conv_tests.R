
file_path = "C:/Users/tdeswaef/Repos/24-paper-light-af/01_data/"

Willem_data <- read_delim(file = paste0(file_path, "Data_Willem.csv"), delim = ";")

Willem_data <- Willem_data %>% mutate(UTC_1 = TIMESTAMP-3600, UTC_2= TIMESTAMP-7200) %>% mutate(datetime = UTC_2, radiation = Radiation)
example_data <- Willem_data %>% select(datetime, radiation)
diffdirs_1 <- diffdir_fun(datetime = Willem_data$UTC_1, globrad = Willem_data$Radiation, lat = lat, lon = lon) %>%
  mutate(costheta = cos(theta))
diffdirs_2 <- diffdir_fun(datetime = Willem_data$UTC_2, globrad = Willem_data$Radiation, lat = lat, lon = lon) %>%
  mutate(costheta = cos(theta))




treescene_dir_file = paste0(file_path, "1024_sunlight_alderold_00.csv")
treescene_diff_file = paste0(file_path, "1024_diffuse_light_alderold_00.csv")
lat = 50.9
lon =3.78
sensor_size = 1
inclination = 0
rotation = 0

datetime = Willem_data %>% arrange(UTC_2) %>% .$UTC_2
globrad = Willem_data %>% arrange(UTC_2) %>% .$Radiation


#1. create conversion factor functions
beta = inclination * pi/180
gamma = rotation * pi/180

conv_factors <- diffdir_fun(datetime = datetime, globrad = globrad, lat = lat, lon = lon) %>%
  mutate(costheta = cos(theta)*cos(beta) + sin(theta)*sin(beta)*cos(phi - gamma)) %>%
  mutate(ConvFactor_Diff = rad_diff/0.58, ConvFactor_Dir = rad_dir/costheta) %>%
  select(datetime, ConvFactor_Diff, ConvFactor_Dir)

cf_diff_fun <- approxfun(conv_factors$datetime, conv_factors$ConvFactor_Diff)
cf_dir_fun <- approxfun(conv_factors$datetime, conv_factors$ConvFactor_Dir)

#2. create solarnoons for the lat and lon for an entire year for time aligning
origin = lubridate::as_date("2019-12-31")
solarnoons = tibble::tibble(doy = 1:366,
                            solarnoon = suncalc::getSunlightTimes(date = as_date(1:366, origin = origin),
                                                                  lat = lat, lon = lon, keep = "solarNoon")$solarNoon) %>%
  dplyr::mutate(diffnoon = as_datetime((doy*24*3600)+12*3600, origin = origin) - solarnoon)


#3. read the diff and dir files with trees
a <- data.table::fread(file = treescene_diff_file, sep = ",")
b <- data.table::fread(file = treescene_dir_file, sep = ",")
times <- b %>% dplyr::filter(day == 0) %>% .$`time (s)`

#4. create a tibble with all datetime and day - times combinations
step1 <- tibble::tibble(doy = datetime %>% yday, year = datetime %>% year) %>% unique %>%
  dplyr::left_join(solarnoons, by = join_by(doy)) %>%
  dplyr::group_by(year, doy, diffnoon) %>%
  dplyr::reframe(`time (s)` = times) %>%
  dplyr::mutate(datetime = ymd(paste0(year, "-01-01")) + days(doy-1) + seconds(`time (s)`) - diffnoon) %>%
  dplyr::mutate(day = if_else(doy > 355, doy-356, doy + 10)) %>% select(datetime, day, `time (s)`) %>%
  dplyr::mutate(cf_dir = cf_dir_fun(datetime), cf_dif = cf_diff_fun((datetime))) %>% drop_na()

#5. calculate the direct radiation
step2 <- step1 %>%
  dplyr::left_join(b, by = join_by(day, `time (s)`)) %>%
  dplyr::select(datetime, cf_dir, starts_with("S"))  %>%
  dplyr::mutate(across(starts_with("S"), ~ .x* cf_dir, .names = "{.col}"))

#6. Calculate the diffuse radiation
step3 <- step1 %>%
  dplyr::left_join(a, by = join_by(day)) %>%
  dplyr::select(datetime, cf_dif, starts_with("S"))  %>%
  dplyr::mutate(dplyr::across(starts_with("S"), ~ .x* cf_dif, .names = "{.col}"))

#6. sum of diff and dir + pivot data
step4 <- step3 %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("S"), ~ .x + step2[[dplyr::cur_column()]])) %>%
  dplyr::select(!starts_with("cf_")) %>%
  dplyr::rename_with(~ str_split_i(., pattern = " ", i = 2), starts_with("S")) %>%
  tidytable::pivot_longer(cols = -datetime, names_to = c("pos"), values_to = "total_rad") %>%
  tidytable::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
  dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y))

return(step4)

  dplyr::select(datetime, starts_with("S")) %>%
  tidyr::pivot_longer(cols = starts_with("S"))


#6. sum of diff and dir + pivot data
step4 <- step3 %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("S"), ~ .x + step2[[dplyr::cur_column()]])) %>%
  dplyr::select(!starts_with("Con")) %>%
  dplyr::rename_with(~ str_split_i(., pattern = " ", i = 2), starts_with("S")) %>%
  tidytable::pivot_longer(cols = -datetime, names_to = c("pos"), values_to = "total_rad") %>%
  tidytable::separate_wider_delim(pos, names = c("pos_x", "pos_y"), delim = "|") %>%
  dplyr::mutate(pos_x = as.numeric(pos_x), pos_y = as.numeric(pos_y))




#3. make a combination of datetime and day times from app
step1 <- tibble::tibble(doy = conv_factors$datetime %>% yday, year = conv_factors$datetime %>% year) %>% unique %>%
  dplyr::left_join(solarnoons, by = join_by(doy)) %>%
  dplyr::group_by(year, doy, diffnoon) %>%
  dplyr::reframe(`time (s)` = times) %>%
  dplyr::mutate(datetime = ymd(paste0(year, "-01-01")) + days(doy-1) + seconds(`time (s)`) - diffnoon) %>%
  dplyr::mutate(day = if_else(doy > 355, doy-356, doy + 10)) %>% select(datetime, day, `time (s)`)

step1 <- conv_factors %>%
  dplyr::mutate(doy = datetime %>% yday, year = datetime %>% year) %>%
  dplyr::left_join(solarnoons, by = join_by(doy))

#4. direct rad conversion
step2 <- step1 %>%
  dplyr::left_join(conv_factors, by = join_by(datetime)) %>%
  dplyr::left_join(b, by = join_by(day, `time (s)`)) %>%
  dplyr::select(datetime, ConvFactor_Dir, starts_with("S"))  %>%
  tidyr::drop_na()  %>%
  dplyr::mutate(
    across(starts_with("S"), ~ .x* ConvFactor_Dir, .names = "{.col}"))

alpha = 60*pi/180
beta = -30*pi/180
gamma_s = 180*pi/180
gamma_p = 90*pi/180
cos(alpha)*cos(beta) + sin(alpha)*sin(beta)*cos(gamma_s - gamma_p)


alpha = 30*pi/180
beta = 20*pi/180
gamma_s = 180*pi/180
gamma_p = 35*pi/180
cos(alpha)*cos(beta) + sin(alpha)*sin(beta)*cos(gamma_s - gamma_p)


treescene_file = paste0(file_path, "moment_180_12_100_00.csv")

