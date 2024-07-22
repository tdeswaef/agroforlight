library(tidyverse)
library(rgl)

rawalder <- read_delim("Alder medium.csv", delim = ",", col_names = F)
names(rawalder) <- "value"

attribs <- c("x1", "y1", "z1", "x2", "y2", "z2", "radius") %>% rep(nrow(rawalder)/7)
indices <- rep(1:(nrow(rawalder)/7), each=7)

rawalder <- rawalder %>%
  mutate(index = indices, attrib = attribs)

data_wide <- rawalder %>%
  pivot_wider(id_cols = index, names_from = attrib, values_from = value)


#############
rawalder <- read_delim("TestTom.csv", delim = ",", col_names = F)
names(rawalder) <- "value"

attribs <- c("x", "y", "z", "x", "y", "z", "radius") %>% rep(nrow(rawalder)/7)
indices <- rep(1:(nrow(rawalder)/7), each=7)

rawalder <- rawalder %>%
  mutate(index = indices, attrib = attribs)

data_wide <- rawalder %>%
  pivot_wider(id_cols = index, names_from = attrib, values_from = value) 
test <- data_wide %>% filter(index %in% 1:300)
testcyl <- cylinder3d(center = cbind(test$x %>% unlist(), 
                                     test$y %>% unlist(), 
                                     test$z %>% unlist()), 
                      radius = test$radius %>% unlist())

plot_cyl <- function(index){
  cyl <- data_wide %>% filter(index == index)
  cyl3d <- cylinder3d(center = cbind(cyl$x %>% unlist(),
                                     cyl$y %>% unlist(),
                                     cyl$z %>% unlist()),
                      radius = cyl$radius %>% unlist())
  shade3d(cyl3d)
}

data_wide$index %>%
  walk(\(x) plot_cyl(x))

open3d()

shade3d(testcyl)


theta <- seq(0, 2*pi, length.out = 25)
knot <- cylinder3d(
  center = cbind(
    sin(theta) + 2*sin(2*theta), 
    2*sin(3*theta), 
    cos(theta) - 2*cos(2*theta)),
  # e1 = cbind(
  #   cos(theta) + 4*cos(2*theta), 
  #   6*cos(3*theta), 
  #   sin(theta) + 4*sin(2*theta)),
  radius = 0.4, 
  closed = TRUE,
  color = "red")
shade3d(addNormals(subdivision3d(knot, depth = 2))) 
shade3d(knot)
