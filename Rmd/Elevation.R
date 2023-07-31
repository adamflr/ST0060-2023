library(rayshader)

library(raster)
library(sp)
dat_tif <- raster("Data/Ven_elev/619_35_5050_2010.tif")

tifs <- list.files("Data/Ven_elev", "tif", full.names = T)[1:5]
tif_l <- list()

j <- 1
for(i in tifs){
  tif_l[[j]] <- raster(i)
  j <- j + 1
}

raster::merge(tif_l[[1]], tif_l[[2]]) %>% image()
raster::merge(tif_l[[1]], tif_l[[2]],
              tif_l[[3]], tif_l[[4]],
              tif_l[[5]]) %>% 
  image()

dat_el <- raster::merge(tif_l[[1]], tif_l[[2]],
                        tif_l[[3]], tif_l[[4]],
                        tif_l[[5]])

coord <- coordinates(dat_el)
vect <- raster_to_matrix(dat_el) %>% as.vector()
dat <- tibble(x = coord[,1], y = coord[,2], el = vect)

sample <- dat[sample(9375000, 100000),]
ggplot(sample, aes(x, y, color = el)) +
  geom_point()

dat_el_mat <- dat_el %>% raster_to_matrix()
dat_el_mat <- dat_tif %>% raster_to_matrix()
dat_el_mat <- dat_el_mat[seq(1, nrow(dat_el_mat), 10), seq(1, ncol(dat_el_mat), 10)]

dat_el_mat %>% 
  sphere_shade(texture = "desert") %>% 
  add_water(detect_water(dat_el_mat), color = "desert") %>%
  plot_map()

dat_el_mat %>% 
  sphere_shade(texture = "desert") %>% 
  plot_3d(dat_el_mat, zscale = 5, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))

dat_img <- raster("Data/Ven_elev/rak2_52414b5f4a3131322d312d3233_1_1.tiff", band = 1)
library(magick)
dat_img <- image_read("Data/Ven_elev/rak2_52414b5f4a3131322d312d3233_1_1.tiff")
dat_img_mat <- image_data(dat_img) %>% as.double()

# dat_img_mat <- array(raster_to_matrix(dat_img)
# dim(dat_el_mat)
dat_img_mat <- dat_img_mat[seq(1, 4710, length.out = 375), seq(1, 5210, length.out = 250), 1:3]
dat_el_mat %>% 
  sphere_shade(texture = "desert") %>% 
  add_overlay(dat_img_mat) %>% 
  add_shadow(ray_shade(dat_el_mat, sunaltitude = 5, zscale = 5), max_darken = 0.4) %>% 
  plot_3d(dat_el_mat, zscale = 2.5, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))

