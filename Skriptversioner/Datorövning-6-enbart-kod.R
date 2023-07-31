
library(tidyverse)

prop.test(x = 16, n = 20, p = 0.5, correct = F)

library(binom)
binom.asymp(16, 20)

chisq.test(c(102, 53, 75, 12), p = c(0.5, 0.15, 0.25, 0.1))

dat_apple <- tibble(Tree = 1:4,
              Before = c(48, 43, 30, 47),
              After = c(51, 44, 42, 54))
dat_apple

dat_long <- dat_apple %>% pivot_longer(-Tree, names_to = "Time", values_to = "Height")
dat_long

t.test(dat_apple$Before - dat_apple$After)

t.test(dat_apple$Before, dat_apple$After, paired = T)

dat_berry <- data.frame(Behandling = c("A", "A", "A", "A", "B", "B", "B", "B"),
              Vikt = c(40, 48.2, 39.2, 47.9, 57.5, 61.5, 58, 66.5))
dat_berry

ggplot(dat_berry, aes(Behandling, Vikt)) +
  geom_point()


t.test(Vikt ~ Behandling, data = dat_berry, var.equal = T)


#
Vikt_A <- dat_berry$Vikt[dat_berry$Behandling == "A"]

#
Vikt_B <- dat_berry$Vikt[dat_berry$Behandling == "B"]

t.test(Vikt_A, Vikt_B, var.equal = T)

pairwise.t.test(dat_berry$Vikt, dat_berry$Behandling, p.adjust.method = "none", pool.sd = F)

prop.test(c(17, 26), c(50, 60), correct = F)

dat_titanic <- Titanic %>% data.frame() %>% filter(Sex == "Male", Age == "Adult")
dat_titanic

dat_wide <- dat_titanic %>%
  pivot_wider(names_from = Survived, values_from = Freq)
dat_wide

ggplot(dat_titanic, aes(Class, Freq, fill = Survived)) +
  geom_col(position = position_fill(), color = "black") +
  scale_fill_manual(values = c("red4", "white"))

dat_wide[, 4:5]

chisq.test(dat_wide[, 4:5])

test_result <- chisq.test(dat_wide[, 4:5])
test_result$expected


library(magick)

url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Nils_Dardel_D%C3%B6ende_dandyn.jpg/1920px-Nils_Dardel_D%C3%B6ende_dandyn.jpg"
img <- image_read(url)
img

img <- img %>%
  image_resize("500")
img

img %>%
  image_modulate(saturation = 50) %>%
  image_modulate(hue = 50)

img %>% image_quantize(max = 10)

img <- img %>% image_quantize(max = 10)
info <- img %>% image_info()
pixel_values <- img %>% image_data() %>% as.vector()

dat_pix <- expand_grid(y = info$height:1, x = 1:info$width, color = c("R", "G", "B")) %>%
  mutate(value = pixel_values) %>%
  pivot_wider(values_from = value, names_from = color) %>%
  mutate(hex = paste0("

dat_pix

ggplot(dat_pix, aes(x, y)) +
  geom_raster(fill = dat_pix$hex)

dat_pix_count <- dat_pix %>%
  count(hex) %>%
  mutate(hex = reorder(hex, n))

ggplot(dat_pix_count, (aes(n, hex))) +
  geom_col(fill = dat_pix_count$hex)

