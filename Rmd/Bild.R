url <- "https://uploads0.wikiart.org/00339/images/leonardo-da-vinci/mona-lisa-c-1503-1519.jpg"
library(magick)

url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Nils_Dardel_D%C3%B6ende_dandyn.jpg/1920px-Nils_Dardel_D%C3%B6ende_dandyn.jpg"
img <- url %>% image_read()
img

img <- img %>% image_quantize(max = 10)
info <- img %>% image_info()
pixel_values <- img %>% image_data() %>% as.vector()
dat_pix <- expand_grid(y = info$height:1, x = 1:info$width, color = c("R", "G", "B")) %>% 
  mutate(value = pixel_values) %>% 
  pivot_wider(values_from = value, names_from = color) %>% 
  mutate(hex = paste0("#", R, G, B))

ggplot(dat, aes(x, y)) +
  geom_raster(fill = dat$hex)

ggplot(dat, aes(x, y, fill = hex)) +
  geom_raster() +
  scale_fill_brewer(palette = "Pastel1") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")

img %>% image_resize("25%")
img %>% image_blur(50, 5) %>% image_resize("25%")

img %>% image_canny()

g <- img %>% image_ggplot()

img %>% image_convert(colorspace = "gray", depth = 8)
img <- img %>% image_quantize(max = 20)

dat2 <- dat %>% count(hex)
dat2 %>% ggplot(aes(n, reorder(hex, n))) + geom_col(fill = dat2$hex)

img <- img %>% image_quantize(max = 2) %>% image_contrast(50)

img %>% image_convert(colorspace = "gray") %>% image_threshold(channel = 1)

url <- "https://www.wikihow.com/images/thumb/6/6f/Start-a-Text-Conversation-with-a-Girl-%28test%29-Step-11.jpg/aid2660480-v4-728px-Start-a-Text-Conversation-with-a-Girl-%28test%29-Step-11.jpg.webp"
img <- image_read(url)
img
image_ocr(img) %>% cat()
image_ocr_data(img)
