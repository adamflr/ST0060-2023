library(tidyverse)

# Antal övningar
tibble(Datorövning = 1:8,
       file = paste0("Rmd/Datorövning-", Datorövning, ".Rmd")) %>% 
  mutate(text = map(file, read_lines)) %>% 
  mutate(Exercises = map_dbl(text, ~ sum(grepl("exercise", .x))))

# Funktioner
extract_funs <- function(file){
  text <- read_lines(file)
  chunks <- cumsum(grepl("```", text))
  tibble(text, chunks) %>% 
    mutate(even = chunks %% 2) %>% 
    filter(even == 1) %>% 
    filter(!grepl("```", text)) %>% 
    filter(grepl("\\(", text)) %>% 
    separate(text, c("name"), sep = "\\(", extra = "drop") %>% 
    mutate(last_name = map_chr(name, ~ strsplit(.x, " ")[[1]] %>% rev() %>% `[`(1))) %>% 
    mutate(last_name = paste0(last_name, "()")) %>% 
    pull(last_name) %>% 
    unique() %>% 
    sort()
}

tab_funs <- tibble(Datorövning = 1:8,
       file = paste0("Rmd/Datorövning-", Datorövning, ".Rmd")) %>% 
  mutate(text = map(file, extract_funs)) %>% 
  unnest(cols = text) %>% 
  distinct(Datorövning, text)


# Allsvenskan. Poisson ----
library(tidyverse)
dat <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Allsvenskan%2C%20herrar%2C%201924-2020.csv")

dat %>% 
  filter(as.numeric(Säsong) > 1990) %>% 
  count(Hemmamål) %>% 
  mutate(p = n / sum(n),
         pois = dpois(Hemmamål, lambda = sum(p * Hemmamål))) %>% 
  ggplot(aes(Hemmamål, p)) +
  geom_point() +
  geom_line(aes(y = pois))

dat %>% 
  filter(as.numeric(Säsong) > 1990) %>% 
  count(Bortamål) %>% 
  mutate(p = n / sum(n),
         pois = dpois(Bortamål, lambda = sum(p * Bortamål))) %>% 
  ggplot(aes(Bortamål, p)) +
  geom_point() +
  geom_line(aes(y = pois))

dat2 <- dat %>% filter(as.numeric(Säsong) >= 1990)
write_csv(dat2, "Data/Allsvenskan, herrar, 1990-2020.csv")

dat %>% 
  mutate(Res = sign(Hemmamål - Bortamål)) %>% 
  count(Res) %>% 
  mutate(p = n / sum(n))

dat_ore %>% 
  mutate(Time = paste0(Year, "-", ifelse(Month<10, paste0("0", Month), Month), "-", "01")) %>% 
  select(1,2,5,3,4) %>% 
  write_csv("Oresund-bridge-traffic.csv")

ggplot() + geom_histogram(aes(x = rbinom(10000, 100, 0.9)))

dat %>% 
  filter(as.numeric(Säsong) > 1990) %>% 
  count(Bortamål) %>% 
  mutate(p = n / sum(n),
         pois = dpois(Bortamål, lambda = sum(p * Bortamål))) %>% 
  mutate(E = sum(n) * pois,
         chisq = (n - E)^2 / E) %>% 
  summarise(E = sum(E) * 100, n = sum(n), chisq = sum(chisq))
1 - pchisq(17.5, df = 7)

# Feromoninventering. Leaflet ----
library(readxl)
dat_leaf <- read_excel("Data/Artportalen, SLU feromoninventering, 2011-2013.xlsx", skip = 2)

m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dat_leaf$Nordkoordinat, lat = dat_leaf$Ostkoordinat)
m

library(sf)

coord <- dat_leaf %>% 
  mutate(Ostkoordinat = Ostkoordinat + 50 * (runif(n()) - 0.5),
         Nordkoordinat = Nordkoordinat + 50 * (runif(n()) - 0.5)) %>% 
  st_as_sf(coords = c("Ostkoordinat", "Nordkoordinat")) %>% 
  `st_crs<-`(3847) %>% 
  st_transform("+proj=longlat") %>% 
  st_coordinates() %>% 
  as_tibble()

m <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = coord$X, lat = coord$Y, popup = "TEXT")
m

coord2 <- coord %>% 
  rename(lng = X, lat = Y)
dat_leaf <- bind_cols(dat_leaf, coord2)

openxlsx::write.xlsx(dat_leaf, "Data/Artportalen, feromoninventering, 2011-2013.xlsx")
