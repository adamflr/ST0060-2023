
library(tidyverse)

ToothGrowth <- ToothGrowth %>% mutate(dose = as.character(dose))

ggplot(ToothGrowth, aes(len, supp, fill = dose)) +
  geom_boxplot()

mod <- lm(len ~ dose, data = ToothGrowth)

library(car)
Anova(mod)

library(patchwork)
ToothGrowth <- ToothGrowth %>%
  mutate(Skattade = fitted(mod),
         Residualer = residuals(mod))
g1 <- ggplot(ToothGrowth, aes(sample = Residualer)) + geom_qq() + geom_qq_line()
g2 <- ggplot(ToothGrowth, aes(Skattade, Residualer)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red")
g1 + g2

mod <- lm(len ~ dose * supp, ToothGrowth)
Anova(mod)

library(emmeans)
emmeans(mod, pairwise ~ dose | supp)

library(gapminder)
dat_eu07 <- gapminder %>%
  filter(year == 2007, continent == "Europe") %>%
  mutate(gdpPercap = gdpPercap / 1000)

library(ggrepel)
ggplot(dat_eu07, aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3)

mod <- lm(lifeExp ~ gdpPercap, data = dat_eu07)
summary(mod)

ggplot(dat_eu07, aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3) +
  geom_smooth(method = lm)

library(car)
Anova(mod)

dat_eu07 <- dat_eu07 %>%
  mutate(Residualer = residuals(mod),
         Skattade = fitted(mod))

ggplot(dat_eu07, aes(sample = Residualer)) + geom_qq() + geom_qq_line()
ggplot(dat_eu07, aes(Skattade, Residualer)) + geom_point()

cor(dat_eu07$lifeExp, dat_eu07$gdpPercap)
cor.test(dat_eu07$lifeExp, dat_eu07$gdpPercap)


library(osmdata)
dat_osm <- opq(bbox = 'Malmö') %>%
    add_osm_feature(key = 'admin_level', value = '10') %>%
    osmdata_sf()

dat_osm_pol <- dat_osm$osm_multipolygons

ggplot(dat_osm_pol, aes()) +
  geom_sf() +
  geom_sf_text(aes(label = name), size = 3)

dat_osm <- opq(bbox = 'Malmö') %>%
    add_osm_feature(key = 'amenity', value = 'restaurant') %>%
    osmdata_sf()

dat_osm_point <- dat_osm$osm_points %>%
  filter(cuisine %in% c("pizza", "sushi", "burger", "chinese", "indian", "vietnamese"))

ggplot() +
  geom_sf(data = dat_osm_pol) +
  geom_sf(data = dat_osm_point, aes(color = cuisine), size = 2)


library(rvest)

url <- "https://www.criterion.com/shop/browse/list"
html <- read_html(url)

dat_crit <- html %>%
  html_table()

dat_crit <- dat_crit[[1]] %>%
  select(-2) %>%
  filter(Director != "")
dat_crit

url <- "https://en.wikipedia.org/wiki/List_of_Nobel_laureates_in_Literature"
dat_nob <- url %>%
  read_html() %>%
  html_table()
dat_nob <- dat_nob[[1]]

