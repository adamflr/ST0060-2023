# install.packages("tidyverse")
library(tidyverse)

dat <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Spotify_data.csv")
dat

mean(dat$tempo, na.rm = T)

dat %>%
  filter(artist_name == "Tame Impala", tempo > 170) %>%
  select(artist_name, track_name, tempo)

dat_small <- dat %>% filter(artist_name == "The Weeknd")

ggplot(dat_small, aes(tempo, danceability, size = valence, color = mode_name)) +
  geom_point()

library(readxl)

gapminder <- read_excel("C:/Users/User_name/Downloads/Gapminder.xlsx")
gapminder

getwd()

gapminder <- read_excel("Data/Gapminder.xlsx")
gapminder

# install.packages("gapminder")
library(gapminder)
gapminder

gapminder <- gapminder %>%
  mutate(gdp = gdpPercap * pop)

gapminder %>% select(gdpPercap, pop, gdp)

gapminder %>%
  mutate(`National GDP` = gdpPercap * pop)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  facet_wrap(~ year)

g <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, text = country)) +
  geom_point() +
  facet_wrap(~ year)


library(plotly)
ggplotly(g)

gdpPercap <- gapminder$gdpPercap
mean(gdpPercap)
median(gdpPercap)

gapminder %>%
  summarise(Mean = mean(gdpPercap),
            Median = median(gdpPercap))

gapminder %>%
  group_by(year) %>%
  summarise(Mean = mean(gdpPercap),
            Median = median(gdpPercap))

dat_gdp_2007 <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(Mean = mean(gdpPercap))

ggplot(dat_gdp_2007, aes(continent, Mean)) +
  geom_col()

ggplot(dat_gdp_2007, aes(continent, Mean)) +
  geom_col() +
  geom_point(aes(continent, gdpPercap), data = gapminder %>% filter(year == 2007))

ggplot(dat_gdp_2007, aes(continent, Mean)) +
  geom_col() +
  geom_text(aes(continent, gdpPercap, label = country), data = gapminder %>% filter(year == 2007), size = 2)

gdpPercap <- gapminder$gdpPercap

var(gdpPercap)
sd(gdpPercap)
IQR(gdpPercap)

gapminder %>%
  summarise(Varians = var(gdpPercap),
            Standardavvikelse = sd(gdpPercap),
            Kvartilavstånd = IQR(gdpPercap))

gapminder %>%
  group_by(year) %>%
  summarise(Varians = var(gdpPercap),
            Standardavvikelse = sd(gdpPercap),
            Kvartilavstånd = IQR(gdpPercap))

dat_sum <- gapminder %>%
  group_by(year, continent) %>%
  summarise(Mean = mean(gdpPercap),
            SE = sd(gdpPercap) / sqrt(n()))
dat_sum

ggplot(dat_sum, aes(year, Mean, color = continent)) +
  geom_line() +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE))

dat_sum <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(Mean = mean(lifeExp),
            SD = sd(lifeExp))
dat_sum

ggplot(dat_sum, aes(continent, Mean, fill = continent)) +
  geom_col()+
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD)) +
  labs(title = "Average life expectancy by continent, 2007",
       caption = "Errorbars given by mean +/- standard deviation.
       Source: Gapminder")

ggplot(gapminder, aes(year, lifeExp, fill = continent, group = year)) +
  geom_boxplot() +
  facet_wrap(~ continent)

gapminder_2007 <- gapminder %>% filter(year == 2007)
gapminder_agg <- gapminder_2007 %>%
  group_by(continent) %>%
  summarise(lifeExp = mean(lifeExp))
gapminder_agg

ggplot() +
  geom_boxplot(aes(lifeExp, continent), data = gapminder_2007) +
  geom_point(aes(lifeExp, continent), data = gapminder_agg, color = "red", shape = "X", size = 6)

gapminder_eu <- gapminder %>% filter(continent == "Europe")

ggplot(gapminder_eu, aes(gdpPercap, lifeExp, group = country, label = country)) +
  geom_text(data = gapminder_eu %>% filter(year == 2007)) +
  geom_path(color = "blue", size = 3, data = gapminder_eu %>% filter(country == "Sweden"), alpha = 0.3) +
  geom_path(color = "red", size = 3, data = gapminder_eu %>% filter(country == "Poland"), alpha = 0.3)

dat_sum <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(Mean = mean(lifeExp),
            SD = sd(lifeExp))
dat_sum

dat_sum %>%
  mutate(mean_plus_minus_sd = paste(Mean, "±", SD))

dat_sum <- dat_sum %>%
  mutate(mean_plus_minus_sd = paste(round(Mean, 1), "±", round(SD, 1)))

getwd()
write_csv(dat_sum, "Exporterad data från R.csv")

dat <- data.frame(Rad = c(1,2,3,4,5), x = c(6,3,2,3,5))
dat

dat <- dat %>%
  mutate(Medelvärde = mean(x))
dat

dat <- dat %>%
  mutate(Differenser = x - Medelvärde)
dat

dat <- dat %>%
  mutate(Kvadrater = Differenser^2)
dat

dat_sum <- dat %>%
  summarise(Kvadratsumma = sum(Kvadrater))
dat_sum

dat_sum %>% mutate(Varians = Kvadratsumma / 4)

var(dat$x)

dat_dice <- data.frame(Utfall = c(6,3,2,3,5)) %>%
  mutate(Kast = 1:n())
dat_dice

dat_dice <- dat_dice %>%
  mutate(`Kumulativ summa` = cumsum(Utfall),
         `Kumulativt medelvärde` = `Kumulativ summa` / Kast)
dat_dice

library(plotly)

dat_ex <- data.frame(Var1 = c(1,2,3), Var2 = c(3,1,2), Var3 = c(2,3,1), Type = c("A", "B", "C"))
dat_ex

plot_ly(dat_ex, x = ~Var1, y = ~Var2, z = ~Var3, color = ~Type) %>%
  add_markers()

