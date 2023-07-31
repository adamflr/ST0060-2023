
library(tidyverse)


library(gapminder)

gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(`Livslängd, medel` = mean(lifeExp),
            `Befolkning, median` = median(pop),
            `Bnp per capita, standardavvikelse` = sd(gdpPercap))

ggplot(gapminder, aes(lifeExp, continent, fill = continent)) +
  geom_boxplot() +
  facet_wrap(~ year)

dat_sum <- gapminder %>%
  group_by(continent, year) %>%
  summarise(Mean = mean(lifeExp),
            SD = sd(lifeExp))
dat_sum

ggplot(dat_sum, aes(continent, Mean, fill = continent)) +
  geom_col() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.3) +
  facet_wrap(~ year)

dat_dice6 <- data.frame(x = c(1,2,3,4,5,6),
                        p = 1/6)
dat_dice6

ggplot(dat_dice6, aes(x, p)) + geom_col()

dat_dice6 %>%
  mutate(x_times_p = x * p) %>%
  summarise(Expected_value = sum(x_times_p))

dat_dice6 %>%
  mutate(mean = sum(x * p)) %>%
  mutate(differenser = x - mean,
         kvadrater = differenser^2,
         produkter = kvadrater * p) %>%
  summarise(varians = sum(produkter),
            standardavvikelse = sqrt(varians))

slumputfall <- sample(x = dat_dice6$x, size = 10000, replace = T)

mean(slumputfall)
var(slumputfall)

dbinom(3, size = 10, prob = 0.5)

dat_bin <- data.frame(x = 0:10) %>%
  mutate(p = dbinom(x, size = 10, prob = 0.5))

ggplot(dat_bin, aes(x, p)) + geom_col()

dat_bin <- data.frame(x = 0:10) %>%
  mutate(p = dbinom(x, size = 10, prob = 0.5),
         P = pbinom(x, size = 10, prob = 0.5))
dat_bin

pbinom(7, size = 10, prob = 0.5)

dat_bin <- data.frame(x = 0:10) %>%
  mutate(p = dbinom(x, size = 10, prob = 0.5),
         P = pbinom(x, size = 10, prob = 0.5))

ggplot(dat_bin, aes(x, p, fill = x <= 7)) +
  geom_col()

dat_two_bin <- data.frame(x = 0:30) %>%
  mutate(binom1 = dbinom(x, size = 5000, p = 0.0006),
         binom2 = dbinom(x, size = 400, p = 0.0075),
         Differens = binom1 - binom2)
dat_two_bin

ggplot(dat_two_bin) +
  geom_line(aes(x, binom1), color = "red") +
  geom_point(aes(x, binom2), color = "blue")

dat_two_dist <- data.frame(x = 0:30) %>%
  mutate(pois = dpois(x, lambda = 3),
         binom = dbinom(x, size = 400, p = 0.0075),
         Differens = binom - pois)
dat_two_dist

ggplot(dat_two_dist) +
  geom_line(aes(x, pois), color = "red") +
  geom_point(aes(x, binom), color = "blue")

ggplot() +
  geom_function(fun = dnorm, args = list(mean = 5, sd = 2), color = "red", size = 2) +
  geom_function(fun = dnorm, args = list(mean = 3, sd = 1), color = "blue", size = 2) +
  geom_function(fun = dnorm, args = list(mean = 5, sd = 1), color = "green3", size = 2) +
  xlim(0, 10)

pnorm(1, mean = 2, sd = 3)

x_value <- 1
mu <- 2
sigma <- 3

P_value <- pnorm(x_value, mean = mu, sd = sigma)
P_value

dat_norm <- data.frame(x = seq(from = mu - 4 * sigma, to = mu + 4 * sigma, 0.1)) %>%
  mutate(p = dnorm(x, mean = mu, sd = sigma),
         P = pnorm(x, mean = mu, sd = sigma))

g1 <- ggplot(dat_norm, aes(x, p)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = p), data = dat_norm %>% filter(x < x_value), fill = "salmon", color = "black") +
  labs(title = "Täthetsfunktion")

g2 <- ggplot(dat_norm, aes(x, P)) +
  geom_line() +
  annotate("segment", x = x_value, y = 0, xend = x_value, yend = P_value) +
  annotate("segment", x = x_value, y = P_value, xend = -Inf, yend = P_value) +
  labs(title = "Fördelningsfunktion")


library(patchwork)
g1 / g2

pnorm(1, mean = 0, sd = 1) - pnorm(-1, mean = 0, sd = 1)

x_values <- c(-1,1)
mu <- 0
sigma <- 1

dat_norm <- data.frame(x = seq(from = mu - 4 * sigma, to = mu + 4 * sigma, 0.1)) %>%
  mutate(p = dnorm(x, mean = mu, sd = sigma))

ggplot(dat_norm, aes(x, p)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = p), data = dat_norm %>% filter(x < max(x_values) & x > min(x_values)), fill = "salmon", color = "black") +
  labs(title = "Täthetsfunktion")

1 - pnorm(1.96)

pnorm(7, mean = 8, sd = 4)

pnorm(-0.25, mean = 0, sd = 1)

dat_pois <- data.frame(x = rpois(10000, lambda = 3)) %>%
  count(x) %>%
  mutate(p = n / sum(n),
         theoretical_p = dpois(x, lambda = 3))

ggplot(dat_pois, aes(x, p)) +
  geom_point() +
  geom_line(aes(y = theoretical_p))

dat_pois <- data.frame(x1 = rpois(10000, lambda = 3),
                       x2 = rpois(10000, lambda = 2)) %>%
  mutate(x = x1 + x2) %>%
  count(x) %>%
  mutate(p = n / sum(n),
         theoretical_p = dpois(x, lambda = 5))

ggplot(dat_pois, aes(x, p)) +
  geom_point() +
  geom_line(aes(y = theoretical_p))

dat_dice_sum <- data.frame(x1 = sample(c(1,2,3,4,5,6), size = 10000, replace = T),
                           x2 = sample(c(1,2,3,4,5,6), size = 10000, replace = T)) %>%
  mutate(x = x1 + x2) %>%
  count(x) %>%
  mutate(p = n / sum(n))

ggplot(dat_dice_sum, aes(x, p)) +
  geom_col()


library(ggforce)
g <- ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1), fill = NA, color = "black")
g

dat_random <- data.frame(x = runif(100, min = -1, max = 1),
                         y = runif(100, min = -1, max = 1))

g + geom_point(aes(x, y), data = dat_random)

dat_random <- data.frame(x = runif(100, min = -1, max = 1),
                         y = runif(100, min = -1, max = 1)) %>%
  mutate(inner = x^2 + y^2 < 1)

4 * mean(dat_random$inner)
pi


x_original <- c(0,1,2)
y_original <- c(0,2,0)


x_new <- 0.4
y_new <- 0.2
dat_tri <- data.frame(x = x_new, y = y_new)

n <- 100

for(i in 1:n){
  new_point <- sample(c(1,2,3), 1)
  x_new <- (x_new + x_original[new_point]) / 2
  y_new <- (y_new + y_original[new_point]) / 2

  dat_tri <- bind_rows(dat_tri, data.frame(x = x_new, y = y_new))
}

ggplot(dat_tri, aes(x, y)) +
  geom_point()

