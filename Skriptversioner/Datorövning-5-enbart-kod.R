
library(tidyverse)

dat_pH <- data.frame(pH = c(6.3, 6.55, 6.75, 6.4, 7.25, 6.65, 6.8, 7.3, 7.15, 6.7))

ggplot(dat_pH, aes(pH, 0)) +
  geom_point(size = 4) +
  geom_vline(xintercept = 7, size = 5, color = "red", alpha = 0.4) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())

t.test(dat_pH$pH, mu = 7)

ggplot(dat_pH, aes(sample = pH)) +
  geom_qq() +
  geom_qq_line()

library(tidyverse)
dat_alls <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Allsvenskan%2C%20damer%2C%202000-2020.csv")

ggplot(dat_alls, aes(hemmamal, bortamal)) +
  geom_jitter(size = 0.1)

dat_alls %>% count(Bortaseger = bortamal > hemmamal)

n <- 947 + 1803
p_est <- 947 / n

p_est
n

p0 <- 0.33
z_value <- (p_est - p0) / sqrt(p0 * (1 - p0) / n)
z_value

dat_norm <- data.frame(x = seq(-4, 4, 0.1)) %>%
  mutate(p = dnorm(x))

ggplot(dat_norm) +
  geom_line(aes(x, p)) +
  geom_ribbon(aes(x = x, ymin = 0, ymax = p), data = dat_norm %>% filter(x > abs(z_value)), fill = "salmon") +
  geom_ribbon(aes(x = x, ymin = 0, ymax = p), data = dat_norm %>% filter(x < -abs(z_value)), fill = "salmon")

2 * pnorm(-z_value)

prop.test(x = 947, n = 2750, p = 0.33, correct = F)

z_value^2

n <- 947 + 1803
p <- 947 / n

p - 1.96 * sqrt(p * (1 - p) / n)
p + 1.96 * sqrt(p * (1 - p) / n)


library(binom)
binom.asymp(x = 947, n = 2750)

O <- c(947, 518, 1285)
E <- c(0.33,0.20,0.47) * 2750

chisq_value <- sum((O - E)^2 / E)

dat_chisq <- data.frame(x = seq(0, 10, 0.1)) %>%
  mutate(p = dchisq(x, df = 2))

ggplot() +
  geom_line(aes(x, p), data = dat_chisq) +
  geom_ribbon(aes(x, ymin = 0, ymax = p), data = dat_chisq %>% filter(x > chisq_value), fill = "salmon")

1 - pchisq(chisq_value, df = 2)

qchisq(0.95, df = 2)

chisq.test(O, p = c(0.33, 0.2, 0.47))

chisq.test(c(6,4), p = c(0.51, 0.49))

test <- chisq.test(c(947, 518, 1285), p = c(0.33, 0.2, 0.47))
test$expected
test$observed
(test$observed - test$expected)^2 / test$expected

mean_goals <- mean(dat_alls$hemmamal + dat_alls$bortamal)
mean_goals

dat_goals <- dat_alls %>%
  count(Mål = bortamal + hemmamal, name = "O") %>%
  mutate(p = dpois(Mål, lambda = mean_goals),
         E = p * 2750)
dat_goals

dat_goals_merged <- dat_goals %>%
  mutate(Mål = ifelse(Mål > 9, 10, Mål)) %>%
  group_by(Mål) %>%
  summarise(O = sum(O),
            p = sum(p),
            E = sum(E))
dat_goals_merged

chisq_value <- sum((dat_goals_merged$O - dat_goals_merged$E)^2 / dat_goals_merged$E)
1 - pchisq(chisq_value, df = 9)

m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = 174.768, lat = -36.852, popup="The birthplace of R")
m

dat_leaf %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$Stamen.Toner)

dat_leaf %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = dat_leaf$lng, lat = dat_leaf$lat, radius = 10)

dat_leaf %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = dat_leaf$lng, lat = dat_leaf$lat, radius = 10, popup = dat_leaf$Rödlistade)

