
library(tidyverse)

dbinom(3, 5, prob = 0.1)

dpois(5, lambda = 10)

1 - pnorm(13, mean = 10, sd = 2)

library(tidyverse)

dat <- data.frame(x = c(49.8, 58.4, 49.4, 57.1, 52.2, 49.1, 44.6, 55.4))
dat

ggplot(dat, aes(x, 0)) +
  geom_point() +
  geom_vline(xintercept = 50, color = "red")

mean(dat$x)
sd(dat$x)

t_value <- (52 - 50) / (4.680354 / sqrt(8))

dat_t <- data.frame(x = seq(-4, 4, 0.1)) %>%
  mutate(p = dt(x, df = 7),
         P = pt(x, df = 7))

ggplot(dat_t, aes(x, p)) +
  geom_line()

ggplot(dat_t, aes(x, P)) +
  geom_line()

ggplot(dat_t) +
  geom_line(aes(x, p)) +
  geom_ribbon(aes(x = x, ymin = 0, ymax = p), data = dat_t %>% filter(x > abs(t_value)), fill = "salmon") +
  geom_ribbon(aes(x = x, ymin = 0, ymax = p), data = dat_t %>% filter(x < -abs(t_value)), fill = "salmon")

2 * pt(-abs(t_value), 7)

t.test(dat$x, mu = 50)

t.test(dat$x, mu = 50)
t.test(dat$x, mu = 48)

mean(dat$x)
sd(dat$x)
qt(0.975, 7)

interval <- t.test(dat$x)$conf.int
ggplot(dat, aes(x, 0)) +
  geom_point() +
  annotate("errorbar", xmin = interval[1], xmax = interval[2], y = -1, width = 0.1)

ggplot(dat, aes(x)) + geom_histogram(bins = 5)

n <- 10
ggplot() + geom_histogram(aes(x = rnorm(n)), bins = 30)

ggplot(dat, aes(sample = x)) + geom_qq() + geom_qq_line()

n <- 100
dat_norm <- data.frame(x = rnorm(n))
ggplot(dat_norm, aes(sample = x)) + geom_qq() + geom_qq_line()

n <- 2

dat_sim_unif <- expand_grid(Observation = 1:n, Upprepning = 1:10000) %>%
  mutate(x = runif(n())) %>%
  group_by(Upprepning) %>%
  summarise(x = mean(x))

ggplot(dat_sim_unif, aes(x)) + geom_histogram(bins = 50)
ggplot(dat_sim_unif, aes(sample = x)) + geom_qq() + geom_qq_line()

n <- 10
lambda <- 10
dat_sim_unif <- expand_grid(Observation = 1:n, Upprepning = 1:10000) %>%
  mutate(x = rpois(n(), lambda = lambda)) %>%
  group_by(Upprepning) %>%
  summarise(x = mean(x))

ggplot(dat_sim_unif, aes(x)) + geom_histogram(bins = 30)
ggplot(dat_sim_unif, aes(sample = x)) + geom_qq() + geom_qq_line()

library(gapminder)
gapminder_2007 <- gapminder %>% filter(year == 2007)

g1 <- ggplot(gapminder_2007, aes(pop)) + geom_histogram(bins = 30)
g2 <- ggplot(gapminder_2007, aes(sample = pop)) + geom_qq() + geom_qq_line()
g3 <- ggplot(gapminder_2007, aes(sqrt(pop))) + geom_histogram(bins = 30)
g4 <- ggplot(gapminder_2007, aes(sample = sqrt(pop))) + geom_qq() + geom_qq_line()
g5 <- ggplot(gapminder_2007, aes(log10(pop))) + geom_histogram(bins = 30)
g6 <- ggplot(gapminder_2007, aes(sample = log10(pop))) + geom_qq() + geom_qq_line()

library(patchwork)
g1 + g3 + g5 + g2 + g4 + g6

dat_sim <- data.frame(x = rnorm(10, mean = 7, sd = 5))
t.test(dat_sim$x, mu = 7)

dat_sim <- data.frame()
for(i in 1:1000){
  new_data <- data.frame(x = rnorm(10, mean = 7, sd = 5))
  test <- t.test(new_data$x, mu = 7)
  new_results <- data.frame(t_value = test$statistic, p_value = test$p.value,
               ci_lower = test$conf.int[1], ci_upper = test$conf.int[2])
  dat_sim <- bind_rows(dat_sim, new_results)
}

ggplot(dat_sim) +
  geom_histogram(aes(t_value, y = ..density..), bins = 50, fill = "white", color = "black") +
  geom_function(fun = dt, args = list(df = 9), color = "red", size = 1)

mean(dat_sim$p_value < 0.05)

