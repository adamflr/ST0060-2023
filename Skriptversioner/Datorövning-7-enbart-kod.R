
library(tidyverse)

dat_fish <- data.frame(Vessel = c("A", "B", "C", "D", "E", "F"),
                       Region = c("N", "N", "N", "S", "S", "S"),
                       Species1 = c(115.7, 98.5, 82.1, 89.2, 95.7, 99.4),
                       Species2 = c(122.8, 105.3, 99.8, 106.8, 114, 102.7))

dat_long <- dat_fish %>%
  pivot_longer(-c(Vessel, Region), names_to = "Species", values_to = "Catch")
ggplot(dat_long, aes(Species, Catch, group = Vessel)) +
  geom_point() +
  geom_line() +
  labs(title = "Fångster av två arter", subtitle = "Linje sammanbinder observationer från samma fartyg")

ggplot(dat_fish, aes(Species1, Region)) +
  geom_point() +
  labs(title = "Fångster i två regioner")

t.test(dat_fish$Species1, dat_fish$Species2, paired = T)




t.test(Species1 ~ Region, data = dat_fish, var.equal = T)

dat_parti <- data.frame(Kommun = c("Malmö", "Lund", "Kävlinge"),
                        S = c(54, 102, 40),
                        M = c(30, 98, 53),
                        MP = c(7, 50, 5))
dat_parti

chisq.test(dat_parti[, -1])

PlantGrowth

ggplot(PlantGrowth, aes(group, weight)) +
  geom_point()

mod <- lm(weight ~ group, data = PlantGrowth)

library(car)
Anova(mod)


library(emmeans)
emmeans(mod, pairwise ~ group)

emmeans(mod, pairwise ~ group, adjust = "none")

em <- emmeans(mod, pairwise ~ group)

library(multcomp)
cld(em, Letters = letters)

library(MASS)
oats_marvel <- oats %>% filter(V == "Marvellous")
oats_marvel

ggplot(oats_marvel, aes(N, Y, color = B, group = B)) +
  geom_point(size = 4) +
  geom_line()

mod_bl <- lm(Y ~ N + B, data = oats_marvel)
Anova(mod_bl)

mod_wo_block <- lm(Y ~ N, data = oats_marvel)
Anova(mod_wo_block)

cld(emmeans(mod_bl, ~ N), Letters = letters)

cld(emmeans(mod_wo_block, ~ N), Letters = letters)

ggplot(oats, aes(N, Y, color = B)) +
  geom_point(size = 4) +
  facet_wrap(~ V)

mod_two_fact <- lm(Y ~ N * V + B, data = oats)

Anova(mod_two_fact)

emmeans(mod_two_fact, ~ N)
emmeans(mod_two_fact, ~ N + V)
emmeans(mod_two_fact, ~ N | V)

cld(emmeans(mod_two_fact, ~ N | V), Letters = letters)

oats <- oats %>%
  mutate(Residualer = residuals(mod_two_fact),
         Skattade = fitted(mod_two_fact))

g_hist <- ggplot(oats, aes(Residualer)) + geom_histogram(bins = 20)
g_qq <- ggplot(oats, aes(sample = Residualer)) + geom_qq() + geom_qq_line()

library(patchwork)
g_hist + g_qq

ggplot(oats, aes(x = Skattade, y = Residualer)) +
  geom_point() +
  geom_hline(yintercept = 0, alpha = 0.3)


library(vegan)


library(factoextra)

data(dune)
dune <- dune %>%
  mutate(Site = 1:n(),
         Type = rep(c("A", "B"), each = 10))

dune_long <- dune %>%
  pivot_longer(-c(Site, Type), names_to = "Species", values_to = "Abundance")

ggplot(dune_long, aes(Site, Species, fill = Abundance)) +
  geom_tile() +
  scale_fill_gradient2() +
  theme_minimal()

ggplot(dune_long %>% filter(Abundance > 0), aes(Site, Species, size = Abundance, color = Type)) +
  geom_point()

dune_data <- dune %>% select(-Site, -Type)
d <- dist(dune_data, method = "euclidean")
hc <- hclust(d)
plot(hc, hang = -1, labels = dune$Type,
     axes = F, xlab = "", ylab = "", ann = F)

dune_data <- t(dune_data)
d <- dist(dune_data, method = "euclidean")
hc <- hclust(d)
plot(hc, hang = -1,
     axes = F, xlab = "", ylab = "", ann = F)

dune_data <- dune %>% select(-Site, -Type)
pca <- prcomp(dune_data, scale. = F)
fviz_pca_biplot(pca, geom.ind = "point", habillage = dune$Type, labelsize = 3)

-(0.3 * log(0.3) + 0.5 * log(0.5) + 0.2 * log(0.2))

diver <- diversity(dune_data, index = "shannon")
dune <- dune %>% mutate(Diversity = diver)

ggplot(dune, aes(Diversity, Type)) + geom_point()

mod <- lm(Diversity ~ Type, data = dune)
Anova(mod)
emmeans(mod, ~ Type)

