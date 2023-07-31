# Grundläggande kodexempel på vanliga analyser för populationsdata
# BI1397 och ST0060. HT2021

# Paket
library(tidyverse)
library(vegan) # Inget absolut måste. Används för datan och diversitetsindex här.

# Importera exempeldata. Skapa en fiktiv behandling som Location
data(dune)
dat <- dune %>% 
  mutate(id = 1:n(),
         Location = rep(letters[1:2], each = 10))

# Om NA i datan (pga tomma celler i Excel)
dat <- dat %>% 
  mutate_all(~ replace_na(.x, 0))

# Pivotera till långt format för plottar i ggplot2
dat_long <- dat %>% 
  pivot_longer(-c(id, Location), values_to = "Count", names_to = "Species") # Den bit som här är 'id, Location' måste ändras beroende på den faktiska datan
dat_long

# Boxplot för antal per art
ggplot(dat_long, aes(Count, Species, fill = Location)) +
  geom_boxplot()

# Tile-plot för antal per plats och art. Färg ger relativt antal
ggplot(dat_long, aes(x = id, y = Species, fill = Count)) +
  geom_tile() +
  scale_fill_gradient2() +
  facet_wrap(~ Location, scales = "free_x")

# Spridningsdiagram med storlek för antal
dat_long %>% 
  filter(Count > 0) %>% 
  ggplot(aes(x = id, y = Species, size = Count)) +
  geom_point() +
  facet_wrap(~ Location, scales = "free_x")

# Stapeldiagram, antal efter plats, per art
dat_long %>% 
  count(Location, Species, wt = Count) %>% 
  ggplot(aes(Location, n, fill = Location)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Species)

# Hierarkiska kluster ----
# Dendrogram för platser
d <- dist(dat %>% select(-id, -Location))
hc <- hclust(d)
plot(hc, hang = -1, labels = dat$Location)

# Transponera datan med funktionen t() för ett dendrogram för arter
d <- dist(t(dat %>% select(-id, -Location)))
hc <- hclust(d)
plot(hc, hang = -1)

# PCA ----
pca <- prcomp(dat %>% select(-id, -Location), scale. = T)

# install.packages("factoextra")
library(factoextra) # Kör raden ovan om paket ej installerat

fviz_pca_biplot(pca, habillage = dat$Location)

# Diversitetsindex ----
dat <- dat %>% 
  mutate(Diversity_index = diversity(dat %>% select(-id, -Location),index = "shannon"))

dat %>% 
  group_by(Location) %>% 
  summarise(`Index, medelvärde` = mean(Diversity_index), `Index, stdav` = sd(Diversity_index))

dat %>% 
  filter(id != 4 | Location != "a")

ggplot(dat, aes(Location, Diversity_index)) +
  geom_point()

ggplot(dat, aes(Location, Diversity_index)) +
  geom_boxplot()

mod <- lm(Diversity_index ~ Location, dat)
anova(mod)
