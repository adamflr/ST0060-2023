# Variansanalys
## Repetition av datorövning 6

# install.packages("tidyverse")
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

# Alternativt
# t.test(dat_fish$Species1 - dat_fish$Species2)

t.test(Species1 ~ Region, data = dat_fish, var.equal = T)

dat_parti <- data.frame(Kommun = c("Malmö", "Lund", "Kävlinge"),
                        S = c(54, 102, 40),
                        M = c(30, 98, 53),
                        MP = c(7, 50, 5))
dat_parti

chisq.test(dat_parti[, -1])

## Allmänt
## Variansanalys. En faktor

PlantGrowth

ggplot(PlantGrowth, aes(group, weight)) +
  geom_point()

mod <- lm(weight ~ group, data = PlantGrowth)

library(car)
Anova(mod)

##### Övningsuppgift 7.1 #####
# Anovatabell från `Anova()` ger kvadratsummor och frihetsgrader. Använd den informationen för att, för hand, beräkna medelkvadratsummor och F-värdet.
##########

##### Övningsuppgift 7.2 #####
# Anova-tabellen ger ett p-värde från vilket vi kan dra en direkt slutsats. Om man istället löser uppgiften för hand ställer man det beräknade F-värdet mot ett kritiskt värde från en tabell över F-fördelningen. Se efter om man kan hitta ett lämpligt tabellvärde för det aktuella testet (med 2 och 27 frihetsgrader). Det är möjligt att det inte finns en rad för 27 i en vanlig F-fördelningstabell, använd isåfall värdet på närmast övre rad (t.ex. 26 eller 25). I R kan kvantiler för F-fördelningen tas fram med `qf()`, t.ex.

qf(0.95, 2, 27)

##########

# install.packages("emmeans")
library(emmeans)
emmeans(mod, pairwise ~ group)

emmeans(mod, pairwise ~ group, adjust = "none")

em <- emmeans(mod, pairwise ~ group)

library(multcomp)
cld(em, Letters = letters)

##### Övningsuppgift 7.3 #####
# Följande kod skapar en datamängd med två behandlingar.

dat_two <- PlantGrowth %>% filter(group %in% c("trt1", "trt2"))

# Använd den datan för att göra ett t-test för två oberoende stickprov med lika varians, ett t-test för två oberoende stickprov utan antagande om lika varians, och ett F-test (ofullständig exempelkod nedan). Vad kan sägas om p-värdena från de tre testen?

t.test(___ ~ group, data = dat_two, var.equal = T)
t.test(weight ~ ___, data = dat_two, var.equal = F)

mod <- lm(weight ~ group, data = ___)
Anova(mod)

##########

##### Övningsuppgift 7.4 #####
# Anledning till att vi justerar p-värden är att man vid varje test har en sannolikhet att förkasta. Om man gör ett stort antal tester är man nästan garanterad att få något (falskt) signifikant resultat. Justering höjer p-värdena för att minska den risken. Följande kod simulerar data med 5 grupper och producerar de parvisa jämförelserna.

n_groups <- 5
dat_sim <- expand_grid(obs = 1:10, group = letters[1:n_groups]) %>% mutate(y = rnorm(n()))
mod <- lm(y ~ group, dat_sim)
emmeans(mod, pairwise ~ group, adjust = "none")

# Kör koden tio gånger. Hur många gånger av de tio ger de parvisa jämförelserna *någon* signifikant skillnad (det vill säga något p-värde under 0.05)?

# En passande xkcd-serie: https://xkcd.com/882/
##########

##### Övningsuppgift 7.5 #####
# En studie har givit ett mått på infektion hos äppelträd. Fyra sorter jämförs med tre replikat per sort. Data finns i fliken *Äppelangrepp* i excelfilen *Uppgiftsdata.xslx* på canvassidan. Fyll i kodstycket nedan för att importera datan.

library(readxl)
dat_apple <- read_excel("___", sheet = "Äppelangrepp")
dat_apple

# dat_apple <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Uppgiftsdata/Uppgift_%C3%84ppelangrepp.csv") # Alternativ lösning

##########

##### Övningsuppgift 7.6 #####
# Fyll i kodstycket nedan för att skapa en graf av äppeldatan.

ggplot(___, aes(x = ___, y = ___)) +
  geom_point()

##########

##### Övningsuppgift 7.7 #####
# Fyll i kodstycket nedan för att skatta en anovamodell och ta fram anovatabellen. Vad är F-testets noll- och alternativhypotes? Vilken slutsats kan man dra från testet?

mod <- lm(___ ~ ___, data = dat_apple)
Anova(mod)

##########

## Variansanalys. En faktor med block

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

##### Övningsuppgift 7.8 #####
# Det minsta möjliga blocket är det med två behandlingar. Vi filtrerar havredatan för att den situationen.

dat_small_block <- oats %>% filter(V == "Marvellous", N %in% c("0.6cwt", "0.0cwt"))
dat_small_block

# Fyll i stycket nedan för att skapa en graf med `N` på x-axeln, `Y` på y-axeln och en gruppering som länkar observationer från samma block.

ggplot(dat_small_block, aes(x = ___, y = ___, group = ___)) +
  geom_point() +
  geom_line()

##########

##### Övningsuppgift 7.9 #####
# Eftersom det är ett försök med en förklarande faktor och block kan man modellera det med den tidigare blockmodellen. Men eftersom man bara har två observationer per block kan man också se det som matchade stickprov, vilket kan lösas med ett t-test. Fyll i stycket nedan för att göra de två testen - utfallsvariabeln är skörd `Y` och den förklarande faktorn är kvävenivån `N`. Jämför resultaten.

mod <- lm(___ ~ ___ + B, data = dat_small_block)
Anova(mod)

t.test(___ ~ ___, data = dat_small_block, paired = ___)

##########

##### Övningsuppgift 7.10 #####
# I fliken *Majshybrider* i excelfilen *Uppgiftsdata.xlsx* finns data på fyra majssorter, vardera sorterad på fem platser (som agerar som block). Importera datan med funktionen `read_excel()` genom att fylla i kodstycket nedan.

dat_corn <- read_excel("", sheet = ___)

##########

##### Övningsuppgift 7.11 #####
# Skapa en lämplig graf av datan på majshybrider. Grafen ska illustrera både jämförelsen mellan hybrider och jämförelsen mellan platser. Se exemplet ovan som guide.
##########

##### Övningsuppgift 7.12 #####
# Fyll i koden nedan för att skatta en anova-modell med block för datan på majshybrider. Ta fram anovatabellen med `Anova()`. Vilka slutsatser kan man dra från anovatabellen?

mod <- lm(___ ~ ___ + Plats, data = dat_corn)
Anova(mod)

##########

##### Övningsuppgift 7.13 #####
# Gör lämplig ändring i koden nedan för att jämföra hybrider, istället för platser.

emmeans(mod, pairwise ~ Plats)

##########

## Variansanalys. Två faktorer med block

ggplot(oats, aes(N, Y, color = B)) +
  geom_point(size = 4) +
  facet_wrap(~ V)

mod_two_fact <- lm(Y ~ N * V + B, data = oats)

Anova(mod_two_fact)

emmeans(mod_two_fact, ~ N)
emmeans(mod_two_fact, ~ N + V)
emmeans(mod_two_fact, ~ N | V)

cld(emmeans(mod_two_fact, ~ N | V), Letters = letters)

##### Övningsuppgift 7.14 #####
# Gör lämplig ändring i koden ovan för att jämföra sorter *inom* kvävenivå. Finns det några signifikanta skillnader?
##########

##### Övningsuppgift 7.15 #####
# I modellen ovan är block en *additiv* faktor - den ingår inte i någon interaktionseffekt. Vad händer med testerna om man skattar modellen där samtliga interaktioner tas med? Varför?

mod_two_fact <- lm(Y ~ N * V * B, data = oats)

##########

## Modellantaganden och residualer

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

##### Övningsuppgift 7.16 #####
# Fliken *Bakterier* i filen *Uppgiftsdata.xlsx* innehåller data om tillväxt hos gräs efter inokulering av bakterier. Ladda ner filen och importera datan genom att fylla i koden nedan.

dat_bact <- read_excel("___", sheet = "Bakterier")

##########

##### Övningsuppgift 7.17 #####
# Illustrera datan med en lämplig graf, till exempel ett spridningsdiagram med `Inoculation` på x-axeln, `Dry weight` på y-axeln, småfönster efter `Cultivar` och färg efter `Block`.

ggplot(dat_bact, aes(x = ___, y = `___`, color = Block)) +
  geom_point(size = 6) +
  facet_wrap(~ ___)

# Hur blev färgerna för blocket? Om de inte blev distinkta färger kan variabeln `Block` ha blivit inläst som numerisk. Transformera variabeln med `as.character()` och gör om grafen. Ändras färgerna?

dat_bact <- dat_bact %>% 
  mutate(Block = as.character(Block))

##########

##### Övningsuppgift 7.18 #####
# Bakteriedatan har två faktorer och en blockfaktor. Skatta en anova-modell med interaktion och block genom att fylla i stycket nedan. Ta fram anovatabell och dra en slutsats från F-testen. Ligger slutsatsen i linje med grafen?

mod <- lm(`___` ~ ___ * ___ + ___, data = dat_bact)
Anova(mod)

##########

##### Övningsuppgift 7.19 #####
# Använd `emmeans()` för parvisa jämförelser mellan inokuleringsmetoder. Vilka par är signifikant åtskilda?

emmeans(mod, pairwise ~ ___)

##########

##### Övningsuppgift 7.20 #####
# Vi använder den skattade modellen för att ta fram skattade värden och residualer.

dat_bact <- dat_bact %>% 
  mutate(Residualer = residuals(mod),
         Skattade = fitted(mod))

# Använd exemplet på residualtester ovan för att undersöka antagandet om normalfördelade residualer.
##########

## Bonus. Statistik för ekologi

# install.packages("vegan")
library(vegan)

# install.packages("factoextra")
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

##### Övningsuppgift 7.21 #####
# Vad måste läggas till i stycket nedan för göra ett lådagram (med art på y-axeln och abundans på x-axeln) och ett stapeldiagram (med platstyp på x-axeln och abundans på y-axeln)?

dune_long
ggplot(dune_long, aes(x = ___, y = ___, fill = Type)) +
  geom_boxplot()

ggplot(dune_long, aes(x = ___, y = ___, fill = Type)) +
  geom_col() +
  facet_wrap(~ ___, nrow = 2)

##########

dune_data <- dune %>% select(-Site, -Type)
d <- dist(dune_data, method = "euclidean")
hc <- hclust(d)
plot(hc, hang = -1, labels = dune$Type, 
     axes = F, xlab = "", ylab = "", ann = F)

##### Övningsuppgift 7.22 #####
# Ta upp hjälpsidan till distansfunktionen med `?dist`. Under `method` finns flera möjliga avståndsmått. Vad måste ändras i kodstycket ovan för att ange ett Manhattan-avstånd? Har avståndet någon betydande effekt på träddiagrammet?
##########

dune_data <- t(dune_data)
d <- dist(dune_data, method = "euclidean")
hc <- hclust(d)
plot(hc, hang = -1,
     axes = F, xlab = "", ylab = "", ann = F)

dune_data <- dune %>% select(-Site, -Type)
pca <- prcomp(dune_data, scale. = F)
fviz_pca_biplot(pca, geom.ind = "point", habillage = dune$Type, labelsize = 3)

##### Övningsuppgift 7.23 #####
# En PCA kan göras med och utan att skala variablerna. Om variablerna skalas får en variabel som varierar mycket samma vikt som en variabel som varierar lite. Det kan vara bra om man har variabler som är mätta på olika sätt, till exempel om en variabel är i meter och en är i centimeter. Gör lämplig ändring i kodstycket ovan för att skala variablerna i `prcomp()`. Har det någon effekt på grafen?
##########

-(0.3 * log(0.3) + 0.5 * log(0.5) + 0.2 * log(0.2))

diver <- diversity(dune_data, index = "shannon")
dune <- dune %>% mutate(Diversity = diver)

ggplot(dune, aes(Diversity, Type)) + geom_point()

mod <- lm(Diversity ~ Type, data = dune)
Anova(mod)
emmeans(mod, ~ Type)

##### Övningsuppgift 7.24 #####
# Ta upp hjälpsidan till funktionen `diversity()`. Hur anger man att funktionen ska ge Simpsons index? 
##########

##### Övningsuppgift 7.25 #####
# Gör om analysen på diversitet (anovamodellen och F-testet) med Simpsons index istället för Shannon-Weaver. Påverkar valet av diversitetsindex utfallet av testet?
##########
NA
