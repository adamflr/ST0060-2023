# Regression och korrelation
## Repetition av datorövning 7

# install.packages("tidyverse")
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

## Regression

library(gapminder)
dat_eu07 <- gapminder %>% 
  filter(year == 2007, continent == "Europe") %>% 
  mutate(gdpPercap = gdpPercap / 1000)

library(ggrepel)
ggplot(dat_eu07, aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3)

##### Övningsuppgift 8.1 #####
# Vad måste ändras i stycket nedan för att plocka ut data och göra en graf för Europa 1957?

dat_eu57 <- gapminder %>% 
  filter(year == 2007, continent == "Europe") %>% 
  mutate(gdpPercap = gdpPercap / 1000)

ggplot(dat_eu57, aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3)

##########

mod <- lm(lifeExp ~ gdpPercap, data = dat_eu07)
summary(mod)

##### Övningsuppgift 8.2 #####
# Skatta samma modell som ovan, denna gång med data från 1957. Tolka lutningsparametern i ord. Är effekten av ökad bnp större 2007 än den var 1957?
##########

ggplot(dat_eu07, aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3) +
  geom_smooth(method = lm)

##### Övningsuppgift 8.3 #####
# Använd `geom_smooth(method = lm)` för att lägga till en regressionslinje för data för 1957. Hur mycket påverkar de två avvikande länderna?
##########

library(car)
Anova(mod)

dat_eu07 <- dat_eu07 %>% 
  mutate(Residualer = residuals(mod),
         Skattade = fitted(mod))

ggplot(dat_eu07, aes(sample = Residualer)) + geom_qq() + geom_qq_line()
ggplot(dat_eu07, aes(Skattade, Residualer)) + geom_point()

##### Övningsuppgift 8.4 #####
# Gör lämpliga ändringar i data ovan för diagnosgrafer för data från 1957. Finns det några tydliga avvikande värden?
##########

##### Övningsuppgift 8.5 #####
# Låt oss titta på hela gapminder-datan för 2007.

dat_2007 <- gapminder %>% filter(year == 2007)
ggplot(dat_2007, aes(gdpPercap, lifeExp)) + geom_point()

# Hur ser sambandet mellan bnp och medellivslängd ut? Vad skulle vara problematiskt med simpel linjär regression i det här fallet? När vi tittade på normalfördelningen sa vi att man ofta kan logaritmera en variabeln och få *bättre* egenskaper. Vad ska ändras i koden ovan för att använda logaritmerad `gdpPercap` istället för den ursprungliga variabeln? Är det sambandet mer linjärt?
##########

##### Övningsuppgift 8.6 #####
# Vad ska ändras i koden nedan för att använda logaritmerad `gdpPercap` istället för den ursprungliga variabeln? Är det sambandet mer linjärt?

dat_2007 <- gapminder %>% filter(year == 2007)
ggplot(dat_2007, aes(gdpPercap, lifeExp)) + geom_point()

##########

##### Övningsuppgift 8.7 #####
# Gör lämplig ändring i stycket nedan för att läsa in fliken *Blodtrycksdata* från filen *Uppgiftsdata.xlsx*.

library(readxl)
dat_blod <- read_excel("___", sheet = "Blodtryck")

##########

##### Övningsuppgift 8.8 #####
# Gör ett spridningsdiagram med ålder på x-axeln och blodtryck på y-axeln. Lägg till en regressionslinje med `geom_smooth(method = lm)`.

ggplot(___, aes(x = ___, y = ___)) +
  ___() +
  ___()

##########

##### Övningsuppgift 8.9 #####
# Skatta och tolka en regressionmodell med ålder som förklarande variabel och blodtryck som förklarad variabel.

mod <- lm(___ ~ ___, data = dat_blod)

##########

##### Övningsuppgift 8.10 #####
# Använd `Anova()` för att testa om det finns ett signifikant samband mellan ålder och blodtryck. Vad är testets nollhypotes och alternativhypotes?
##########

##### Övningsuppgift 8.11 #####
# Ta fram diagnosgrafer för blodtrycksmodell och avgör om det finns några tydliga avvikelser från normalfördelning eller några extrema värden.

dat_blod <- dat_blod %>% 
  mutate(Residualer = residuals(mod),
         Skattade = fitted(mod))

ggplot(___, aes(sample = ___)) + geom_qq() + geom_qq_line()
ggplot(___, aes(Skattade, ___)) + geom_point()

##########

## Korrelation

cor(dat_eu07$lifeExp, dat_eu07$gdpPercap)
cor.test(dat_eu07$lifeExp, dat_eu07$gdpPercap)

##### Övningsuppgift 8.12 #####
# Om man har fler än två variabler sammanfattas korrelationer ofta med en korrelationsmatris.

dat_eu07[, 4:6]
cor(dat_eu07[, 4:6])

# Vad är korrelationen mellan befolkningsstorlek och bnp per capita?
##########

##### Övningsuppgift 8.13 #####
# Den raka regressionslinjen eller det enkla korrelationsmåttet säger lite om hur data egentligen ser ut. En vanlig illustration av detta är *Anscombes kvartett*, fyra exempel konstruerade av den brittiske statistikern Francis Anscombe 1973. Datan finns tillgänglig i R som datasetet `anscombe`.

anscombe

# Plotta de fyra graferna (`x1` paras med `y1` och så vidare) i spridningsdiagram och beräkna korrelation för varje par. Ett exempel ges för den första mängden nedan. Kommentera utfallet.

ggplot(anscombe, aes(x1, y1)) + geom_point()
cor(anscombe$x1, anscombe$y1)

##########

##### Övningsuppgift 8.14 #####
# Datasaurus-datan är en konstruerad datamängd som illustrerar hur skilda mönster i data kan ge samma punktskattningar (medelvärden, standardavvikelser och korrelationer). Datan finns tillgänglig som en del av TidyTuesday-projektet och kan hämtas med följande rad.

dat_saurus <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

# Datan innehåller en gruppering (`dataset`) och x- och y-koordinater. Beräkna medelvärden, standardavvikelser och korrelation för varje grupp i `dataset` genom att fylla i stycket nedan. 

dat_saurus %>% 
  group_by(___) %>% 
  summarise(mean(x), mean(y), sd(x), sd(y), cor(x, y))

# Kommentera utfallet.
##########

##### Övningsuppgift 8.15 #####
# Illustrera datasaurus datan med spridningsdiagram. Använd `facet_wrap()` för småfönster per `dataset`.

ggplot(dat_saurus, aes(x, y)) +
  geom_point() +
  facet_wrap(~ ___)

##########

##### Övningsuppgift 8.16 #####
# En modern förståelse av regression införs under slutet av 1800-talet av Francis Galton (1822 - 1911). I en studie från 1886 samlade Galton in data på längder hos föräldrar och barn. En av Galtons slutsatser från den datan var att barn till långa föräldrar ofta blev kortade än föräldrarna. Extremvärden hade en tendes att *återgå* mot mitten - härifrån kommer namnet *regression*.

# Galtons längddata finns tillgänglig i paketet `HistData` som `Galton`. Installera paketet, ladda paketet, och skriv ut datan.

install.packages("___")
library(___)
Galton

# Datan är i tum. Om man föredrar cm kan man multiplicera med 2.54.

Galton <- 2.54 * Galton

##########

##### Övningsuppgift 8.17 #####
# Gör en graf med föräldrars medellängd (`parent`) och barnets längd (`child`). Eftersom det finns överlappande punkter kan man använda `geom_count()` eller `geom_jitter()` istället för `geom_point()`.

ggplot(Galton, aes(parent, child)) + geom_count()
ggplot(Galton, aes(parent, child)) + geom_jitter()

##########

##### Övningsuppgift 8.18 #####
# Skatta en regressionmodell med barnets längd som förklarad variabel och förälderns längd som förklarande variabeln. Skriv ut resultaten och tolka lutningsparametern. Gör ett F-test med `Anova()`.

mod <- lm(___ ~ ___, Galton)
summary(___)
Anova(___)

##########

##### Övningsuppgift 8.19 #####
# Paketet `emmeans()`, som vi tidigare använt för att ta fram effekter i anovamodeller, har också en funktion för lutningsparametrar `emtrends()`. Vi kan använda den funktionen för att beräkna konfidensintervall för lutningen.

library(emmeans)
emtrends(mod, ~ 1, var = "parent")

# Funktionen `emmeans()` kan också användas för ett konfidensintervall för barnets längd vid ett specifikt värde för föräldrarnas längd. Följande ger ett konfidensintervall för barnets längd om föräldrarnas medellängd är 170 cm.

emmeans(mod, ~ parent, at = list(parent = 68))

# Vad ska ändras i stycket ovan för att beräkna ett konfidensintervall för barnets längd om föräldrarnas medellängd är 190 cm?
##########

##### Övningsuppgift 8.20 #####
# Galtondatan omfattar 928 mätningar. Ta ut residualerna med `residuals(mod)` och gör ett histogram med `hist()` eller `geom_histogram()`. Följer residualerna en ungefärlig normalfördelning?
##########

## Bonus. Skrapa data från webbsidor

#install.packages("osmdata")
library(osmdata)
dat_osm <- opq(bbox = 'Malmö') %>%
    add_osm_feature(key = 'admin_level', value = '10') %>%
    osmdata_sf()

dat_osm_pol <- dat_osm$osm_multipolygons

ggplot(dat_osm_pol, aes()) + 
  geom_sf() +
  geom_sf_text(aes(label = name), size = 3)

##### Övningsuppgift 8.21 #####
# Vad kan ändras i exemplet ovan för att ta ut Lunds stadsdelar i stället för Malmös?
##########

dat_osm <- opq(bbox = 'Malmö') %>%
    add_osm_feature(key = 'amenity', value = 'restaurant') %>%
    osmdata_sf()

dat_osm_point <- dat_osm$osm_points %>% 
  filter(cuisine %in% c("pizza", "sushi", "burger", "chinese", "indian", "vietnamese"))

ggplot() + 
  geom_sf(data = dat_osm_pol) +
  geom_sf(data = dat_osm_point, aes(color = cuisine), size = 2)

##### Övningsuppgift 8.22 #####
# Offentliga konstverk är ofta registrerade med `key = 'tourism'` och `value = 'artwork'`. Vad kan ändras i exemplet ovan för att ta ut offentliga konstverk i Malmö?
##########

# install.packages("rvest")
library(rvest)

url <- "https://www.criterion.com/shop/browse/list"
html <- read_html(url)

dat_crit <- html %>% 
  html_table()

dat_crit <- dat_crit[[1]] %>% 
  select(-2) %>% 
  filter(Director != "")
dat_crit

##### Övningsuppgift 8.23 #####
# Vilken regissör har flest filmer i criterion-samlingen? Använd datan från exemplet ovan och räkna antal filmer per regissör, t.ex. med `count()`.
##########

url <- "https://en.wikipedia.org/wiki/List_of_Nobel_laureates_in_Literature"
dat_nob <- url %>% 
  read_html() %>% 
  html_table()
dat_nob <- dat_nob[[1]]

##### Övningsuppgift 8.24 #####
# Skapa ett stapeldiagram över antalet vinnare per språk (kolumnen `Language(s)`) genom att fylla i stycket nedan.

dat_agg <- dat_nob %>% count(`Language(s)`)

ggplot(dat_agg, aes(x = n, y = ___)) +
  geom_col()

##########

##### Övningsuppgift 8.25 #####
# Hitta en wikipedia-artikel med en tabell och försök hämta ner den till R genom att göra lämplig ändring i exemplet ovan.
##########

