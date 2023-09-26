# # Regression och korrelation
#
# Datorövning 8 handlar om regression och korrelation. Efter övningen ska vi kunna
#
# - skatta en regressionsmodell i R,
#
# - testa parametrar i modellen med F-test och t-test,
#
# - göra lämpliga tester av modellantaganden,
#
# - beräkna och tolka korrelationen mellan två variabler.
#
# ## Repetition av datorövning 7
#
# När man startar en ny R-session bör man ladda de paket man vet kommer behövas med `library()`. Om
# paket inte finns installerade måste man först köra `install.packages()`.
#

# install.packages("tidyverse")
library(tidyverse)

#
# I datorövning 7 tittade vi på variansanalys - en metod som gör det möjligt att utvidga t-testet
# för två grupper till ett godtyckligt antal grupper eller kombinationer av faktorer. I variansanalys
# skattar man en modell som förklarar ett datautfall. Utifrån modellen sätter man upp en anova-tabell
# som delar upp den totala variansen i en förklarad del och en kvarvarande residualdel. Anova-tabell
# ger också ett F-test som testar om det finns några skillnader mellan grupper. Från en skattad
# modell kan man sedan göra parvisa jämförelser mellan specifika grupper och testa modellantaganden
# (främst antagande om normalfördelning och lika varians inom grupper).
#
# Ta som exempel följande data på tandtillväxt (`len`) hos marsvin under C-vitaminbehandling i
# olika doser (`dose`) och två olika metoder (`supp`), tillgängligt i R som objektet `ToothGrowth`.
#

ToothGrowth <- ToothGrowth %>% mutate(dose = as.character(dose))

ggplot(ToothGrowth, aes(len, supp, fill = dose)) + 
  geom_boxplot()

#
# Ett lådagram visar en klar skillnad mellan doser och en svagare skillnad mellan metoder. Det
# finns också tecken på att metoderna svarar olika på dos i att metoden *VC* ligger lägre än *OJ* vid
# de låga doserna men över (eller iallafall lika) vid den höga dosen.
#
# En envägsanova-modell (en modell med en faktor) kan skattas med `lm()` och en anovatabell kan tas
# fram med `Anova()` från paketet `car`.
#

mod <- lm(len ~ dose, data = ToothGrowth)

library(car)
Anova(mod)

#
# F-testets nollhypotes är att alla grupper (här alla doser) har samma populationsmedelvärde. Det
# låga p-värdet pekar på en klar skillnad mellan doser.
#
# En anovamodell bygger på antaganden om normalfördelning och lika varianser.
# Normalfördelningsantagandet kan undersökas med en QQ-graf över residualerna och variansantagandet
# kan undersökas med en spridningsgraf över skattade värden och residualer.
#

library(patchwork)
ToothGrowth <- ToothGrowth %>% 
  mutate(Skattade = fitted(mod),
         Residualer = residuals(mod))
g1 <- ggplot(ToothGrowth, aes(sample = Residualer)) + geom_qq() + geom_qq_line()
g2 <- ggplot(ToothGrowth, aes(Skattade, Residualer)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red")
g1 + g2

#
# Punkterna ligger ungefär på linjen i QQ-grafen och punkterna har ungefär samma spridning för alla
# nivåer av det skattade värdet.
#
# Anova-modeller kan lätt byggas ut genom att lägga till fler faktorer. Här är det till exempel
# naturligt att skatta en modell med både metod och dos, vilket kan göras genom att lägga till `supp`
# till formeln i `lm()`.
#

mod <- lm(len ~ dose * supp, ToothGrowth)
Anova(mod)

#
# Resultaten är i linje med grafen - dos har en stor effekt medan metod och interaktionen mellan
# dos och metod är något svagare, om än signifikanta.
#
# En anovamodell kan användas för parvisa jämförelse, vilket ibland kallas post-hoc-test. Den
# vanligaste är Tukey-testet, men andra tester kan också förekomma. Testet kan utföras med
# `emmeans()` från paketet med samma namn. Följande ger en jämförelse mellan doser uppdelat efter
# metod.
#

library(emmeans)
emmeans(mod, pairwise ~ dose | supp)

#
# ## Regression
#
# I en regression modelleras en numerisk variabel som en funktion av en annan numerisk variabel.
# Vid enkel linjär regression finns *en* sådan *förklarande variabel* och förhållandet mellan
# variablerna antas vara linjärt.
#
# Ta som exempel data på förväntad medellivslängd och bnp per capita. Datan hämtas från
# `gapminder`-paketet. Paketet `ggrepel` och funktionen `geom_text_repel()` kan användas för att
# sätta punktetiketter som inte överlappar. För enklare tolkning av modellen transformeras bnp per
# capita till att vara i tusen dollar, snarare än dollar.
#

library(gapminder)
dat_eu07 <- gapminder %>% 
  filter(year == 2007, continent == "Europe") %>% 
  mutate(gdpPercap = gdpPercap / 1000)

library(ggrepel)
ggplot(dat_eu07, aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3)

#
# Datan visar ett positivt samband mellan variablerna - högre bnp per capita är kopplat till högre
# medellivslängd.
#
# Uppgift 8.1. (Data för 1957)
# Vad måste ändras i stycket nedan för att plocka ut data och göra en graf för Europa 1957?
#

dat_eu57 <- gapminder %>% 
  filter(year == 2007, continent == "Europe") %>% 
  mutate(gdpPercap = gdpPercap / 1000)

ggplot(dat_eu57, aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3)

# :::
#
# En regressionmodell kan i R skattas med `lm`-funktionen. Syntaxen är väldigt lik den för
# anovamodellen, men istället för en faktor som förklarande variabel används nu en kontinuerlig
# variabel.
#

mod <- lm(lifeExp ~ gdpPercap, data = dat_eu07)
summary(mod)

#
# Funktionen `summary` ger en sammanfattning av modellen. Skattningen av modellens konstanta
# parameter ges som raden `(Intercept)` och dess tolkning är som förväntat värde i medellivslängd om
# bnp per capita är noll. Det är ofta lutningsparametern som är mer intressant. Skattningen av
# lutningsparametern ges på den rad som har samma namn som den förklarande variabeln, här
# `gdpPercap`. Den skattade parametern är 0.2146. Lutningsparametern har den generella tolkning som
# ökningen i y-variabeln när x-variabeln ökar med 1. I det här fallet ger 0.2146 att ett lands
# medellivslängd ökar med ungefär 0.2146 år (eller 78 dagar) när bnp per capita ökar med 1000 dollar.
#
# Uppgift 8.2. (Modell för 1957)
# Skatta samma modell som ovan, denna gång med data från 1957. Tolka lutningsparametern i ord. Är
# effekten av ökad bnp större 2007 än den var 1957?
# :::
#
# Man kan enkelt rita ut regressionlinjen i en graf med `geom_smooth()` och argumentet `method`
# satt till `lm`.
#

ggplot(dat_eu07, aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3) +
  geom_smooth(method = lm)

#
# Den blå linjen illustrerar regressionlinjen 72.27 + 0.2146x. Det grå bandet kring linjen är ett
# konfidensintervall för skattningen av y-variabeln.
#
# Uppgift 8.3. (Graf för 1957)
# Använd `geom_smooth(method = lm)` för att lägga till en regressionslinje för data för 1957. Hur
# mycket påverkar de två avvikande länderna?
# :::
#
# Utskriften från `summary` ger också tester av parametrarna (den högra kolumnen `Pr(>|t|)` ger
# p-värdet för ett test där nollhypotesen är att populationsparametern är noll). I det här fallet är
# både intercept och lutning skilda från noll. Motsvarande F-test för lutningen kan tas fram med en
# anova-tabell.
#

library(car)
Anova(mod)

#
# Testerna av en regressionsmodell bygger på ett normalfördelningsantagande och ett antagande om
# *homoskedasticitet* (lika varians i y oavsett position på x-axeln). Antagandena kan undersökas
# genom att titta på skattningens *residualer* - skillnaden mellan det faktiska y-värdet och
# modellens värde. Residualerna kan undersökas med ett histogram eller en QQ-plot. En annan vanlig
# diagnosplot är ett spridningsdiagram med skattade värden på x-axeln och residualerna på y-axeln.
#

dat_eu07 <- dat_eu07 %>% 
  mutate(Residualer = residuals(mod),
         Skattade = fitted(mod))

ggplot(dat_eu07, aes(sample = Residualer)) + geom_qq() + geom_qq_line()
ggplot(dat_eu07, aes(Skattade, Residualer)) + geom_point()

#
# Om data följer en normalfördelning bör histogrammet visa en ungefärlig normalkurva, QQ-plotten
# bör visa punkter på den diagonala linjen och spridningsdiagrammet bör visa en slumpmässig spridning
# av punkter. Graferna pekar i det här fallet inte på några tydliga avvikelser från
# normalfördelningsantagandet, möjligen pekar QQ-plotten på mindre spridning i svansarna än en
# teoretisk normalfördelning.
#
# Uppgift 8.4. (Diagnos för 1957)
# Gör lämpliga ändringar i data ovan för diagnosgrafer för data från 1957. Finns det några tydliga
# avvikande värden?
# :::
#
# Uppgift 8.5. (Icke-linjära samband)
# Låt oss titta på hela gapminder-datan för 2007.
#

dat_2007 <- gapminder %>% filter(year == 2007)
ggplot(dat_2007, aes(gdpPercap, lifeExp)) + geom_point()

#
# Hur ser sambandet mellan bnp och medellivslängd ut? Vad skulle vara problematiskt med simpel
# linjär regression i det här fallet? När vi tittade på normalfördelningen sa vi att man ofta kan
# logaritmera en variabeln och få *bättre* egenskaper. Vad ska ändras i koden ovan för att använda
# logaritmerad `gdpPercap` istället för den ursprungliga variabeln? Är det sambandet mer linjärt?
# :::
#
# Uppgift 8.6. (Log-transformerad data)
# Vad ska ändras i koden nedan för att använda logaritmerad `gdpPercap` istället för den
# ursprungliga variabeln? Är det sambandet mer linjärt?
#

dat_2007 <- gapminder %>% filter(year == 2007)
ggplot(dat_2007, aes(gdpPercap, lifeExp)) + geom_point()

# :::
#
# Uppgift 8.7. (Blodtrycksdata)
# Gör lämplig ändring i stycket nedan för att läsa in fliken *Blodtrycksdata* från filen
# *Uppgiftsdata.xlsx*.
#

library(readxl)
dat_blod <- read_excel("___", sheet = "Blodtryck")

# :::
#
# Uppgift 8.8. (Blodtrycksgraf)
# Gör ett spridningsdiagram med ålder på x-axeln och blodtryck på y-axeln. Lägg till en
# regressionslinje med `geom_smooth(method = lm)`.
#

ggplot(___, aes(x = ___, y = ___)) +
  ___() +
  ___()

# :::
#
# Uppgift 8.9. (Blodtrycksmodell)
# Skatta och tolka en regressionmodell med ålder som förklarande variabel och blodtryck som
# förklarad variabel.
#

mod <- lm(___ ~ ___, data = dat_blod)

# :::
#
# Uppgift 8.10. (Blodtryckstest)
# Använd `Anova()` för att testa om det finns ett signifikant samband mellan ålder och blodtryck.
# Vad är testets nollhypotes och alternativhypotes?
# :::
#
# Uppgift 8.11. (Blodtrycksdiagnos)
# Ta fram diagnosgrafer för blodtrycksmodell och avgör om det finns några tydliga avvikelser från
# normalfördelning eller några extrema värden.
#

dat_blod <- dat_blod %>% 
  mutate(Residualer = residuals(mod),
         Skattade = fitted(mod))

ggplot(___, aes(sample = ___)) + geom_qq() + geom_qq_line()
ggplot(___, aes(Skattade, ___)) + geom_point()

# :::
#
# ## Korrelation
#
# Korrelation ger ett mått mellan $-1$ och $1$ på hur väl två variabler samvarierar. En korrelation
# över noll tyder på ett positivt samband mellan variablerna - en observation med ett högt värde i
# den ena variabeln har också ett högt värde på den andra - medan en korrelation under noll tyder på
# ett negativt samband. I R kan korrelation beräknas med `cor()` och två variabler som första och
# andra argument. Funktionen `cor.test()` ger ett test där nollhypotesen är att korrelationen är
# noll.
#

cor(dat_eu07$lifeExp, dat_eu07$gdpPercap)
cor.test(dat_eu07$lifeExp, dat_eu07$gdpPercap)

#
# Medellivslängd och bnp per capita har en stark positiv korrelation på 0.85 och den korrelation är
# signifikant skild från noll (p < 0.001). Notera att p-värdet är detsamma som för lutningsparametern
# i regressionen.
#
# Uppgift 8.12. (Korrelationsmatris)
# Om man har fler än två variabler sammanfattas korrelationer ofta med en korrelationsmatris.
#

dat_eu07[, 4:6]
cor(dat_eu07[, 4:6])

#
# Vad är korrelationen mellan befolkningsstorlek och bnp per capita?
# :::
#
# Uppgift 8.13. (Anscombes data)
# Den raka regressionslinjen eller det enkla korrelationsmåttet säger lite om hur data egentligen
# ser ut. En vanlig illustration av detta är *Anscombes kvartett*, fyra exempel konstruerade av den
# brittiske statistikern Francis Anscombe 1973. Datan finns tillgänglig i R som datasetet `anscombe`.
#

anscombe

#
# Plotta de fyra graferna (`x1` paras med `y1` och så vidare) i spridningsdiagram och beräkna
# korrelation för varje par. Ett exempel ges för den första mängden nedan. Kommentera utfallet.
#

ggplot(anscombe, aes(x1, y1)) + geom_point()
cor(anscombe$x1, anscombe$y1)

# :::
#
# Uppgift 8.14. (Datasaurus Dozen. Beskrivande mått)
# Datasaurus-datan är en konstruerad datamängd som illustrerar hur skilda mönster i data kan ge
# samma punktskattningar (medelvärden, standardavvikelser och korrelationer). Datan finns tillgänglig
# som en del av TidyTuesday-projektet och kan hämtas med följande rad.
#

dat_saurus <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

#
# Datan innehåller en gruppering (`dataset`) och x- och y-koordinater. Beräkna medelvärden,
# standardavvikelser och korrelation för varje grupp i `dataset` genom att fylla i stycket nedan.
#

dat_saurus %>% 
  group_by(___) %>% 
  summarise(mean(x), mean(y), sd(x), sd(y), cor(x, y))

#
# Kommentera utfallet.
# :::
#
# Uppgift 8.15. (Datasaurus Dozen. Grafer)
# Illustrera datasaurus datan med spridningsdiagram. Använd `facet_wrap()` för småfönster per
# `dataset`.
#

ggplot(dat_saurus, aes(x, y)) +
  geom_point() +
  facet_wrap(~ ___)

# :::
#
# Uppgift 8.16. (Galtons längdstudier. Installation av paket)
# En modern förståelse av regression införs under slutet av 1800-talet av Francis Galton (1822 -
# 1911). I en studie från 1886 samlade Galton in data på längder hos föräldrar och barn. En av
# Galtons slutsatser från den datan var att barn till långa föräldrar ofta blev kortade än
# föräldrarna. Extremvärden hade en tendes att *återgå* mot mitten - härifrån kommer namnet
# *regression*.
#
# Galtons längddata finns tillgänglig i paketet `HistData` som `Galton`. Installera paketet, ladda
# paketet, och skriv ut datan.
#

install.packages("___")
library(___)
Galton

#
# Datan är i tum. Om man föredrar cm kan man multiplicera med 2.54.
#

Galton <- 2.54 * Galton

#
# :::
#
# Uppgift 8.17. (Galtons längdstudier. Graf)
# Gör en graf med föräldrars medellängd (`parent`) och barnets längd (`child`). Eftersom det finns
# överlappande punkter kan man använda `geom_count()` eller `geom_jitter()` istället för
# `geom_point()`.
#

ggplot(Galton, aes(parent, child)) + geom_count()
ggplot(Galton, aes(parent, child)) + geom_jitter()

# :::
#
# Uppgift 8.18. (Galtons längdstudier. Modell)
# Skatta en regressionmodell med barnets längd som förklarad variabel och förälderns längd som
# förklarande variabeln. Skriv ut resultaten och tolka lutningsparametern. Gör ett F-test med
# `Anova()`.
#

mod <- lm(___ ~ ___, Galton)
summary(___)
Anova(___)

# :::
#
# Uppgift 8.19. (Galtons längdstudier. Konfidensintervall)
# Paketet `emmeans()`, som vi tidigare använt för att ta fram effekter i anovamodeller, har också
# en funktion för lutningsparametrar `emtrends()`. Vi kan använda den funktionen för att beräkna
# konfidensintervall för lutningen.
#

library(emmeans)
emtrends(mod, ~ 1, var = "parent")

#
# Funktionen `emmeans()` kan också användas för ett konfidensintervall för barnets längd vid ett
# specifikt värde för föräldrarnas längd. Följande ger ett konfidensintervall för barnets längd om
# föräldrarnas medellängd är 170 cm.
#

emmeans(mod, ~ parent, at = list(parent = 68))

#
# Vad ska ändras i stycket ovan för att beräkna ett konfidensintervall för barnets längd om
# föräldrarnas medellängd är 190 cm?
# :::
#
# Uppgift 8.20. (Galtons längdstudier. Diagnosgrafer)
# Galtondatan omfattar 928 mätningar. Ta ut residualerna med `residuals(mod)` och gör ett histogram
# med `hist()` eller `geom_histogram()`. Följer residualerna en ungefärlig normalfördelning?
# :::
#
# ## Bonus. Skrapa data från webbsidor
#
# Det är väldigt vanligt att hämta in data från externa källor för att bygga ut en statistisk
# analys, till exempel kan offentlig väderdata vara intressant för ett odlingsförsök. Den typen av
# data kan vara mer eller mindre lättillgänglig. Här tittar vi på några exempel på hur allmänt
# tillgänglig data kan hämtas och användas.
#
# Kommunikation mellan datorer sker genom ett API (*Application Programming Interface*). Många
# organisationer som sprider data har ett öppet tillgängligt API som användare kan koppla upp sig
# till. Ofta finns R-paket som gör det enkelt att ange vilket data man är ute efter. Några exempel är
#
# - `pxweb` - statistiska centralbyråns web-API,
# https://cran.r-project.org/web/packages/pxweb/vignettes/pxweb.html,
# - `Eurostat` - europeiska statistikbyrån,
# https://ropengov.github.io/eurostat/articles/eurostat_tutorial.html,
# - `Rspotify` - Spotifys API, https://github.com/tiagomendesdantas/Rspotify.
#
# I följande exempel används paketet `osmdata` för att hämta data från OpenStreetMap,
# https://www.openstreetmap.org/.
#

#install.packages("osmdata")
library(osmdata)
dat_osm <- opq(bbox = 'Malmö') %>%
    add_osm_feature(key = 'admin_level', value = '10') %>%
    osmdata_sf()

dat_osm_pol <- dat_osm$osm_multipolygons

ggplot(dat_osm_pol, aes()) + 
  geom_sf() +
  geom_sf_text(aes(label = name), size = 3)

#
# Uppgift 8.21. (Malmös stadsdelar)
# Vad kan ändras i exemplet ovan för att ta ut Lunds stadsdelar i stället för Malmös?
# :::
#
# Ännu ett exempel. Denna gång Malmös restauranger efter typ.
#

dat_osm <- opq(bbox = 'Malmö') %>%
    add_osm_feature(key = 'amenity', value = 'restaurant') %>%
    osmdata_sf()

dat_osm_point <- dat_osm$osm_points %>% 
  filter(cuisine %in% c("pizza", "sushi", "burger", "chinese", "indian", "vietnamese"))

ggplot() + 
  geom_sf(data = dat_osm_pol) +
  geom_sf(data = dat_osm_point, aes(color = cuisine), size = 2)

#
# Uppgift 8.22. (Offentlig konst)
# Offentliga konstverk är ofta registrerade med `key = 'tourism'` och `value = 'artwork'`. Vad kan
# ändras i exemplet ovan för att ta ut offentliga konstverk i Malmö?
# :::
#
# Det är inte alltid data finns tillgängligt genom en API. Mycket information finns publicerad som
# text eller tabeller på vanliga hemsidor. I såna fall kan man ofta ta hem data genom webbskrapning -
# att man med ett skript hämtar hem hemsidan, snarare än att själv läsa genom en webbläsare. I R kan
# det göras med paketet `rvest`. Ta som exempel den här tabellen över filmer i criterion-samlingen:
# https://www.criterion.com/shop/browse/list. För att läsa in den listan i R kan vi göra följande.
#

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

#
# Uppgift 8.23. (Regissör)
# Vilken regissör har flest filmer i criterion-samlingen? Använd datan från exemplet ovan och räkna
# antal filmer per regissör, t.ex. med `count()`.
# :::
#
# Det finns flera paket som kan hämta data från Wikipedia, men det kan också göras med `rvest`. Här
# hämtas en tabell över mottagare av Nobelpriset i litteratur.
#

url <- "https://en.wikipedia.org/wiki/List_of_Nobel_laureates_in_Literature"
dat_nob <- url %>% 
  read_html() %>% 
  html_table()
dat_nob <- dat_nob[[1]]

#
# Uppgift 8.24. (Skrivspråk)
# Skapa ett stapeldiagram över antalet vinnare per språk (kolumnen `Language(s)`) genom att fylla i
# stycket nedan.
#

dat_agg <- dat_nob %>% count(`Language(s)`)

ggplot(dat_agg, aes(x = n, y = ___)) +
  geom_col()

# :::
#
# Uppgift 8.25. (Valfri tabell)
# Hitta en wikipedia-artikel med en tabell och försök hämta ner den till R genom att göra lämplig
# ändring i exemplet ovan.
# :::
#
