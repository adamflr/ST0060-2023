# Ett stickprov av icke-normalfördelad data
## Repetition av datorövning 4

# install.packages("tidyverse")
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

## Proportioner från binär data

library(tidyverse)
dat_alls <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Allsvenskan%2C%20damer%2C%202000-2020.csv")

ggplot(dat_alls, aes(hemmamal, bortamal)) +
  geom_jitter(size = 0.1)

##### Övningsuppgift 5.1 #####
# Kör stycket nedan för en interaktiv målgraf. Vilken match gav det högsta antalet gjorda bortamål?

# install.packages(plotly)
library(plotly)
g <- ggplot(dat_alls, aes(hemmamal, bortamal, text = paste(sasong, hemma, "-", borta))) +
  geom_jitter(size = 0.1)

ggplotly(g)

##########

dat_alls %>% count(resultat)
table(dat_alls$resultat)

n <- 947 + 1803
p_est <- 947 / n

p_est
n

prop.test(x = 947, n = 2750, p = 0.33, correct = F)

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

##### Övningsuppgift 5.2 #####
# Titta på hjälpsidan med `?prop.test`. Hur genomför man ett ensidigt test? Gör lämpligt tillägg för att testa om andelen bortasegrar är större än 0.33.
##########

##### Övningsuppgift 5.3 #####
# Samma gamla bollkunskap säger att 20 procent av matcher blir oavgjorda. I datan är 518 av 2750 matcher oavgjorda. Ställ upp hypoteser och fyll i koden nedan för att testa om bollkunskapen stämmer.

prop.test(x = ___, n = ___, p = ___, correct = F)

##########

##### Övningsuppgift 5.4 #####
# Slutligen är då resten av matcherna, 1285 av 2750, hemmasegrar. Gammal bollkunskap säger: *47 procent av alla matcher är hemmasegrar*. Genomför ett z-test för att testa det påstående.

prop.test(x = ___, n = ___, p = ___, correct = F)

##########

##### Övningsuppgift 5.5 #####
# Ett hypotestest bygger på en underliggande tanke med en population (med någon för oss okänd proportion positiva utfall) och ett stickprov (i vilket vi kan observera andelen positiva utfall). Det är inte alltid uppenbart vad som egentligen är populationen. I fallet med fotbollsdatan, vad kan ses som populationen? Hur långt skulle man kunna generalisera de slutsatser man kan dra från datan?
##########

##### Övningsuppgift 5.6 #####
# (Fråga från Olsson, *Biometri*) En teori inom genetik förutsäger att tre fjärdedelar i en grupp guldfiskar ska ha genomskinliga fjäll. Observationer ger att nittio av hundra har genomskinliga fjäll. Genomför ett test med `prop.test()` för att se om den faktiska proportionen skiljer sig från 0.75. Lös gärna först uppgiften för hand eller med miniräknare.
##########

##### Övningsuppgift 5.7 #####
# (Fråga från Olsson, *Biometri*) En konkurrerande teori inom genetik förutsäger att femton sextondelar (proportionen 0.9375) ska ha genomskinliga fjäll. Observationer ger att nittio av hundra har genomskinliga fjäll. Genomför ett test med `prop.test()` för att se om proportionen skiljer sig från 0.9375. Lös gärna först uppgiften för hand eller med miniräknare.
##########

##### Övningsuppgift 5.8 #####
# I det andra guldfiskexemplet är antalet observationer 100 och nollhypotesens värde 0.9375. Är normalapproximationen *giltig* i det fallet?
##########

##### Övningsuppgift 5.9 #####
# I ett naturreservat tror man fördelningen av tre fåglar (tärnmås, fiskmås och fisktärna) är 50, 30 respektive 20 procent. En studie ger antalen 115, 54 respektive 31. Genomför tre z-test för att testa den antagna andelarna. För tärnmås får man till exempel `prop.test(115, 200, p = 0.5, correct = F)`.
##########

##### Övningsuppgift 5.10 #####
# En intressant egenskap hos proportionstest är att man redan i förväg kan beräkna vilka utfall som ger signifikanta resulat. Säg att man har möjlighet att göra 100 replikat. Ändra i stycket nedan för att hitta det högsta värde på x som ger ett *icke*-signifikant utfall.

prop.test(x, n = 100, p = 0.5, correct = F)

# Det är möjligt att göra liknande beräkningar för ett t-test för normalfördelad data, men då måste man göra antaganden om standardavvikelsens storlek.
##########

## Konfidensintervall för proportioner

n <- 947 + 1803
p <- 947 / n

p - 1.96 * sqrt(p * (1 - p) / n)
p + 1.96 * sqrt(p * (1 - p) / n)

#install.packages("binom")
library(binom)
binom.asymp(x = 947, n = 2750)

##### Övningsuppgift 5.11 #####
# Gör lämplig ändring i koden nedan för att beräkna ett 99-procentigt konfidensintervall för andelen bortasegrar.

binom.asymp(x = 947, n = 2750, conf.level = 0.95)

##########

##### Övningsuppgift 5.12 #####
# Vad händer om man försöker räkna ut ett konfidensintervall för ett perfekt utfall - t.ex. om man får 100 av 100 positiva utfall?
##########

##### Övningsuppgift 5.13 #####
# Använd funktionen `binom.asymp()` för att ta fram konfidensintervallet för andelen guldfiskar från den tidigare uppgiften. Hur förhåller sig resultatet till nollhypotesernas värden (0.75 respektive 0.9375)? Gör motsvarande beräkning med miniräknare.
##########

## Chi-två-test för goodness-of-fit

O <- c(947, 518, 1285)
E <- c(0.33,0.20,0.47) * 2750

##### Övningsuppgift 5.14 #####
# Skriv ut objektet `E` och jämför med de observerade värdena. Notera att de förväntade värdena inte måste vara heltal, trots att de observerade värdena förstås alltid kommer vara det.
##########

chisq_value <- sum((O - E)^2 / E)

dat_chisq <- data.frame(x = seq(0, 10, 0.1)) %>% 
  mutate(p = dchisq(x, df = 2))

ggplot() +
  geom_line(aes(x, p), data = dat_chisq) +
  geom_ribbon(aes(x, ymin = 0, ymax = p), data = dat_chisq %>% filter(x > chisq_value), fill = "salmon")

1 - pchisq(chisq_value, df = 2)

qchisq(0.95, df = 2)

chisq.test(O, p = c(0.33, 0.2, 0.47))

##### Övningsuppgift 5.15 #####
# Situationen med flera klasser kan som sagt ses som en generalisering av fallet med två klasser. Det är alltså logiskt att chi-två-test kan användas även när man har två klasser. Följande exempel ger samma test som vi sett tidigare av andelen bortasegrar.

chisq.test(x = c(947, 1803), p = c(0.33, 0.67), correct = F)

# Likt `prop.test()` sätter vi `correct` till `FALSE` för att inte göra en korrektion. Notera att `x` här anges som positiva och negativa utfall istället för positiva utfall och totalt antal utfall, vilket var fallet i `prop.test()`.

# Använd stycket ovan som mall för att göra uppgiften om guldfiskar som ett chi-två-test. Testa nollhypotesen att andelen positiva utfall är 0.75.
##########

chisq.test(c(6,4), p = c(0.51, 0.49))

##### Övningsuppgift 5.16 #####
# En vanlig tillämpning av goodness-of-fit-testet är för att testa om alla klasser är lika sannolika. En jämn fördelning är grundinställning i `chisq.test()` så i det fallet behöver man bara ange de observerade värdena. En datainsamling om M&M-godis gav följande antal.

dat_mnm <- data.frame(Color = c("blue", "brown", "green", "orange", "red", "yellow"),
                      Count = c(180, 80, 88, 160, 134, 166))

ggplot(dat_mnm, aes(Color, Count, fill = Color)) +
  geom_col() +
  scale_fill_manual(values = dat_mnm$Color)

# Använd de observerade värdena i kolumnen `Count` för att testa om alla godisfärger är lika vanliga.
##########

test <- chisq.test(c(947, 518, 1285), p = c(0.33, 0.2, 0.47))
test$expected
test$observed
(test$observed - test$expected)^2 / test$expected

##### Övningsuppgift 5.17 #####
# Spara testobjektet från testet på M&M-färger för att se vilka färger som avviker mest från det väntade utfallet.
##########

##### Övningsuppgift 5.18 #####
# I naturreservatet från en tidigare uppgift tror man fördelningen av tre fåglar (tärnmås, fiskmås och fisktärna) är 50, 30 respektive 20 procent. En studie ger antalen 115, 54 respektive 31. Genomför ett chi-två-test för att testa de antagna andelarna. Är resultatet i linje med de separata testen från den tidigare uppgiften?
##########

##### Övningsuppgift 5.19 #####
# Spara testobjektet från chi-två-testet för fåglarna för att se vilka fågelarter som avviker mest från det väntade utfallet.
##########

## Chi-två-test när någon parameter skattas från datan

mean_goals <- mean(dat_alls$hemmamal + dat_alls$bortamal)
mean_goals

dat_goals <- dat_alls %>% 
  count(Mål = bortamal + hemmamal, name = "O") %>% 
  mutate(p = dpois(Mål, lambda = mean_goals),
         E = p * 2750)
dat_goals

##### Övningsuppgift 5.20 #####
# Fyll i de saknade delarna i koden nedan för en graf med faktiska antal `O` som punkter och förväntade antal `E` som en linje.

ggplot(dat_goals) +
  geom_point(aes(x = Mål, y = ___)) +
  geom_line(aes(x = Mål, y = ___))

##########

dat_goals_merged <- dat_goals %>% 
  mutate(Mål = ifelse(Mål > 9, 10, Mål)) %>% 
  group_by(Mål) %>% 
  summarise(O = sum(O),
            p = sum(p),
            E = sum(E))
dat_goals_merged

chisq_value <- sum((dat_goals_merged$O - dat_goals_merged$E)^2 / dat_goals_merged$E)
1 - pchisq(chisq_value, df = 9)

## Bonus. Interaktiva kartor med leaflet

##### Övningsuppgift 5.21 #####
# Installera och ladda `leaflet genom att fylla i och köra raden nedan.

install.packages("leaflet")
library(leaflet)

##########

m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = 174.768, lat = -36.852, popup="The birthplace of R")
m

##### Övningsuppgift 5.22 #####
# Ladda ner excelfilen och läs in datan med `read_excel()` från paketet `readxl`.

library(readxl)
dat_leaf <- read_excel("___")

##########

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$Stamen.Toner)

##### Övningsuppgift 5.23 #####
# Skriv ut tillgängliga baskartor med `providers`. Välj ett alternativ slumpmässigt och ändra koden ovan för att se hur det ser ut.
##########

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = dat_leaf$lng, lat = dat_leaf$lat, radius = 10)

##### Övningsuppgift 5.24 #####
# Ändra storleken på cirklarna från `addCircleMarkers()` genom argumentet `radius`.
##########

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = dat_leaf$lng, lat = dat_leaf$lat, radius = 10, popup = dat_leaf$Rödlistade)

##### Övningsuppgift 5.25 #####
# Ändra i koden ovan för att ange artnamn som popup-text istället för rödlistestatus. Funktionen `paste()` kan också vara intressant för att ta med mer information i texten: `paste(dat_leaf$Rödlistade, dat_leaf$Antal)` skulle exempelvis ge både status och antal individer vid den specifika observationen.
##########
NA
